
-- Copyright (C) 2013  Shou

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


-- XXX
--  - Associate Connection with a POSIXTime instead of nick

-- FIXME
--  - Warning send: invalid argument (Bad file descriptor)

-- TODO
--  - Channels
--      - Not limited to one IRC channel wtf????
--      - In progress...
--      - State monad with channel specific data
--          - Users in channel
--              - Is that it? Is it really necessary then? Just filter...
--  - Banning
--      - User data
--          - Banned POSIXTime
--          - How will it affect the structure of the software?
--  - IRC
--      - Track online users?
--      - Relay messages


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- {{{ Imports

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Error
import           Control.Exception (SomeException)
import qualified Control.Exception      as E
import           Control.Monad
import           Control.Monad.State

import           Data.Attoparsec.Text
import           Data.CaseInsensitive   (CI)
import qualified Data.CaseInsensitive   as CI
import           Data.Char
import           Data.List
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Time.Clock.POSIX

import           Network
import           Network.WebSockets

import           System.Environment
import           System.IO
import           System.Timeout

import           Text.JSON

import           Prelude                as P

-- }}}

-- {{{ Data

-- XXX conns moved to User
data Memory = Memory { users :: Users
                     , config :: Config
                     , msgs :: [Msg]
                     }

instance Monoid Memory where
    mempty = Memory mempty mempty mempty
    mappend (Memory x0 y0 z0) (Memory x1 y1 z1) =
        Memory (x0 <> x1) (y0 <> y1) (z0 <> z1)

data Cmd = Post { postChan :: Key, postMsg :: Msg }
         | Set { setNick :: Nick, setKey :: Key, setVal :: Text }
         | Opt { optChan :: Key, optKey :: Key }
         | Opts { optsChan :: Key }
         | Join { joinNick :: Nick, joinChan :: Key }
         | Part { partNick :: Nick, partChan :: Key }
         | Req { reqNick :: Nick, reqChan :: Key, reqTime :: POSIXTime }
         | List { listChan :: Key }
         | Ban { banNick :: Nick, banTime :: POSIXTime, banVictim :: Nick }
         | End { endNick :: Nick }
         deriving (Show)

data Msg = Msg { msgNick :: Nick
               , msgTime :: POSIXTime
               , msgText :: Text
               }

data CmdWrap = CmdWrap { wrapTime :: POSIXTime
                       , wrapConn :: Connection
                       , wrapCmd :: Cmd
                       }

data User = User { userNick :: Nick
                 , userStat :: Status
                 , userConns :: Map POSIXTime Connection
                 , userChans :: [Key]
                 }

data Status = Banned POSIXTime
            | Offline
            | Online
            | Mod
            | Admin
            deriving (Show)

instance Show Msg where
    show (Msg ni ts mg) = encodeStrict . makeObj $
        [ ("nick", showJSON . T.unpack $ CI.original ni)
        , ("time", showJSON (realToFrac ts :: Double))
        , ("msg", showJSON $ T.unpack mg)
        ]

instance Show User where
    show (User ni st co ch) = mconcat [ "User ", show ni, " ", show st, " "
                                      , show $ M.keys co, " ", show ch
                                      ]

type CmdConn = Either CmdWrap NewConn

type NewConn = (Nick, POSIXTime, Connection)

type Key = CI Text
type Nick = Key

type Config = Map Key (Map Nick Text)

type Users = Map Nick User

data Privmsg = Privmsg { privNick :: Nick
                       , privName :: Text
                       , privHost :: Text
                       , privDest :: Text
                       , privText :: Text
                       }

-- }}}

-- {{{ Constants

nick :: Text
nick = "Chabunko"

server :: String
server = "irc.rizon.net"

port = 6667

channels :: [Text]
channels = [ "#bnetmlp" ]

-- }}}

-- {{{ Debugging

warn :: (MonadIO m, Show a) => a -> m ()
warn x = liftIO $ putStrLn $ "\x1b[0;33mWarning " <> show x <> "\x1b[0m"

verb :: (MonadIO m, Show a) => a -> m ()
verb x = liftIO $ putStrLn $ "\x1b[1;33mVerbose " <> show x <> "\x1b[0m"

erro :: (MonadIO m, Show a) => a -> m ()
erro x = liftIO $ putStrLn $ "\x1b[0;31mError " <> show x <> "\x1b[0m"

-- }}}

-- {{{ Parsing

name :: Parser Nick
name = do
    asciiCI "name"
    space
    CI.mk <$> takeText

-- TODO channels
msg :: Nick -> POSIXTime -> Parser Cmd
msg n t = do
    asciiCI "msg"
    space
    c <- CI.mk <$> takeWhile1 (/= ' ')
    space
    Post c . Msg n t <$> takeText

opt :: Parser Cmd
opt = do
    asciiCI "opt"
    space
    c <- CI.mk <$> takeWhile1 (/= ' ')
    space
    Opt c . CI.mk <$> takeWhile1 (/= ' ')

set :: Nick -> Parser Cmd
set n = do
    asciiCI "set"
    space
    k <- CI.mk <$> takeWhile1 (/= ' ')
    space
    Set n k <$> takeText

enter :: Nick -> Parser Cmd
enter n = do
    asciiCI "join"
    space
    Join n . CI.mk <$> takeWhile1 (/= ' ')

part :: Nick -> Parser Cmd
part n = do
    asciiCI "part"
    space
    Part n . CI.mk <$> takeWhile1 (/= ' ')

opts :: Parser Cmd
opts = do
    asciiCI "opts"
    space
    Opts . CI.mk <$> takeWhile1 (/= ' ')

req :: Nick -> Parser Cmd
req n = do
    asciiCI "req"
    space
    c <- CI.mk <$> takeWhile1 (/= ' ')
    space
    mt <- readMay . T.unpack <$> takeWhile1 isDigit
    return . Req n c . fromIntegral $ fromJust (mt :: Maybe Integer)

list :: Parser Cmd
list = do
    asciiCI "list"
    space
    c <- CI.mk <$> takeWhile1 (/= ' ')
    return $ List c

ban :: Nick -> POSIXTime -> Parser Cmd
ban n t = do
    asciiCI "ban"
    space
    v <- CI.mk <$> takeText
    return $ Ban n t v

runtimeCmd :: Text -> Nick -> POSIXTime -> Maybe Cmd
runtimeCmd xs n t = parseMay xs parser
  where
    parser = msg n t <|> opt <|> set n <|> opts <|> req n
                     <|> list <|> ban n t <|> enter n

parseMay :: Text -> Parser a -> Maybe a
parseMay t p = maybeResult $ parse p t `feed` ""


ircUser :: Parser (Nick, Text, Text)
ircUser = do
    char ':'
    nick <- CI.mk <$> takeWhile1 (/= '!')
    char '!'
    name <- takeWhile1 (/= '@')
    char '@'
    host <- takeWhile1 (/= ' ')
    space
    return $ (nick, name, host)

privmsg :: Parser Privmsg
privmsg = do
    (nick, name, host) <- ircUser
    string "PRIVMSG"
    space
    dest <- takeWhile1 (/= ' ')
    space
    _ <- char ':'
    text <- takeText
    return $ Privmsg nick name host dest text

-- }}}

-- {{{ Utils

safe :: IO a -> IO (Either () a)
safe io = do
    ei <- try io
    case ei of
        Left er -> warn er >> return (Left ())
        Right a -> return $ Right a
  where
    try :: IO a -> IO (Either SomeException a)
    try = E.try

trySt :: E.Exception e => StateT s IO a -> StateT s IO (Either e a)
trySt st = do
    s <- get
    liftIO $ E.try $ fst <$> runStateT st s

safeSt :: StateT s IO a -> StateT s IO (Either () a)
safeSt st = do
    e <- trySt st
    case e of
        Left (er :: SomeException) -> warn er >> return (Left ())
        Right a -> return $ Right a

escape :: Text -> Text
escape = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

quote :: (Monoid a, IsString a) => a -> a
quote x = "\"" <> x <> "\""

nullConns :: Nick -> Users -> Bool
nullConns nk cns = maybe True ((== 0) . length . M.toList . userConns) $ M.lookup nk cns

offSum :: Num a => [a] -> a
offSum [] = 0
offSum [_] = 0
offSum (x:y:[]) = x - y
offSum (x:y:xs) = x - y + offSum (y:xs)

-- | Filter users by which have a certain channel in their `userChans'.
chanUsers :: Key -> Users -> Users
chanUsers c = M.filter (elem c . userChans)

-- }}}

-- | Accept a WebSocket connection. Wait for a nickname. Send the new socket
--   associated with a timestamp and the nickname to the `consumer'.
app :: TMVar CmdConn -> ServerApp
app t pc = do
    c <- acceptRequest pc
    time <- getPOSIXTime
    eu <- nickInit c
    flip (either $ close c) eu $ \u -> do
        atomically $ putTMVar t $ Right (u, time, c)
        appListen t u c time
  where
    nickInit c = do
        em <- safe $ receiveData c
        let emnick = flip parseMay name <$> em
        case emnick of
            Right (Just n) -> do
                verb ("Name: " <> CI.original n)
                if n == "" then return $ Left () else return $ Right n
            Right Nothing -> do
                let da = ($ ("name <nickname>" :: Text))
                forM_ [warn, void . safe . sendTextData c] da
                nickInit c
            Left _ -> return $ Left ()
    close c _ = sendClose c ("Connection closed" :: Text) >> return ()

-- | Listen to client socket. If 60 seconds pass, send a ping and wait 10
--   seconds for a pong response, if nothing received close the connection.
--
--   This sends received commands to the `consumer'.
appListen :: TMVar CmdConn -> Nick -> Connection -> POSIXTime -> IO ()
appListen t u c time = do
    mem <- safe . timeout (10^6 * 60) $ receiveData c
    case mem of
        Right Nothing -> do
            _ <- safe $ sendTextData c ("ping" :: Text)
            verb $ "Ping " <> CI.original u <> " " <> T.pack (show time)
            pong t u c time

        Right (Just m) -> do
            timestamp <- getPOSIXTime
            let me = Left . CmdWrap time c <$> runtimeCmd m u timestamp
            maybe (warn $ "Not a valid cmd: " <> m) (atomically . putTMVar t) me
            appListen t u c time

        Left _ -> do
            sendClose c ("appListen: Connection closed" :: Text)
            atomically . putTMVar t . Left . CmdWrap time c $ End u

-- | Wait 10 seconds for a response to a ping before closing the socket.
pong :: TMVar CmdConn -> Nick -> Connection -> POSIXTime -> IO ()
pong t u c time = do
    emm <- safe . timeout (10^6 * 10) $ receiveData c
    case emm of
        Left _ -> do
            let da = ($ ("pong: Connection closed" :: Text))
            forM_ [void . safe . sendClose c, warn] da
            atomically . putTMVar t . Left . CmdWrap time c $ End u
        Right Nothing -> do
            warn $ "Connection timeout for " <> u
            let da = ($ "Ping timeout: " <> CI.original u)
            forM_ [void . safe . sendClose c, warn] da
            atomically . putTMVar t . Left . CmdWrap time c $ End u
        Right (Just (m :: Text)) ->
            if CI.mk m == "pong"
            then do
                verb ("Pong " <> CI.original u <> " " <> T.pack (show time))
                appListen t u c time
            else do
                let da = ($ "Ping timeout: " <> CI.original u)
                forM_ [void . safe . sendClose c, warn] da

-- TODO channels
-- TODO split up, make `consumePost', `consumeReq', `consumeBan', etc
-- | The "core" that talks to everything else.
consumer :: TMVar CmdConn -> IO ()
consumer t = void . flip runStateT (mempty :: Memory) $ forever $ do
    e <- liftIO . atomically $ takeTMVar t
    flip (`either` addConn) e $ \(CmdWrap time cn cmd) -> do
        m@(Memory us cf ms) <- get
        case cmd of
            -- TODO finish anti-flooding!
            -- | Post a message
            Post c mg -> do
                verb $ "Received a message from " <> CI.original (msgNick mg)
                let umsgs = P.take 10 $ filter ((== msgNick mg) . msgNick) ms
                    times = map (msgTime) $ mg : umsgs
                    n :: POSIXTime
                    n = product (replicate (length times) 1.1) * genericLength times
                warn times
                warn $ show (offSum times) <> " < " <> show n
                if length umsgs >= 2 && offSum times < n
                then liftIO $ sendTextData cn ("warn 1" :: Text)
                else do
                    put $ m { msgs = P.take 200 $ mg : ms }

                    relay (Just c) time $ "msgs " <> "[" <> (T.pack . show $ mg) <> "]"

            -- | Join a channel
            Join nk c -> do
                m <- get
                let cus = chanUsers c $ users m
                when (nullConns nk cus) . void . safeSt $ do
                    relay (Just c) time $ "join " <> (quote . escape $ CI.original nk)

                let du = Just $ User nk Online (M.singleton time cn) [c]
                    f = maybe du Just . fmap (\u -> u { userChans = c : userChans u})
                put $ m { users = M.alter f nk $ users m }

            -- TODO
            -- | Part a channel
            Part nk c -> do
                return ()

            -- | Set a config value
            Set nk ke va -> do
                let f = Just . M.insert nk va . maybe mempty id
                put $ m { config = M.alter f ke cf }
                let tu = makeObj
                            [ ( T.unpack $ CI.original ke
                              , makeObj
                                    [ ( T.unpack $ CI.original nk
                                      , showJSON va
                                      ) ]
                              ) ]
                relay Nothing time $ "set " <> T.pack (encodeStrict tu)

            -- | Return all values from users under an option
            Opt c ke -> liftIO $ do
                let cus :: Map Nick Text
                    cus = M.intersection (maybe mempty id $ M.lookup ke cf) (chanUsers c us)
                    pave (f, s) = (T.unpack $ CI.original f, showJSON s)
                    cus' :: [(String, JSValue)]
                    cus' = [ ( T.unpack . CI.original $ ke
                            , makeObj . map pave $ M.toList cus
                            ) ]
                sendTextData cn $ "opt " <> T.pack (encodeStrict $ makeObj cus')

            -- TODO channels
            -- | Return all option keys
            Opts c -> liftIO $ do
                let cf' = map CI.original $ M.keys cf
                sendTextData cn $ "opts " <> T.pack (encodeStrict $ showJSON cf')

            Req nk c ts -> do
                liftIO $ print ms
                let ums = map (T.pack . show) $ filter ((> ts) . msgTime) ms
                    ls = "[" <> T.intercalate "," (reverse ums) <> "]"
                liftIO . void . safe $ sendTextData cn $ "msgs " <> ls

            -- | List users
            List c -> do
                verb us
                verb $ chanUsers c us
                let cus = chanUsers c us
                    usl = encodeStrict . showJSON . map CI.original $ M.keys cus
                liftIO . void . safe $ sendTextData cn $ "list " <> T.pack usl

            -- TODO check if user is mod, subsequently ban target if so
            -- | Ban a user
            Ban nk ts vi -> do
                let isMod = const False
                liftIO $ if isMod nk
                then return ()
                else sendTextData cn ("ban 0" :: Text)

            -- | Quit
            End nk -> cEnd nk time

        liftIO $ print cmd
  where
    addConn (nk, ts, cn) = do
        m <- get

        let du = Just $ User nk Online (M.singleton ts cn) []
            f = maybe du Just . fmap (\u -> u { userConns = M.insert ts cn $ userConns u })
        put $ m { users = M.alter f nk $ users m }

-- | w0t does this do
cEnd :: Nick -> POSIXTime -> StateT Memory IO ()
cEnd nk time = do
    m@(Memory us cf ms) <- get

    let us' = M.alter (fmap (\u -> u { userConns = M.delete time $ userConns u })) nk us
        empty = nullConns nk us'
        f = if empty then M.delete nk else id

    put $ m { users = f us' }

    when empty . void . safeSt $ do
        let nick = encode . showJSON $ CI.original nk
        relay Nothing time $ "quit " <> T.pack nick

-- | Relay a message to all other sockets.
relay :: Maybe Key -> POSIXTime -> Text -> StateT Memory IO ()
relay c time te = do
    us <- gets users
    verb $ "Relaying message to " <> show (length $ M.keys us) <> " users"
    verb te

    let cus = M.toList $ maybe us (flip chanUsers us) c

    forM_ cus $ \(nk, u) -> forM_ (M.toList $ userConns u) $ \(ts, cn) -> do
        unless (time == ts) $ do
            verb $ "Sending to " <> CI.original nk <> " " <> T.pack (show ts)
            e <- liftIO . safe $ sendTextData cn te
            case e of
                Right _ -> return ()
                Left _ -> cEnd nk time

ircWrite _ _ = return ()

ircListen _ = return ()
    -- atomically (takeTMVar ircin) >>= ircWrite h . ("PRIVMSG #bnetmlp :" <>)

runIRC :: TMVar Text -> TMVar Text -> IO ()
runIRC ircout ircin = do
    h <- connectTo server (PortNumber $ fromIntegral port)
    utf8t <- mkTextEncoding "UTF-8//TRANSLIT"
    hSetEncoding h utf8t
    hSetBuffering h LineBuffering
    hSetNewlineMode h (NewlineMode CRLF CRLF)
    ircWrite h $ "NICK " <> nick
    ircWrite h $ "USER " <> nick <> " 0 * :" <> nick
    forM_ channels $ ircWrite h . ("JOIN " <>)
    _ <- forkIO . forever $ userInput h
    _ <- forkIO $ ircListen h
    void . forkIO . forever $
        atomically . putTMVar ircout =<< T.hGetLine h
  where
    userInput h = do
        line <- getLine
        hPutStrLn h line


main :: IO ()
main = do
    verb "Starting server..."
    as <- getArgs
    let n = case as of
            (sn:_) -> maybe 8088 id $ readMay sn
            [] -> 8088
    tcc <- newEmptyTMVarIO
    temc <- newEmptyTMVarIO
    _ <- forkIO $ consumer tcc
    runServer "0.0.0.0" n (app tcc)

