
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
--  - Banning


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

import           System.IO
import           System.Timeout

import           Text.JSON

import           Prelude                as P

-- }}}

-- {{{ Data

data Memory = Memory { users :: Users
                     , msgs :: [Msg]
                     , conns :: Map Nick (Map POSIXTime Connection)
                     }

instance Monoid Memory where
    mempty = Memory mempty mempty mempty
    mappend (Memory x0 y0 z0) (Memory x1 y1 z1) =
        Memory (x0 <> x1) (y0 <> y1) (z0 <> z1)

data Cmd = Post { postMsg :: Msg }
         | Set { setNick :: Nick, setKey :: CI Text, setVal :: Text }
         | Opt { optKey :: CI Text }
         | Opts
         | Req { reqNick :: Nick, reqTime :: POSIXTime }
         | List
         | Ban { banNick :: Nick, banTime :: POSIXTime, banVictim :: Nick }
         | End { endNick :: Nick }
         deriving (Show)

data Msg = Msg { msgNick :: Nick, msgTime :: POSIXTime, msgText :: Text }

data CmdWrap = CmdWrap { wrapTime :: POSIXTime
                       , wrapConn :: Connection
                       , wrapCmd :: Cmd
                       }

instance Show Msg where
    show (Msg ni ts mg) = encodeStrict . makeObj $
        [ ("nick", showJSON . T.unpack $ CI.original ni)
        , ("time", showJSON (realToFrac ts :: Double))
        , ("msg", showJSON $ T.unpack mg)
        ]

type CmdConn = Either CmdWrap NewConn

type NewConn = (Nick, POSIXTime, Connection)

type Nick = CI Text
type Key = CI Text

type Users = Map Key (Map Nick Text)

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

msg :: Nick -> POSIXTime -> Parser Cmd
msg n t = do
    asciiCI "msg"
    space
    Post . Msg n t <$> takeText

opt :: Parser Cmd
opt = do
    asciiCI "opt"
    space
    Opt . CI.mk <$> takeWhile1 (/= ' ')

set :: Nick -> Parser Cmd
set n = do
    asciiCI "set"
    space
    k <- CI.mk <$> takeWhile1 (/= ' ')
    space
    Set n k <$> takeText

opts :: Parser Cmd
opts = asciiCI "opts" >> return Opts

req :: Nick -> Parser Cmd
req n = do
    asciiCI "req"
    space
    mt <- readMay . T.unpack <$> takeWhile1 isDigit
    return . Req n . fromIntegral $ fromJust (mt :: Maybe Integer)

list :: Parser Cmd
list = do
    asciiCI "list"
    return List

ban :: Nick -> POSIXTime -> Parser Cmd
ban n t = do
    asciiCI "ban"
    space
    v <- CI.mk <$> takeText
    return $ Ban n t v

runtimeCmd :: Text -> Nick -> POSIXTime -> Maybe Cmd
runtimeCmd xs n t = parseMay xs parser
  where
    parser = msg n t <|> opt <|> set n <|> opts <|> req n <|> list <|> ban n t

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

nullConns :: Ord a => a -> Map a (Map b c) -> Bool
nullConns nk cns = maybe True ((== 0) . length . M.toList) $ M.lookup nk cns

offSum :: Num a => [a] -> a
offSum [] = 0
offSum [_] = 0
offSum (x:y:[]) = x - y
offSum (x:y:xs) = x - y + offSum (y:xs)

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

-- TODO
-- TODO split up, make `consumePost', `consumeReq', `consumeBan', etc
-- | The "core" that talks to everything else.
consumer :: TMVar CmdConn -> IO ()
consumer t = void . flip runStateT (mempty :: Memory) $ forever $ do
    e <- liftIO . atomically $ takeTMVar t
    flip (`either` addConn) e $ \(CmdWrap time cn cmd) -> do
        m@(Memory ks ms cs) <- get
        case cmd of
            -- TODO finish anti-flooding!
            Post mg -> do
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

                    relay time $ "msgs " <> "[" <> (T.pack . show $ mg) <> "]"

            Set nk ke va -> do
                let f = Just . M.insert nk va . maybe mempty id
                put $ m { users = M.alter f ke ks }
                let tu = makeObj
                            [ ( T.unpack $ CI.original ke
                              , makeObj
                                    [ ( T.unpack $ CI.original nk
                                      , showJSON va
                                      ) ]
                              ) ]
                relay time $ "set " <> T.pack (encodeStrict tu)

            Opt ke -> liftIO $ do
                let us :: Map Nick Text
                    us = maybe mempty id $ M.lookup ke ks
                    pave (f, s) = (T.unpack $ CI.original f, showJSON s)
                    us' :: [(String, JSValue)]
                    us' = [ ( T.unpack . CI.original $ ke
                            , makeObj . map pave $ M.toList us
                            ) ]
                sendTextData cn $ "opt " <> T.pack (encodeStrict $ makeObj us')

            Opts -> liftIO $ do
                let ks' = map CI.original $ M.keys ks
                sendTextData cn $ "opts " <> T.pack (encodeStrict $ showJSON ks')

            Req nk ts -> do
                liftIO $ print ms
                let ums = map (T.pack . show) $ filter ((> ts) . msgTime) ms
                    ls = "[" <> T.intercalate "," (reverse ums) <> "]"
                liftIO . void . safe $ sendTextData cn $ "msgs " <> ls

            List -> do
                let usl = encodeStrict . showJSON . map CI.original $ M.keys cs
                liftIO . void . safe $ sendTextData cn $ "list " <> T.pack usl

            -- TODO check if user is mod, subsequently ban target if so
            Ban nk ts vi -> do
                let isMod = const False
                liftIO $ if isMod nk
                then return ()
                else sendTextData cn ("ban 0" :: Text)

            End nk -> cEnd nk time

        liftIO $ print cmd
  where
    addConn (nk, ts, cn) = do
        m <- get
        when (nullConns nk $ conns m) . void . safeSt $ do
            relay ts $ "join " <> (quote . escape $ CI.original nk)

        let f = Just . M.insert ts cn . maybe mempty id
        put $ m { conns = M.alter f nk $ conns m }

cEnd :: Nick -> POSIXTime -> StateT Memory IO ()
cEnd nk time = do
    m@(Memory ks ms cs) <- get

    let cs' = M.alter (fmap $ M.delete time) nk cs
        empty = nullConns nk cs'
        f = if empty then M.delete nk else id

    put $ m { conns = f cs' }

    when empty . void . safeSt $ do
        let nick = encode . showJSON $ CI.original nk
        relay time $ "quit " <> T.pack nick

-- | Relay a message to all other sockets.
relay :: POSIXTime -> Text -> StateT Memory IO ()
relay time te = do
    cs <- gets conns
    verb $ "Relaying message to " <> show (length $ M.keys cs) <> " users"
    verb te

    forM_ (M.toList cs) $ \(nk, cns) -> forM_ (M.toList cns) $ \(ts, cn) -> do
        unless (time == ts) $ do
            verb $ "Sending to " <> CI.original nk <> " " <> T.pack (show ts)
            e <- liftIO . safe $ sendTextData cn te
            case e of
                Right _ -> return ()
                Left _ -> cEnd nk time

ircWrite _ _ = return ()

ircListen _ = return ()

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
    _ <- forkIO . forever $ do
        atomically (takeTMVar ircin) >>= ircWrite h . ("PRIVMSG #bnetmlp :" <>)
    void . forkIO . forever $
        atomically . putTMVar ircout =<< T.hGetLine h
  where
    userInput h = do
        line <- getLine
        hPutStrLn h line


main :: IO ()
main = do
    tcc <- newEmptyTMVarIO
    temc <- newEmptyTMVarIO
    _ <- forkIO $ consumer tcc
    runServer "0.0.0.0" 8088 (app tcc)

