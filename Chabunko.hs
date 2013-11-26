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

-- TODO
--  - Separate metadata file per user
--      - Set avatars
--      - Custom nick colors
--      - We can do anything!
--  - Flood control!!
--      - Send five messages per 10 seconds
--  - Timestamp as name or value attribute or something!
--  - Improved highlighting


{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

import           Blaze.ByteString.Builder.ByteString (fromByteString)

import           Control.Applicative
import           Control.Concurrent                  (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource

import           Data.ByteString                     (ByteString)
import           Data.Char                           (ord)
import           Data.Map                            (Map)
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified Data.Text.IO                        as T
import           Data.Time.Clock.POSIX

import           Network
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp            hiding (Handle)

import           System.IO

-- }}}

-- {{{ Constants

settings :: Settings
settings = defaultSettings { settingsPort = 8088 }

server :: String
server = "irc.rizon.net"

port :: Int
port = 6667

nick :: Text
nick = "Chabunko"

channels :: [Text]
channels = ["#bnetmlp"]

messageamount :: Int
messageamount = 200

-- }}}


io :: MonadIO m => IO a -> m a
io = liftIO

debug :: (MonadIO m, Show a) => a -> m ()
debug = io . print

atMay :: Int -> [a] -> Maybe a
atMay _ [] = Nothing
atMay 0 (x:_) = Just x
atMay n (_:xs) = atMay (pred n) xs

colorize :: ByteString -> ByteString
colorize n = "\ETX" <> colorOf n <> n <> "\ETX"

colorOf :: ByteString -> ByteString
colorOf n = pack $ colors !! (sum (map ord snick) `mod` length colors)
  where
    pack = T.encodeUtf8 . T.pack
    snick = T.unpack $ T.decodeUtf8 n
    colors =
        [ "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13"
        ]

type MApplication = MVar Text -> MVar (Text, Int) -> Request -> ResourceT IO Response

appendTime :: Text -> ([Text], (Text, Integer))
           -> ([Text], (Text, Integer))
appendTime x (acc, (ot, n)) =
  let cls = T.split (== '\t') x
      mct = atMay 0 cls
      ct = fromMaybe mempty mct
      n' = if ot == ct && ct /= "" then succ n else 0
      ctn = ct <> T.cons '-' (T.pack $ show n')
      x' = T.intercalate "\t" $ ctn : drop 1 cls
  in (x' : acc, (ct, n'))

app :: MApplication
app mvar0 mvar1 req = case pathInfo req of
    ("irc-send":_) -> ircIn req mvar0 mvar1
    ("irc-new":_) -> ircNew req
    ("irc-set":_) -> ircSet req
    ("irc":_) -> ircOut req
    _ -> return $ res "Who are you?!?!? are you food!!!"

ircOut :: MonadIO m => Request -> m Response
ircOut req = do
    debug $ queryString req
    base <- io $ T.readFile "base.html"
    ls <- io $ reverse . dropEmpty . T.lines <$> T.readFile "irc.txt"
    let ps = T.unlines . reverse . map mkLine . appendTimes $ take messageamount ls
        query = map conv $ queryString req
        -- def' = ("html", ps) : query <> def
        def' = query <> def
        html = format base def'
    return . res $ if isJust $ lookup "nick" query
                   then T.encodeUtf8 html
                   else "Error - no nickname provided!"
  where
    appendTimes = fst . foldr appendTime (mempty, ("", 0))
    dropEmpty = dropWhile (== mempty)
    mkLine x = "<span class=line>" <> mkCons x <> "</span>"
    wrap = zipWith (\x y -> "<span class=" <> x <> ">" <> esc y <> "</span>")
    mkCons x = T.unwords $ wrap ["timestamp", "nick", "message"] (T.split (== '\t') x)
    conv (x, y) = (T.decodeUtf8 x, maybe "" T.decodeUtf8 y)
    esc = T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

ircIn :: MonadIO m => Request -> MVar Text -> MVar (Text, Int) -> m Response
ircIn req mvar0 mvar1 = do
    io $ print $ queryString req
    let mnick = join $ lookup "nick" $ queryString req
        mmsg = join $ lookup "msg" $ queryString req
        mfull = (\x y -> colorize x <> ": " <> y) <$> mnick <*> mmsg
    bs <- io $ M.fromList . map (T.break (== '\t')) . T.lines <$> readBanned
    let mts = join $ flip M.lookup bs . T.decodeUtf8 <$> mnick
        -- i dont believe in side effects
        t = maybe 0 (read . drop 1 . T.unpack) mts
    ct <- io $ floor <$> getPOSIXTime
    case () of
      _ | isNothing mnick -> return $ res "Error - no nickname provided!"
        | t > ct + 1000 -> return $ res $ "Error - you are permanently banned!"
        | t > ct -> return $ res $ "Error - you are temporarily banned!"
        | t <= ct -> do
            let nick = T.decodeUtf8 $ fromJust mnick
            io $ putMVar mvar1 (nick, t)
            io $ maybe (return ()) (putMVar mvar0 . T.decodeUtf8) mfull
            io $ T.writeFile "banned" ""
            -- Surely nothing else will write to the file! (╹◡╹✿)
            io $ forM_ (M.toList bs) $ \(ni, ts) -> unless (ni == nick) $ do
                T.appendFile "banned" $ ni <> ts
            return $ res ""
        | otherwise -> return $ res "Error - I am a cat!"
  where
    readBanned = T.readFile "banned"

ircNew :: MonadIO m => Request -> m Response
ircNew req = do
    io $ print $ queryString req
    ls <- io $ reverse . dropEmpty . T.lines <$> T.readFile "irc.txt"
    let mtime = join $ lookup "time" $ queryString req
        time = maybe "" T.decodeUtf8 mtime
        ps = T.unlines . reverse . take messageamount . dropOld time $ appendTimes ls
        query = map conv $ queryString req
    return . res $ if isJust $ lookup "nick" query
                   then T.encodeUtf8 ps
                   else "Error - no nickname provided!"
  where
    appendTimes = fst . foldr appendTime (mempty, ("", 0))
    dropEmpty = dropWhile (== mempty)
    dropOld time = filter ((time <) . head . T.split (== '\t'))
    conv (x, y) = (T.decodeUtf8 x, maybe "" T.decodeUtf8 y)

ircSet :: MonadIO m => Request -> m Response
ircSet req = do
    return $ res ""

def :: [(Text, Text)]
def = [ ("bg", "#fcfcfc")
      , ("fg", "#101010")
      , ("link", "blue")
      , ("font", "Consolas, monospace")
      , ("size", "9pt")
      , ("oddbg", "#f0f0f0")
      , ("evenbg", "#fcfcfc")
      , ("hoverbg", "#e9e9e9")
      , ("self", "#888888")
      , ("info", "#888888")
      , ("highlight", "red")
      , ("panelbg", "#fcfcfc")

      , ("white", "white")
      , ("black", "black")
      , ("blue", "blue")
      , ("green", "green")
      , ("red", "red")
      , ("darkred", "darkRed")
      , ("darkmagenta", "darkMagenta")
      , ("orange", "orange")
      , ("yellow", "yellow")
      , ("lightgreen", "lime")
      , ("cyan", "cyan")
      , ("lightcyan", "lightCyan")
      , ("darkblue", "darkBlue")
      , ("magenta", "magenta")
      , ("gray", "gray")
      , ("lightgray", "lightGray")

      , ("css", "")
      ]

format :: Text -> [(Text, Text)] -> Text
format t [] = t
format t ((k, v) : xs) = format (T.replace ("%" <> k) v t) xs

--res :: Response
res :: ByteString -> Response
res t = ResponseBuilder
    ok200
    [ ( "Content-Type", "text/html; charset=utf-8" )
    ] $ fromByteString t

-- This is silly, it produces ["", ""] on
-- let x = "foo" in splits x (T.pack x)
-- | Splits input text on every character provided.
splits :: String -- ^ Characters to split on
          -> Text -- ^ Text to split
          -> [Text]
splits [] t = [t]
splits (c:cs) t = splits' cs $ T.split (==c) t
  where
    splits' :: String -> [Text] -> [Text]
    splits' [] ts = ts
    splits' (c':cs') ts = splits' cs' . concat $ map (T.split (==c')) ts

write :: Handle -> T.Text -> IO ()
write h t = T.hPutStrLn h t >> T.putStrLn ('>' `T.cons` t)

listen :: Handle -> IO ()
listen h = forever $ do
    line <- T.hGetLine h
    case () of
      _ | T.take 4 line == "PING" -> write h $ "PONG" `T.append` T.drop 4 line
        | otherwise -> do
            let lineSplit = T.split (== ':') line
                msg = T.intercalate ":" $ drop 2 lineSplit
                args = splits "!@ " . T.intercalate ":" $ take 2 lineSplit
            T.putStrLn $ (T.pack . show $ args) `T.append` T.cons ' ' msg

runIRC :: MVar Text -> IO ()
runIRC mvar = do
    h <- connectTo server (PortNumber $ fromIntegral port)
    hSetEncoding h utf8
    hSetBuffering h LineBuffering
    hSetNewlineMode h (NewlineMode CRLF CRLF)
    write h $ "NICK " <> nick
    write h $ "USER " <> nick <> " 0 * :" <> nick
    forM_ channels $ write h . ("JOIN " <>)
    _ <- forkIO . forever $ userInput h
    _ <- forkIO . forever $ takeMVar mvar >>= write h . ("PRIVMSG #bnetmlp :" <>)
    void . forkIO $ listen h
  where
    userInput h = do
        line <- getLine
        hPutStrLn h line


type Memory = StateT (Map Text [Int]) IO

runAntiFlooder :: MVar (Text, Int) -> IO ()
runAntiFlooder = void . forkIO . void . flip runStateT mempty . antiFlooder

antiFlooder :: MVar (Text, Int) -> Memory ()
antiFlooder m = forever $ do
    (nick, ts) <- io $ takeMVar m
    debug $ "antiFlooder found message by " <> T.unpack nick
    mts <- get
    let mts' = M.alter (Just . maybe [ts] (ts :)) nick mts
        tss = fromJust $ M.lookup nick mts'
    when (length tss >= 5 && offSum (take 5 tss) < 10) $ do
        debug $ "Banning " <> T.unpack nick
        -- IO Exceptions are for losers!
        bt <- textPOSIXTime (+ 30)
        io $ T.appendFile "banned" $ nick <> "\t" <> bt
    put mts'
  where
    -- (゜△゜;)
    offSum (x:y:[]) = x - y
    offSum (x:y:xs) = x - y + offSum (y:xs)
    textPOSIXTime f = io $ T.pack . show . floor . f <$> getPOSIXTime


main :: IO ()
main = do
    debug "Starting server..."
    m0 <- newEmptyMVar
    m1 <- newEmptyMVar
    runAntiFlooder m1 >> runIRC m0 >> runSettings settings (app m0 m1)

