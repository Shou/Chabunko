
-- Copyright Shou, 2013
-- Licensed under the GNU GPLv2

-- TODO
--  - Separate metadata file per user
--      - Set avatars
--      - We can do anything!
--  - Flood control!!
--      - Send three messages per second only???


{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

import Blaze.ByteString.Builder.ByteString (fromByteString)

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans

import Data.Conduit.Internal (ConduitM (..))
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Network
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp hiding (Handle)
import Network.Wai.Parse

import System.IO
import System.IO.Unsafe (unsafePerformIO)

-- }}}


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

mvar :: MVar Text
mvar = unsafePerformIO newEmptyMVar


atMay :: Int -> [a] -> Maybe a
atMay _ [] = Nothing
atMay 0 (x:_) = Just x
atMay n (x:xs) = atMay (pred n) xs

app :: Application
app req = case pathInfo req of
    ("irc-send":_) -> ircIn req
    ("irc-new":_) -> ircNew req
    ("irc":_) -> ircOut req
    _ -> return $ res "Who are you?!?!? are you food!!!"

-- UGUU!!!
ircRes req = case requestMethod req of
    "GET" -> ircOut req
    "POST" -> ircIn req

ircOut req = do
    liftIO $ print $ queryString req
    base <- liftIO $ T.readFile "base.html"
    ls <- liftIO $ reverse . dropEmpty . T.lines <$> T.readFile "irc.txt"
    let ps = T.unlines . reverse . map mkLine . appendTimes $ take 100 ls
        query = map conv $ queryString req
        def' = ("html", ps) : query <> def
        html = format base def'
    if isJust $ lookup "nick" query
    then return $ res $ T.encodeUtf8 html
    else return $ res "Error - no nickname provided!"
  where
    appendTime x (acc, (ot, n)) =
        let cls = T.split (== '\t') x
            mct = atMay 0 cls
            ct = maybe "" id mct
            n' = if ot == ct && ct /= "" then succ n else 0
            ctn = ct <> T.cons '-' (T.pack $ show n')
            x' = T.intercalate "\t" $ ctn : drop 1 cls
        in (x' : acc, (ct, n'))
    appendTimes = fst . foldr appendTime (mempty, ("", 0))
    dropEmpty = dropWhile (== mempty)
    mkLine x = "<span class=line>" <> mkCons x <> "</span>"
    wrap = zipWith (\x y -> "<span class=" <> x <> ">" <> y <> "</span>")
    mkCons x = T.unwords $ wrap ["timestamp", "nick", "message"] (T.split (== '\t') x)
    conv (x, y) = (T.decodeUtf8 x, maybe "" T.decodeUtf8 y)

-- sink = Sink 
-- parseRequestBody sink req
ircIn req = do
    liftIO $ print $ queryString req
    let mnick = join $ lookup "nick" $ queryString req
        mmsg = join $ lookup "msg" $ queryString req
        mfull = (\x y -> x <> ": " <> y) <$> mnick <*> mmsg
    if isJust $ lookup "nick" $ queryString req
    then do
        liftIO $ maybe (return ()) (putMVar mvar . T.decodeUtf8) mfull
        return $ res "You posted!"
    else return $ res "Error - no nickname provided!"

ircNew req = do
    liftIO $ print $ queryString req
    ls <- liftIO $ reverse . dropEmpty . T.lines <$> T.readFile "irc.txt"
    let mtime = join $ lookup "time" $ queryString req
        time = maybe "" T.decodeUtf8 mtime
        ps = T.unlines . reverse . take 100 . map mkLine . dropOld time $ appendTimes ls
        query = map conv $ queryString req
    if isJust $ lookup "nick" query
    then return $ res $ T.encodeUtf8 ps
    else return $ res "Error - no nickname provided!"
  where
    appendTime x (acc, (ot, n)) =
        let cls = T.split (== '\t') x
            mct = atMay 0 cls
            ct = maybe "" id mct
            n' = if ot == ct && ct /= "" then succ n else 0
            ctn = ct <> T.cons '-' (T.pack $ show n')
            x' = T.intercalate "\t" $ ctn : drop 1 cls
        in (x' : acc, (ct, n'))
    appendTimes = fst . foldr appendTime (mempty, ("", 0))
    dropEmpty = dropWhile (== mempty)
    dropOld time = filter ((time <) . head . T.split (== '\t'))
    mkLine x = "<span class=line>" <> mkCons x <> "</span>"
    wrap = zipWith (\x y -> "<span class=" <> x <> ">" <> y <> "</span>")
    mkCons x = T.unwords $ wrap ["timestamp", "nick", "message"] (T.split (== '\t') x)
    conv (x, y) = (T.decodeUtf8 x, maybe "" T.decodeUtf8 y)

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

format t [] = t
format t ((k, v) : xs) = format (T.replace ("%" <> k) v t) xs

--res :: Response
res t = ResponseBuilder
    ok200
    [ ( "Content-Type", "text/html; charset=utf-8" )
    ] $ fromByteString t

splits :: [Char] -> T.Text -> [T.Text]
splits (c:cs) t = splits' cs $ T.split (==c) t
  where
    splits' :: [Char] -> [T.Text] -> [T.Text]
    splits' [] ts = ts
    splits' (c':cs') ts = splits' cs' . concat $ map (T.split (==c')) ts

write :: Handle -> T.Text -> IO ()
write h t = T.hPutStrLn h t >> T.putStrLn ('>' `T.cons` t)

listen :: Handle -> IO ()
listen h = forever $ do
    line <- T.hGetLine h
    case () of
      _ | T.take 4 line == "PING" -> write h $ "PONG" `T.append` (T.drop 4 line)
        | otherwise -> do
            let lineSplit = T.split (== ':') line
                msg = T.intercalate ":" $ drop 2 lineSplit
                args = splits "!@ " . T.intercalate ":" $ take 2 lineSplit
            T.putStrLn $ (T.pack . show $ args) `T.append` T.cons ' ' msg

runIRC :: IO ()
runIRC = do
    h <- connectTo server (PortNumber $ fromIntegral port)
    hSetEncoding h utf8
    hSetBuffering h LineBuffering
    hSetNewlineMode h (NewlineMode CRLF CRLF)
    write h $ "NICK " `T.append` nick
    write h $ "USER " `T.append` nick `T.append` " 0 * :" `T.append` nick
    forM_ channels $ write h . ("JOIN " <>)
    forkIO . forever $ userInput h
    forkIO . forever $ takeMVar mvar >>= write h . ("PRIVMSG #bnetmlp :" <>)
    void . forkIO $ listen h
  where
    userInput h = do
        line <- getLine
        hPutStrLn h line


main :: IO ()
main = runSettings settings app
