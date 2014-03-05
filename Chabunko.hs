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


{-# LANGUAGE OverloadedStrings #-}

-- {{{ Imports

import           Blaze.ByteString.Builder.ByteString (fromByteString)

import           Control.Error
import           Control.Monad.State
import           Control.Monad.Trans.Resource

import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as B
import           Data.Maybe
import           Data.Monoid
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified Data.Text.IO                        as T

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp            hiding (Handle)

import           System.Environment

-- }}}

-- {{{ Constants

settings :: Settings
settings = defaultSettings { settingsPort = 8080 }

-- }}}


io :: MonadIO m => IO a -> m a
io = liftIO

debug :: (MonadIO m, Show a) => a -> m ()
debug = io . print

app :: Request -> ResourceT IO Response
app req = case pathInfo req of
    ("irc":_) -> ircOut req
    [x] -> file req $ T.unpack x
    _ -> return $ res "Who are you?!?!? are you food!!!"

ircOut :: MonadIO m => Request -> m Response
ircOut req = do
    debug $ queryString req
    base <- io $ T.readFile "base.html"
    let query = map conv $ queryString req
        def' = query <> def
        html = format base def'
    return . res $ if isJust $ lookup "nick" query
                   then T.encodeUtf8 html
                   else "Error - no nickname provided!"
  where
    conv (x, y) = (T.decodeUtf8 x, maybe "" T.decodeUtf8 y)

file :: MonadIO m => Request -> FilePath -> m Response
file req x = do
    file <- io $ B.readFile (filter (/= '/') x)
    let r = ResponseBuilder
            ok200
            []
            (fromByteString file)
    return r

def :: [(Text, Text)]
def = [ ("chan", "main")
      , ("serv", "localhost")

      , ("bg", "#efeaea")
      , ("fg", "#101010")
      , ("link", "#66ccff")
      , ("font", "\"Junction Regular\", sans-serif")
      , ("size", "1em")
      , ("oddbg", "rgba(255,255,255,0.9)")
      , ("evenbg", "rgb(252,252,252)")
      , ("hoverbg", "rgba(0,0,0.0.1)")
      , ("self", "#666")
      , ("info", "#666")
      , ("highlight", "#ff0090")
      , ("panelbg", "rgb(252,252,252)")

      , ("white", "rgb(247,247,247)")
      , ("black", "rgb(16,16,16)")
      , ("blue", "blue")
      , ("green", "#66aa11")
      , ("red", "#ff0090")
      , ("darkred", "#960050")
      , ("darkmagenta", "#7e40a5")
      , ("orange", "#c47f2c")
      , ("yellow", "#ffba68")
      , ("lightgreen", "#80ff00")
      , ("cyan", "#3579a8")
      , ("lightcyan", "#66ccff")
      , ("lightblue", "lightblue")
      , ("magenta", "#bb88dd")
      , ("gray", "#666")
      , ("lightgray", "#888")

      , ("css", "")
      ]

format :: Text -> [(Text, Text)] -> Text
format t [] = t
format t ((k, v) : xs) = format (T.replace ("%" <> k) v t) xs

res :: ByteString -> Response
res t = ResponseBuilder
    ok200
    [ ( "Content-Type", "text/html; charset=utf-8" )
    ] $ fromByteString t


main :: IO ()
main = do
    as <- getArgs
    let n = case as of
            (sn:_) -> maybe 8008 id $ readMay sn
            [] -> 8008
        cfg = settings { settingsPort = n }
    debug "Starting server..."
    runSettings cfg app

