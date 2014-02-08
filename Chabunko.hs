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
    _ -> return $ res "Who are you?!?!? are you food!!!"

ircOut :: MonadIO m => Request -> m Response
ircOut req = do
    debug $ queryString req
    base <- io $ T.readFile "base.html"
    let query = map conv $ queryString req
        -- def' = ("html", ps) : query <> def
        def' = query <> def
        html = format base def'
    return . res $ if isJust $ lookup "nick" query
                   then T.encodeUtf8 html
                   else "Error - no nickname provided!"
  where
    conv (x, y) = (T.decodeUtf8 x, maybe "" T.decodeUtf8 y)

def :: [(Text, Text)]
def = [ ("chan", "home")
      , ("bg", "#fcfcfc")
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
      , ("lightblue", "lightblue")
      , ("magenta", "magenta")
      , ("gray", "gray")
      , ("lightgray", "lightGray")

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

