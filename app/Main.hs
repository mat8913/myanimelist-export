-- Copyright (C) 2017  Matthew Harm Bekkema
--
-- This file is part of myanimelist-export.
--
-- myanimelist-export is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- myanimelist-export is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Function (fix)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Data.Text (Text)

import           Data.Yaml     (decodeFileEither)
import           Data.Aeson.TH (deriveJSON, defaultOptions)

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Exception (throwIO)

import           Data.Conduit
import           Data.Conduit.Zlib   (ungzip)
import           Data.Conduit.Binary (sinkFile)

import           System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)

import           Network.URI                  (URI)
import           Network.HTTP.Client          (BodyReader, Manager, brRead,
                                               defaultRequest, withResponse,
                                               responseBody)
import           Network.HTTP.Client.TLS      (newTlsManager)
import           Network.HTTP.Client.Internal (setUri)

import           MyAnimeList.Export


data Config = Config { username     :: Text
                     , password     :: Text
                     , animeXmlPath :: FilePath
                     , mangaXmlPath :: FilePath
                     }

$(deriveJSON defaultOptions ''Config)


main :: IO ()
main = do
    configPath <- getXdgDirectory XdgConfig "myanimelist-export.yaml"
    Config {..} <- either throwIO pure =<< decodeFileEither configPath
    manager <- newTlsManager
    putStrLn "exporting"
    [animeUri, mangaUri] <- exportLists username password manager [Anime, Manga]
    putStrLn "downloading anime list"
    downloadList manager animeUri animeXmlPath
    putStrLn "downloading manga list"
    downloadList manager mangaUri mangaXmlPath

downloadList :: Manager -> URI -> FilePath -> IO ()
downloadList manager uri fp = do
    request <- setUri defaultRequest uri
    withResponse request manager $ \res ->
        runConduitRes $ sourceBodyReader (responseBody res)
                     .| ungzip
                     .| sinkFile fp

sourceBodyReader :: MonadIO m => BodyReader -> ConduitM i ByteString m ()
sourceBodyReader br = fix $ \r -> do
    x <- liftIO $ brRead br
    unless (BS.null x) (yield x >> r)
