-- Copyright (C) 2017-2018  Matthew Harm Bekkema
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

import           Data.Maybe    (mapMaybe)
import           Data.Function (fix)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Data.Text (Text)

import           Data.Yaml     (decodeFileEither)
import           Data.Aeson.TH (deriveJSON, defaultOptions, omitNothingFields)

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Exception (throwIO)

import           Data.Functor.Compose (Compose(Compose))

import           Data.Conduit
import           Data.Conduit.Zlib   (ungzip)
import           Data.Conduit.Binary (sinkFile)

import           System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)

import           Network.URI                  (URI, uriToString)
import           Network.HTTP.Client          (Manager, BodyReader, CookieJar,
                                               withResponse, parseRequest,
                                               responseBody, brRead, cookieJar)
import           Network.HTTP.Client.TLS      (getGlobalManager)

import           MyAnimeList.Export


data Config = Config { username     :: Text
                     , password     :: Text
                     , animeXmlPath :: Maybe FilePath
                     , mangaXmlPath :: Maybe FilePath
                     }

$(deriveJSON defaultOptions {omitNothingFields = True} ''Config)


main :: IO ()
main = do
    configPath <- getXdgDirectory XdgConfig "myanimelist-export.yaml"
    Config {..} <- either throwIO pure =<< decodeFileEither configPath
    manager <- getGlobalManager
    putStrLn "exporting"
    let wanted = mapMaybe sequence [ (Anime, animeXmlPath)
                                   , (Manga, mangaXmlPath)
                                   ]
    (cj, Compose uris) <- exportLists username password manager $ Compose $
        fmap (\x -> (x, fst x)) wanted
    forM_ uris $ \((mt, fp), uri) -> do
        putStrLn $ "downloading " ++ mediaTypeString mt ++ " list"
        downloadList manager cj uri fp

downloadList :: Manager -> CookieJar -> URI -> FilePath -> IO ()
downloadList manager cj uri fp = do
    request <- fmap (\x -> x { cookieJar = Just cj }) $
               parseRequest $
               uriToString id uri ""
    withResponse request manager $ \res ->
        runConduitRes $ sourceBodyReader (responseBody res)
                     .| ungzip
                     .| sinkFile fp

sourceBodyReader :: MonadIO m => BodyReader -> ConduitM i ByteString m ()
sourceBodyReader br = fix $ \r -> do
    x <- liftIO $ brRead br
    unless (BS.null x) (yield x >> r)

mediaTypeString :: MediaType -> String
mediaTypeString Anime = "anime"
mediaTypeString Manga = "manga"
