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

-----------------------------------------------------------------------------
-- |
-- Module      : MyAnimeList.Export
-- Copyright   : Matthew Harm Bekkema 2017
-- License     : GPL-3
-- Maintainer  : mbekkema97@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MyAnimeList.Export
    ( MediaType(..)
    , exportLists
    ) where

import           Data.Function

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent.Async (Concurrently(Concurrently),
                                           runConcurrently)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8

import           Data.Text          (Text)
import           Data.Text.Encoding (encodeUtf8)

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Text.HTML.TagStream.ByteString (Token, tokenStream)
import           Text.HTML.TagStream            (Token' (TagOpen))

import           Network.URI         (URI, parseURIReference, uriPath,
                                      relativeTo)
import           Network.HTTP.Client

import           MemoizedTraverse (memoizedTraverse)


-- | Represents a media type supported by MyAnimeList
data MediaType = Anime | Manga
  deriving (Read, Show, Eq, Ord, Enum)

newtype CSRF = CSRF { unCsrf :: ByteString }


malHomePage :: Request
malHomePage = "https://myanimelist.net/"

malLoginPage :: Request
malLoginPage = malHomePage { path = "/login.php" }

malExportPage :: Request
malExportPage = malHomePage { path = "/panel.php?go=export" }

malExportPath :: String
malExportPath = "/export/download.php"


-- | Export list(s) and return URL(s) to gzipped XML
exportLists :: (Functor f, Foldable f)
            => Text        -- ^ Username
            -> Text        -- ^ Password
            -> Manager     -- ^ Manager (must support TLS)
            -> f MediaType -- ^ Which list(s) to export
            -> IO (f URI)
exportLists username password manager mediaTypes = do
    (cj, csrf) <- getCsrf manager
    cj' <- login username password cj csrf manager
    runConcurrently $ memoizedTraverse
            (Concurrently . exportList cj' csrf manager) mediaTypes

login :: Text      -- ^ Username
      -> Text      -- ^ Password
      -> CookieJar
      -> CSRF
      -> Manager
      -> IO CookieJar
login username password cj csrf manager = withResponse req manager $
    pure . responseCookieJar
  where
    req = urlEncodedBody [ ("user_name", encodeUtf8 username)
                         , ("password", encodeUtf8 password)
                         , ("sublogin", "Login")
                         , ("submit", "1")
                         , ("csrf_token", unCsrf csrf)
                         ]
                         malLoginPage { cookieJar = Just cj }

getCsrf :: Manager
        -> IO (CookieJar, CSRF)
getCsrf manager = withResponse malHomePage manager $ \res ->
    runConduit $ sourceBodyReader (responseBody res)
              .| tokenStream
              .| C.mapMaybe tagToCsrf
              .| (await >>= maybe
                           (fail $ moduleName ++ ".getCsrf: couldn't find csrf")
                           (pure . (,) (responseCookieJar res))
                 )

exportList :: CookieJar
           -> CSRF
           -> Manager
           -> MediaType
           -> IO URI
exportList cj csrf manager mediaType = withResponse req manager $ \res ->
    runConduit $ sourceBodyReader (responseBody res)
              .| tokenStream
              .| C.mapMaybe tagToUri
              .| C.filter ((malExportPath ==) . uriPath)
              .| C.map (`relativeTo` getUri malHomePage)
              .| (await >>= maybe
                         (fail $ moduleName ++ ".exportList: couldn't find uri")
                         pure
                 )
  where
    req = urlEncodedBody [ ("type", mediaTypeValue mediaType)
                         , ("subexport", "Export My List")
                         , ("csrf_token", unCsrf csrf)
                         ]
                         malExportPage { cookieJar = Just cj }

sourceBodyReader :: MonadIO m => BodyReader -> ConduitM i ByteString m ()
sourceBodyReader br = fix $ \r -> do
    x <- liftIO $ brRead br
    unless (BS.null x) (yield x >> r)


tagToCsrf :: Token -> Maybe CSRF
tagToCsrf (TagOpen "meta" xs False) = case (lookup "name" xs) of
    Just "csrf_token" -> CSRF <$> lookup "content" xs
    _ -> Nothing
tagToCsrf _ = Nothing

tagToUri :: Token -> Maybe URI
tagToUri (TagOpen "a" xs False) =
    parseURIReference . B8.unpack =<< lookup "href" xs
tagToUri _ = Nothing

mediaTypeValue :: MediaType -> ByteString
mediaTypeValue Anime = "1"
mediaTypeValue Manga = "2"

moduleName :: String
moduleName = "MyAnimeList.Export"
