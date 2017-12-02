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

module MemoizedTraverse (memoizedTraverse) where

import           Data.Foldable

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


memoizedTraverse :: (Applicative f, Functor t, Foldable t, Ord a)
                 => (a -> f b)
                 -> t a
                 -> f (t b)
memoizedTraverse f xs = vals <&> \vals' ->
    lookupErr "myanimelist-export:memoizedTraverse: lookup error" vals' <$> xs
  where
    keys = foldl' (flip (flip M.insert ())) M.empty xs
    vals = M.traverseWithKey (\k () -> f k) keys

lookupErr :: Ord a => String -> Map a b -> a -> b
lookupErr e = flip (M.findWithDefault (error e))

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
