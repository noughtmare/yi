{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  Yi.CompletionTree
License     :  GPL-2
Maintainer  :  yi-devel@googlegroups.com
Stability   :  experimental
Portability :  portable

Little helper for completion interfaces.

Intended to be imported qualified:

>import qualified Yi.CompletionTree as CT
-}
module Yi.CompletionTree ( 
  CompletionTree (CT), Completable (..),
  fromCompletable, toCompletable, 
  update, obvious, pretty
  ) where

import           Control.Arrow   (first) 
import           Data.Function   (on)
import           Data.List       (partition, maximumBy, intercalate)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe      (isJust, fromJust, listToMaybe, catMaybes)
import           Data.Monoid     ((<>))
import           Data.Text       (Text)

-- For completable instances.
import qualified Data.List       as L (isPrefixOf, inits, stripPrefix)
import qualified Data.Text       as T (isPrefixOf, inits, stripPrefix)

-- | A CompletionTree is a map of parital completions.
newtype CompletionTree a = CT (Map a (CompletionTree a)) deriving (Monoid, Show)

-- | All things that can be completed.
class (Monoid a) => Completable a where
  stripPrefix :: a -> a -> Maybe a
  isPrefixOf  :: a -> a -> Bool
  inits       :: a -> [a]

instance Completable Text where
  stripPrefix = T.stripPrefix
  isPrefixOf  = T.isPrefixOf
  inits       = T.inits

instance Eq a => Completable [a] where
  stripPrefix = L.stripPrefix
  isPrefixOf  = L.isPrefixOf
  inits       = L.inits

-- | This function converts a list of completable elements to a CompletionTree
-- It finds elements that share a common prefix and groups them.
fromCompletable :: (Ord a, Completable a, Show a) => [a] -> CompletionTree a
fromCompletable [] = mempty
fromCompletable (x:xs)
  | x == mempty = CT (Map.singleton mempty mempty) <> fromCompletable xs
  | otherwise = case maximumBy' (compare `on` childrenIn xs) (tail $ inits x) of
      Nothing -> CT (Map.singleton x mempty) <> fromCompletable xs
      Just parent -> case first (x:) $ partition (parent `isPrefixOf`) xs of
        ([_],rest) -> CT (Map.singleton parent mempty) <> fromCompletable rest
        (hasParent, rest) -> CT (Map.singleton parent . fromCompletable $
           map (fromJust . stripPrefix parent) hasParent) <> fromCompletable rest
      -- A parent is the prefix and the children are the items with the parent as prefix
      where childrenIn list parent = length $ filter (parent `isPrefixOf`) list

-- | The largest element of a non-empty structure with respect to the
-- given comparison function, Nothing if there are multiple 'largest' elements.
maximumBy' :: Eq a => (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy' cmp l | atleast 2 (== max') l = Nothing
                 | otherwise = Just max'
  where  max' = maximumBy cmp l
         -- This short-circuits if the condition is met n times before the end of the list.
         atleast :: Int -> (a -> Bool) -> [a] -> Bool
         atleast 0 _ _ = True
         atleast _ _ [] = False
         atleast n cmp' (x:xs) | cmp' x = atleast (n - 1) cmp' xs
                               | otherwise = atleast n cmp' xs

-- | Complete as much as possible without guessing.
obvious :: Completable a => CompletionTree a -> (a, CompletionTree a)
obvious (CT ct) | Map.size ct == 1 = Map.elemAt 0 ct
                | otherwise = (mempty,CT ct)

-- | Update the CompletionTree with new information.
update :: (Ord a, Completable a) => CompletionTree a -> a -> CompletionTree a
update (CT ct) p
  | mempty == p = CT ct
  | isJust one = fromJust one
  | isJust remaining = uncurry update $ fromJust remaining
  | otherwise = CT $ Map.mapKeys fromJust
                   $ Map.filterWithKey (const . isJust)
                   $ Map.mapKeys (stripPrefix p) ct
  where
    one = Map.lookup p ct
    remaining = listToMaybe . catMaybes $ map (\p' -> (,fromJust $ stripPrefix p' p) <$> Map.lookup p' ct) (inits p)

-- | Converts a CompletionTree to a list of completions
toCompletable :: Completable a => CompletionTree a -> [a]
toCompletable (CT ct)
  | Map.null ct = [mempty]
  | otherwise = concat $ Map.elems $ Map.mapWithKey (\k v -> map (k <>) $ toCompletable v) ct

-- | For debugging purposes
pretty :: Show a => CompletionTree a -> String
pretty (CT ct)
  | Map.null ct = ""
  | otherwise = "[" ++ intercalate "|" (Map.elems (Map.mapWithKey (\k v -> shows k (pretty v)) ct)) ++ "]"
