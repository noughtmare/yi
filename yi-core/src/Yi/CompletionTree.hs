{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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
  -- * CompletionTree type
  CompletionTree (),
  -- * Lists
  fromList, toList,
  -- * Modification
  complete, update, updateOne,
  -- * Debugging
  pretty
  ) where

import           Control.Arrow       (first)
import           Data.Function       (on)
import           Data.List           (partition, maximumBy, intercalate)
import qualified Data.Map.Strict     as M
import           Data.Map.Strict     (Map)
import           Data.Maybe          (isJust, fromJust, listToMaybe, catMaybes)
import qualified Data.ListLike       as LL
import           Data.ListLike       (ListLike)
import           Lens.Micro.Platform (over, makeLenses, Lens')

-- | A CompletionTree is a map of partial completions.
newtype CompletionTree a = CompletionTree {_completionTree :: (Map a (CompletionTree a))}
  deriving (Monoid)

completionTree :: Lens' (CompletionTree a) (Map a (CompletionTree a))
completionTree f ct = (\completionTree' -> ct {_completionTree = completionTree'}) <$>
                        f (_completionTree ct)

instance (Show a, ListLike a i) => Show (CompletionTree a) where
  show ct = "fromList " ++ show (toList ct)

stripPrefix :: (Eq item, ListLike full item) => full -> full -> Maybe full
stripPrefix a b
  | LL.null a = Just b
  | LL.null b = Nothing
  | LL.head a == LL.head b = stripPrefix (LL.tail a) (LL.tail b)
  | otherwise = Nothing

-- | This function converts a list of completable elements to a CompletionTree
-- It finds elements that share a common prefix and groups them.
--
-- prop> fromList . toList = id
fromList :: (Ord a, ListLike a i, Show a, Eq i) => [a] -> CompletionTree a
fromList [] = mempty
fromList (x:xs)
  | x == mempty = over completionTree (M.insert mempty mempty) (fromList xs)
  | otherwise = case maximumBy' (compare `on` childrenIn xs) (tail $ LL.inits x) of
      Nothing -> over completionTree (M.insert x mempty) (fromList xs)
      Just parent -> case first (x:) $ partition (parent `LL.isPrefixOf`) xs of
        ([_],rest) -> over completionTree (M.insert parent mempty) $ fromList rest
        (hasParent, rest) -> over completionTree (M.insert parent (fromList $
           map (fromJust . stripPrefix parent) hasParent)) $ fromList rest
      -- A parent is the prefix and the children are the items with the parent as prefix
      where childrenIn list parent = length $ filter (parent `LL.isPrefixOf`) list

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
--
-- Examples:
--
-- >>> complete $ fromList ["put","putStrLn","putStr"]
-- ("put", fromList ["","Str","StrLn"])
--
-- >>> complete $ fromList ["put","putStr","putStrLn","xxx"]
-- ("", fromList ["put","putStr","putStrLn","xxx"])
complete :: ListLike a i => CompletionTree a -> (a, CompletionTree a)
complete (CompletionTree ct)
  | M.size ct == 1 = M.elemAt 0 ct
  | otherwise = (mempty,CompletionTree ct)

-- | Update the CompletionTree with new information.
--
-- Examples:
--
-- >>> update (fromList ["put","putStr"]) "p"
-- fromList ["ut","utStr"]
--
-- >>> update (fromList ["put","putStr"]) "put"
-- fromList ["","Str"]
--
-- >>> update (fromList ["put","putStr"]) "putS"
-- fromList ["tr"]
update :: (Ord a, ListLike a i, Eq i) => CompletionTree a -> a -> CompletionTree a
update (CompletionTree ct) p
  -- p is empty:
  | mempty == p = CompletionTree ct
  -- p is a key in the map ct:
  | isJust one = fromJust one
  -- a substring of p is a key in ct:
  | isJust remaining = uncurry update $ fromJust remaining
  -- p is a substring of a key in ct:
  | otherwise = CompletionTree $ M.mapKeys fromJust
                               $ M.filterWithKey (const . isJust)
                               $ M.mapKeys (stripPrefix p) ct
  where
    one = M.lookup p ct
    remaining = listToMaybe . catMaybes $
      map (\p' -> (,fromJust $ stripPrefix p' p) <$> M.lookup p' ct) (tail $ LL.inits p)

-- | Like 'update' but with only one item.
updateOne :: (Ord a, ListLike a i, Eq i) => CompletionTree a -> i -> CompletionTree a
updateOne (CompletionTree ct) p
  -- (LL.singleton p) is a key in ct:
  | isJust one = fromJust one
  -- p is the head of a key in ct:
  | otherwise = CompletionTree $ M.mapKeys fromJust
                               $ M.filterWithKey (const . isJust)
                               $ M.mapKeys (stripPrefixItem p) ct
  where
    one = M.lookup (LL.singleton p) ct
    stripPrefixItem item full
      | LL.null full = Nothing
      | LL.head full == item = Just $ LL.tail full
      | otherwise = Nothing

-- | Converts a CompletionTree to a list of completions.
--
-- prop> toList . fromList = id
toList :: ListLike a i => CompletionTree a -> [a]
toList (CompletionTree ct)
  | M.null ct = [mempty]
  | otherwise = concat $ M.elems $ M.mapWithKey (\k v -> map (k `LL.append`) $ toList v) ct

-- | For debugging purposes.
--
-- Example:
--
-- >>> putStrLn $ pretty $ fromList ["put", "putStr", "putStrLn"]
-- ["put"[""|"Str"[""|"Ln"]]]
pretty :: Show a => CompletionTree a -> String
pretty (CompletionTree ct)
  | M.null ct = ""
  | otherwise = "[" ++ intercalate "|" (M.elems (M.mapWithKey (\k v -> shows k (pretty v)) ct)) ++ "]"
