{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.IReader
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines a list type and operations on it; it further
-- provides functions which write in and out the list. The goal is to
-- make it easy for the user to store a large number of text buffers
-- and cycle among them, making edits as she goes. The idea is
-- inspired by \"incremental reading\", see
-- <http://en.wikipedia.org/wiki/Incremental_reading>.

module Yi.IReader where

import           Control.Exception          (SomeException, catch)
import           Control.Monad              (void)
import           Data.Binary                (Binary, decode, encodeFile)
import qualified Data.ByteString.Char8      as B (ByteString, pack, readFile, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (fromChunks)
import           Data.Default               (Default, def)
import           Data.Sequence              as S (Seq, ViewL (EmptyL, (:<)),
                                                  ViewR ((:>)), empty, length,
                                                  null, splitAt, viewl, viewr,
                                                  (<|), (><), (|>))
import           Data.Typeable              (Typeable)

class Monad m => Yi m where
  getDB :: m ArticleDB
  putDB :: ArticleDB -> m ()
  getCurrentBufferContents :: m String
  setCurrentBufferContents :: String -> m ()
  io :: IO a -> m a
  getArticleDbFilename :: m FilePath

-- Reference implementation:
--
-- instance YiVariable ArticleDB
--
-- instance Yi YiM where
--   getDB = withCurrentBuffer getBufferDyn
--   putDB db = withCurrentBuffer $ putBufferDyn db
--   getCurrentBufferContents = toString $ withCurrentBuffer elemsB
--   setCurrentBufferContents str = do
--     withCurrentBuffer $ replaceBufferContent $ fromString str
--     topB -- replaceBufferContents moves us to bottom?
--   io = io
--   getArticleDbFilename = getConfigPath "articles.db"

-- | TODO: Why 'B.ByteString'?
type Article = B.ByteString

newtype ArticleDB = ADB { unADB :: Seq Article }
  deriving (Typeable, Binary)

instance Default ArticleDB where
    def = ADB S.empty

-- | Take an 'ArticleDB', and return the first 'Article' and an
-- ArticleDB - *without* that article.
split :: ArticleDB -> (Article, ArticleDB)
split (ADB adb) = case viewl adb of
  EmptyL -> (B.pack "", def)
  (a :< b) -> (a, ADB b)

-- | Get the first article in the list. We use the list to express
-- relative priority; the first is the most, the last least. We then
-- just cycle through - every article gets equal time.
getLatestArticle :: ArticleDB -> Article
getLatestArticle = fst . split -- we only want the article

-- | We remove the old first article, and we stick it on the end of the
-- list using the presumably modified version.
removeSetLast :: ArticleDB -> Article -> ArticleDB
removeSetLast adb old = ADB (unADB (snd (split adb)) S.|> old)

-- we move the last entry to the entry 'length `div` n'from the
-- beginning; so 'shift 1' would do nothing (eg. the last index is 50,
-- 50 `div` 1 == 50, so the item would be moved to where it is) 'shift
-- 2' will move it to the middle of the list, though; last index = 50,
-- then 50 `div` 2 will shift the item to index 25, and so on down to
-- 50 `div` 50 - the head of the list/Seq.
shift :: Int ->ArticleDB -> ArticleDB
shift n adb = if n < 2 || lst < 2 then adb else ADB $ (r S.|> lastentry) >< s'
  where lst = S.length (unADB adb) - 1
        (r,s) = S.splitAt (lst `div` n) (unADB adb)
        (s' :> lastentry) = S.viewr s

-- | Insert a new article with top priority (that is, at the front of the list).
insertArticle :: ArticleDB -> Article -> ArticleDB
insertArticle (ADB adb) new = ADB (new S.<| adb)

-- | Serialize given 'ArticleDB' out.
writeDB :: Yi m => ArticleDB -> m ()
writeDB adb = do
  file <- getArticleDbFilename
  io $ void $ encodeFile file adb

-- | Read in database from 'getArticleDbFilename' and then parse it
-- into an 'ArticleDB'.
readDB :: Yi m => m ArticleDB
readDB = getArticleDbFilename >>= \file -> io (r file `catch` returnDefault)
  where r = fmap (decode . BL.fromChunks . return) . B.readFile
        -- We read in with strict bytestrings to guarantee the file is
        -- closed, and then we convert it to the lazy bytestring
        -- data.binary expects. This is inefficient, but alas...
        returnDefault (_ :: SomeException) = return def

-- | Returns the database as it exists on the disk, and the current Yi
-- buffer contents. Note that the Default typeclass gives us an empty
-- Seq. So first we try the buffer state in the hope we can avoid a
-- very expensive read from disk, and if we find nothing (that is, if
-- we get an empty Seq), only then do we call 'readDB'.
oldDbNewArticle :: Yi m => m (ArticleDB, Article)
oldDbNewArticle = do
  saveddb <- getDB
  newarticle <- B.pack <$> getCurrentBufferContents
  if not $ S.null (unADB saveddb)
    then return (saveddb, newarticle)
    else readDB >>= \olddb -> return (olddb, newarticle)

-- | Given an 'ArticleDB', dump the scheduled article into the buffer
-- (replacing previous contents).
setDisplayedArticle :: Yi m => ArticleDB -> m ()
setDisplayedArticle newdb = do
  let next = getLatestArticle newdb
  setCurrentBufferContents $ B.unpack next
  putDB newdb

-- | Go to next one. This ignores the buffer, but it doesn't remove
-- anything from the database. However, the ordering does change.
nextArticle :: Yi m => m ()
nextArticle = do
  (oldb,_) <- oldDbNewArticle
  -- Ignore buffer, just set the first article last
  let newdb = removeSetLast oldb (getLatestArticle oldb)
  writeDB newdb
  setDisplayedArticle newdb

-- | Delete current article (the article as in the database), and go
-- to next one.
deleteAndNextArticle :: Yi m => m ()
deleteAndNextArticle = do
  (oldb,_) <- oldDbNewArticle -- throw away changes
  let ndb = ADB $ case viewl (unADB oldb) of     -- drop 1st article
        EmptyL -> empty
        (_ :< b) -> b
  writeDB ndb
  setDisplayedArticle ndb

-- | The main action. We fetch the old database, we fetch the modified
-- article from the buffer, then we call the function 'updateSetLast'
-- which removes the first article and pushes our modified article to
-- the end of the list.
saveAndNextArticle :: Yi m => Int -> m ()
saveAndNextArticle n = do
  (oldb,newa) <- oldDbNewArticle
  let newdb = shift n $ removeSetLast oldb newa
  writeDB newdb
  setDisplayedArticle newdb

-- | Assume the buffer is an entirely new article just imported this
-- second, and save it. We don't want to use 'updateSetLast' since
-- that will erase an article.
saveAsNewArticle :: Yi m => m ()
saveAsNewArticle = do
  oldb <- readDB -- make sure we read from disk - we aren't in iread-mode!
  (_,newa) <- oldDbNewArticle -- we ignore the fst - the Default is 'empty'
  let newdb = insertArticle oldb newa
  writeDB newdb
