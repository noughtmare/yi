{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.UI.Completion
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Completion dropdown menu.

module Yi.UI.Completion where

import           Control.Lens                   ((^.))
import           Data.Text                      (Text)
import           Yi.Editor                      (Editor (..), completionsA)

-- | A CompletionEntryDescr describes the properties of a UI tab independent of
-- the particular GUI in use.
data CompletionEntryDescr = CompletionEntryDescr
    { entryText :: Text
    , entryInFocus :: Bool
    } deriving (Show, Eq)

type CompletionsDescr = (Text, [CompletionEntryDescr])

completionsDescr :: Editor -> CompletionsDescr
completionsDescr editor = mapSnd (uncurry CompletionEntryDescr) $ editor ^. completionsA
  where mapSnd f (a,b) = (a, map f b)