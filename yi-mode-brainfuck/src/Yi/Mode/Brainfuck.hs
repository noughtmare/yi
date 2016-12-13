{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Yi.Mode.Brainfuck where

import Debug.Trace

import Control.Monad (void)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as M
import Control.Monad (void)
import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.String
import Control.Applicative (many, Alternative (..))
import Data.String (IsString (..))
import Data.Word

import Yi.Buffer.Misc
import Yi.Mode.Common (anyExtension)
import Lens.Micro.Platform
import Yi.Types (Mode (..))

import Yi.Editor
import Yi.Style
import Yi.Syntax

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B

data Stmt = Next | Prev | Inc | Dec | Out | In | Loop | End | Comment
  deriving (Show)

parser :: Parser [(Point, Stmt)]
parser = many stmt <* eof

-- Note: this parser should theoretically parse all possible strings
stmt :: Parser (Point, Stmt)
stmt = (char '>' $> Next)
   <|> (char '<' $> Prev)
   <|> (char '+' $> Inc )
   <|> (char '-' $> Dec )
   <|> (char '.' $> Out )
   <|> (char ',' $> In  )
   <|> (char '[' $> Loop)
   <|> (char ']' $> End )
   <|> (anyChar $> Comment)

{- I had some fun writing a brainfuck interpreter:

-- I <3 IsString instances
instance IsString Stmt where
  fromString = f . parse parser ""
    where f (Right a) = a
          f (Left err) = error $ parseErrorPretty err

type BFState a = (Int,IOVector a)

interpret :: Stmt -> IO ()
interpret = interpretWithArrayLength 30000

interpretWithArrayLength :: Int -> Stmt -> IO ()
interpretWithArrayLength n stmt = void . genericInterpret stmt 
  =<< ((0,) <$> M.replicate n (0 :: Word8))

genericInterpret :: Integral a => Stmt -> BFState a -> IO (BFState a)
genericInterpret Next (i, l) = return (i + 1, l)
genericInterpret Prev (i, l) = return (i - 1, l)
genericInterpret Inc  (i, l) = M.modify l (+ 1) i >> return (i, l)
genericInterpret Dec  (i, l) = M.modify l (+ (-1)) i >> return (i, l)
genericInterpret Out  (i, l) = (M.read l i >>= putChar . toEnum . fromIntegral) >> return (i, l)
genericInterpret In   (i, l) = (M.write l i . fromIntegral . fromEnum =<< getChar) >> return (i, l)
genericInterpret (Seq []) st = return st
genericInterpret (Seq (x:xs)) st = genericInterpret x st >>= genericInterpret (Seq xs)
genericInterpret (Loop stmt)  st@(i, l) = do
  val <- M.read l i 
  if val == 0 then return st else genericInterpret stmt st >>= genericInterpret (Loop stmt)
-}

-- FIXME: WIP
brainfuckMode :: Mode [Span Stmt]
brainfuckMode = emptyMode
  & modeAppliesA .~ anyExtension ["bf", "brainfuck"]
  & modeNameA .~ "brainfuck"
  & modeGetStrokesA .~ getStrokes
  & modeHLA .~ ExtHL bfHighlighter
  where
    bfHighlighter :: Highlighter [Span Stmt] [Span Stmt]
    bfHighlighter = SynHL
      []
      run
      const
      (flip . const)
      where
        run :: Scanner Point Char -> Point -> [Span Stmt] -> [Span Stmt]
        run sc p prev = 


-- FIXME: WIP
getStrokes :: Stmt -> Point -> Point -> Point -> [Stroke]
getStrokes tree _ _ _ = []