import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import qualified Yi.CompletionTree as CT
import           Data.List         (nub, sort)
import qualified Data.Map          as M

main :: IO ()
main = defaultMain . tests =<< specTests

tests :: TestTree -> TestTree
tests specTests' = testGroup "CompletionTree Tests" [properties, specTests']

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  -- It's nub . sort because CompletionTree uses a Map internally, so duplicates and ordering are lost
  [ SC.testProperty "CT.toList . CT.fromList == nub . sort" $
      -- I'm using String because the test suites can't handle Text well.
      \list -> CT.toList (CT.fromList list) == sort (nub (list :: [String]))
  , SC.testProperty "update (fromList [a]) a == fromList [mempty] (a is a non-empty string)" $
      \string -> null string || CT.update (CT.fromList [string :: String]) string == CT.fromList [mempty]
  , SC.testProperty "\"\" `elem` update (fromList [a,...]) a" $
      \listOfStrings -> null listOfStrings || null (head listOfStrings) || "" `elem` CT.toList (CT.update (CT.fromList listOfStrings) (head listOfStrings))
  , SC.testProperty "complete (fromList [a]) == (a, fromList [\"\"])" $
      \string -> CT.complete (CT.fromList [string]) == (string,CT.fromList [""])
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "CT.toList . CT.fromList == nub . sort" $
      \list -> CT.toList (CT.fromList list) == sort (nub (list :: [String]))
  , QC.testProperty "update (fromList [a]) a == fromList [mempty] (a is a non-empty string)" $
      \string -> null string || CT.update (CT.fromList [string :: String]) string == CT.fromList [mempty]
  , QC.testProperty "\"\" `elem` update (fromList [a,...]) a" $
      \listOfStrings -> null listOfStrings || null (head listOfStrings) || "" `elem` CT.toList (CT.update (CT.fromList listOfStrings) (head listOfStrings))
  , QC.testProperty "complete (fromList [a]) == (a, fromList [\"\"])" $
      \string -> CT.complete (CT.fromList [string]) == (string,CT.fromList [""])
  ]

specTests :: IO TestTree
specTests = testSpec "Spec tests" $
  describe "CompletionTree" $ do
    describe "fromList" $ do
      it "returns an empty CompletionTree when given an empty list" $
        CT.fromList [] `shouldBe` (mempty :: CT.CompletionTree String)
      it "returns a map with one key when given a list with one item" $
        CT.fromList ["a"] `shouldBe` CT.CompletionTree (M.fromList [("a",mempty)])
      it "groups elements with the same prefix" $
        CT.fromList ["aa","ab"] `shouldBe` CT.CompletionTree (M.fromList [("a",CT.CompletionTree $ M.fromList [("a",mempty),("b",mempty)])])
    -- toList is covered by the SmallCheck and QuickCheck
    describe "update" $ do
      it "strips its argument from a matching key" $
        CT.update (CT.fromList ["abc"]) "a" `shouldBe` CT.fromList ["bc"]
      it "descends the tree if a substring of its input is found in the CompletionTree" $
        CT.update (CT.fromList ["put","putStr"]) "putS" `shouldBe` CT.fromList ["tr"]
      it "returns an empty list if it can't find a matching key" $
        CT.update (CT.fromList ["put"]) "list" `shouldBe` CT.fromList []
    describe "complete" $ do
      it "Returns the common prefix" $
        CT.complete (CT.fromList ["put","putStr","putStrLn"]) == ("put",CT.fromList ["","Str","StrLn"])
      it "Returns an empty string if there's no common prefix" $
        CT.complete (CT.fromList ["put","putStr","abc"]) == ("",CT.fromList ["put","putStr","abc"])
