import Test.Hspec

import BlockWorldLib

main :: IO ()
main = hspec $ do
  describe "getConditionDiff" $ do
    it "returns diff and diff count of the two lists as tuple (both are empty list)" $ do
      getConditionDiff [] [] `shouldBe` (0, [])
    it "returns diff and diff count of the two lists as tuple (both have same entry)" $ do
      getConditionDiff [HandEmpty] [HandEmpty] `shouldBe` (0, [])
    it "returns diff and diff count of the two lists as tuple" $ do
      getConditionDiff [HandEmpty, IsTop A True] [HandEmpty, On A Table, IsTop A True] `shouldBe` (1, [On A Table])

  describe "mergeNodes " $ do
    it "merges two nodeInfos of first and third argument except the element in second argument" $ do
      let n1 = buildTestNodeInfo 1 [HandEmpty]
          n2 = buildTestNodeInfo 2 [HandHas A]
          n3 = buildTestNodeInfo 3 [IsTop A True]
          n4 = buildTestNodeInfo 1 [HandHas A]
          n5 = buildTestNodeInfo 5 [On A Table]
      mergeNodes [n1, n2] [n3] [n3, n4, n5] `shouldBe` [n1, n4, n5]
  
buildTestNodeInfo score condition = NodeInfo condition NoAction NoNodeInfo score 0 [] 0
      
