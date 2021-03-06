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
      getConditionDiff [HandEmpty, IsTop A True, On A Table, On B (Object A)] [HandEmpty, IsTop A True, On B Table, On A (Object B)] `shouldBe` (2, [On A Table, On B (Object A)])

  describe "mergeNodes " $ do
    it "merges two nodeInfos of first and third argument except the element in second argument" $ do
      let n1 = buildTestNodeInfo 1 [HandEmpty]
          n2 = buildTestNodeInfo 2 [HandHas A]
          n3 = buildTestNodeInfo 3 [IsTop A True]
          n4 = buildTestNodeInfo 1 [HandHas A]
          n5 = buildTestNodeInfo 5 [On A Table]
      mergeNodes [n1, n2] [n3] [n3, n4, n5] `shouldBe` [n1, n4, n5]

  describe "getActionCandidates" $ do
    let extractActionTypes nodeInfo = map actionType $ getActionCandidates buildDomain nodeInfo
    it "gets []" $ do
      let n = buildTestNodeInfo 0 []
      extractActionTypes n `shouldBe` []
    it "gets [Pickup A]" $ do
      let n = buildTestNodeInfo 0 [HandHas A, IsTop A False]
      extractActionTypes n `shouldBe` [Pickup A]
    it "gets [Pickup A, Unstack A B]" $ do
      let n = buildTestNodeInfo 0 [HandHas A, IsTop A False, IsTop B True]
      extractActionTypes n `shouldBe` [Pickup A, Unstack A B]
    it "gets [Putdown A, Putdown B]" $ do
      let n = buildTestNodeInfo 0 [HandEmpty, IsTop A True, IsTop B True, On A Table, On B Table]
      extractActionTypes n `shouldBe` [Putdown A, Putdown B]

buildTestNodeInfo score condition = NodeInfo score 0 [] 0 condition NoAction NoNodeInfo

