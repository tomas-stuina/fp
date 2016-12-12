import Test.Hspec
import MExp
import HttpClient
import Game

main :: IO ()
main = hspec $ do
   describe "Verify that winner is correct." $ do
     it "Should return x" $ do
       winner [(Move 0 0 "x"),(Move 2 0 "o"),(Move 1 1 "x"),(Move 0 2 "o"),(Move 2 2 "x")] 
        `shouldBe` Just "x"
   describe "Verify that decoding is correct." $ do
     it "Should return [(Move 1 1 \"x\")]" $ do
       getMovesFromMExp "l[m[\"x\"; 1; \"y\"; 1; \"v\"; \"x\"]]"
        `shouldBe` [(Move 1 1 "x")]
   describe "Verify that encoding is correct." $ do
     it "Should return l[m[\"x\"; 1; \"y\"; 1; \"v\"; \"x\"]]" $ do
       movesToMExp [(Move 1 1 "x")]
        `shouldBe` "l[m[\"x\"; 1; \"y\"; 1; \"v\"; \"x\"]]"
   describe "Verify coordsToPos conversion." $ do
     it "Should return position 5." $ do
       coordsToPos (1, 1)
        `shouldBe` 4
   describe "Verify posToCoords conversion." $ do
     it "Should return position (1, 1)." $ do
       posToCoords 4
        `shouldBe` (1, 1)


