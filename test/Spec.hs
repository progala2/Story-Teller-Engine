module Main where
import Test.HUnit

main :: IO ()
main = (runTestTT $ TestList [TestLabel "test1" test1]) >> return ()


test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1, 3))