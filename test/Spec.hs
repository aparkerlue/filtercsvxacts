import Lib
import Test.HUnit

parseArgsTest :: Test
parseArgsTest = TestCase (assertEqual
                           "call with valid arguments"
                           (Left ["a.csv"])
                           (parseArgs ["a.csv"]))

main :: IO Counts
main = runTestTT $ TestList [TestLabel "parseArgsTest" parseArgsTest]
