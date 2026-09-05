import Test.Hspec
import TestHomework1 qualified
import TestHomework2 qualified
import TestHomework3 qualified

main :: IO ()
main = hspec $ do
  TestHomework1.spec
  TestHomework2.spec
  TestHomework3.spec
