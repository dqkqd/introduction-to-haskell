import Test.Hspec
import TestHomework1 qualified
import TestHomework2 qualified
import TestHomework3 qualified
import TestHomework4 qualified
import TestHomework5 qualified
import TestHomework6 qualified

main :: IO ()
main = hspec $ do
  TestHomework1.spec
  TestHomework2.spec
  TestHomework3.spec
  TestHomework4.spec
  TestHomework5.spec
  TestHomework6.spec
