import Test.Hspec
import TestHomework1 qualified
import TestHomework10 qualified
import TestHomework11 qualified
import TestHomework2 qualified
import TestHomework3 qualified
import TestHomework4 qualified
import TestHomework5 qualified
import TestHomework6 qualified
import TestHomework7 qualified
import TestHomework8 qualified

main :: IO ()
main = hspec $ do
  TestHomework1.spec
  TestHomework2.spec
  TestHomework3.spec
  TestHomework4.spec
  TestHomework5.spec
  TestHomework6.spec
  TestHomework7.spec
  TestHomework8.spec
  TestHomework10.spec
  TestHomework11.spec
