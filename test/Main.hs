import Test.Hspec
import TestHomework1 qualified

main :: IO ()
main = hspec $ do
  TestHomework1.spec
