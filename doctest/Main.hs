import Test.DocTest

main :: IO ()
main =
  doctest
    [ "src/Homework1.hs"
    , "src/Homework2/LogAnalysis.hs"
    , "src/Homework2/Log.hs"
    ]
