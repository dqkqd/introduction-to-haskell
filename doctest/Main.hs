import Test.DocTest

main :: IO ()
main =
  doctest
    [ "src/Homework1.hs"
    , "src/Homework2/LogAnalysis.hs"
    , "src/Homework2/Log.hs"
    , "src/Homework3.hs"
    , "src/Homework4.hs"
    , "src/Homework5/Calc.hs"
    , "src/Homework5/ExprT.hs"
    , "src/Homework5/Parser.hs"
    , "src/Homework5/StackVM.hs"
    ]
