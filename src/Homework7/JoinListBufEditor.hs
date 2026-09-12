module Homework7.JoinListBufEditor where

import Homework7.Buffer (fromString)
import Homework7.Editor (editor, runEditor)
import Homework7.JoinListBuffer (JoinListBuffer)

main :: IO ()
main =
  runEditor editor welcome
 where
  welcome =
    fromString
      ( unlines
          [ "This buffer is for notes you don't want to save, and for"
          , "evaluation of steam valve coefficients."
          , "To load a different file, type the character L followed"
          , "by the name of the file."
          ]
      ) ::
      JoinListBuffer
