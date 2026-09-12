module Main where

import Data.Tree

import Data.List (sort)
import Homework8.Employee (Employee (empName), GuestList (GL))
import Homework8.Party (maxFun)

main :: IO ()
main =
  do
    content <- readFile "data/company.txt"
    let company = read content :: Tree Employee
    let (GL employees totalFun) = maxFun company
    let employeeNames = sort (map empName employees)
    putStrLn ("Total fun: " ++ show totalFun)
      >> putStrLn (unlines employeeNames)
