module Test where

type Tests = (String, [Bool])
assert :: Bool -> Tests -> IO Tests
assert x (d, bs) =
  do putStr (if x then "." else "F")
     return $ (d, bs ++ [x])
describe :: String -> IO Tests
describe desc = return (desc, []) 
render :: Tests -> IO ()
render (d, bs) =
  let passed = length (filter (==True) bs)
      failed = length (filter (==False) bs)
  in if passed == length bs
     then putStrLn $ "\nAll " ++ (show passed) ++ " tests passed for `" ++ d ++ "`."
     else putStrLn $ "\n" ++ (show failed) ++ " tests failed for `" ++ d ++ "`."
test :: IO Tests -> IO ()
test ts = ts >>= render

assertEq :: Eq a => a -> a -> Tests -> IO Tests
assertEq a b = assert $ a == b

