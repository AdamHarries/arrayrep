module Main where

import Lib
import Data.List

main :: IO ()
main = do
  let fixRRInt = ArraySC (ArraySC LInt)
  putStrLn $ "ArraySC (ArraySC LInt): "
  putStrLn $ " -- " ++ (show fixRRInt)
  putStrLn $ " -- lower: " ++ (show $ lower fixRRInt)
  putStrLn $ " -- flatten: " ++ (show $ flatten $ lower fixRRInt)
  putStrLn ""

  let varvarFloat = Array (Array LFloat)
  putStrLn $ "Array (Array LFloat): "
  putStrLn $ " -- " ++ (show varvarFloat)
  putStrLn $ " -- lower: " ++ (show $ lower varvarFloat)
  putStrLn $ " -- flatten: " ++ (show $ flatten $ lower varvarFloat)
  putStrLn ""

  let sparseMatrixIx = ArraySC ( Array LInt)
  putStrLn $ "(Sparse matrix indices use case)" 
  putStrLn $ "ArraySC (Array LInt): "
  putStrLn $ " -- " ++ (show sparseMatrixIx)
  putStrLn $ " -- lower: " ++ (show $ lower sparseMatrixIx)
  putStrLn $ " -- flatten: " ++ (show $ flatten $ lower sparseMatrixIx)
  putStrLn ""

-- lower a lift type to a recursive memory representation
lower :: LiftType -> RecRepr
lower LInt = CLInt
lower LFloat = CLFloat
lower (ArraySC t) = RecRepr { size = CSConst, cap = CSConst, etype = (lower t)}
lower (ArrayC  t) = RecRepr { size = CSVar  , cap = CSConst, etype = (lower t)}
lower (Array   t) = RecRepr { size = CSVar  , cap = CSVar  , etype = (lower t)}

-- flatten a recursive representation to a flat one
-- this is the hard bit...
flatten :: RecRepr -> FlatRepr
flatten RecRepr {size = s, cap = c, etype = t} = case t of 
    CLInt -> FlatRepr {sizes = [s], caps = [c], arrtype = CArr CInt}
    CLFloat -> FlatRepr {sizes = [s], caps = [c], arrtype = CArr CFloat}
    rr -> let FlatRepr {sizes = ss, caps = cc, arrtype = at} = flatten rr in 
      FlatRepr {
        sizes = s : (map CSArr ss),
        caps = c : (map CSArr cc), 
        arrtype = (CArr at)
      }



-- Define the recursive type of lift arrays
data LiftType = LInt | LFloat | ArraySC LiftType | ArrayC LiftType | Array LiftType

instance Show LiftType where
  show LInt = "I"
  show LFloat = "F"
  show (ArraySC t) = "ArrayWSWC[" ++ (show t) ++ "]"
  show (ArrayC t) = "ArrayWC[" ++ (show t) ++ "]"
  show (Array t) = "Array[" ++ (show t) ++ "]"

-- Define the general type for capacity and size values
-- They are either variable or pre-kernel constant
-- Variable changes meaning for both size and capacity.
-- For size, it means that it might change at kernel runtime
-- for capacity it means that an *array* of capacities might have different values
data CSType = CSVar | CSConst | CSArr CSType
instance Show CSType where
  show CSVar = "var" 
  show CSConst = "const"
  show (CSArr t) = "[" ++ (show t) ++ "]"

condP :: String -> CSType -> String  
condP s CSVar = s ++ ": " ++ (show CSVar) ++ " | " 
condP _ CSConst = ""
condP s csar = s ++ ": " ++ (show csar) ++ " | " 

-- a recursive "java like" representation essentially identical to the 
-- lift representation, but with the size and capacity as values
data RecRepr = RecRepr {
  size  :: CSType,
  cap   :: CSType,
  etype :: RecRepr
} | CLInt | CLFloat

instance Show RecRepr where
  show CLInt = "I"
  show CLFloat = "F"
  show rc = "[ " ++ (condP "c" (cap rc)) ++ (condP "s" (size rc)) ++ (show $ etype rc) ++ " ]"

-- and finally, the flat representation that we want, aka structure of arrays form
data EType = CInt | CFloat | CArr EType
instance Show EType where
  show CInt = "I"
  show CFloat = "F"
  show (CArr t) = "[" ++ (show t) ++ "]"

data FlatRepr = FlatRepr {
  sizes :: [CSType], -- this can be recursive.
  caps :: [CSType], 
  arrtype :: EType
}

instance Show FlatRepr where
  show FlatRepr {caps = cs, sizes = ss, arrtype = elems} = csizes ++ (show elems)
    where 
      csizes = (zip cs ss >>= ppair)
      ppair (c, s) = (condP "c" c) ++ (condP "s" c)








