module Calculator where

data AST a =
    X
  | Const  a
  | Add   (AST a) (AST a)
  | Sub   (AST a) (AST a)
  | Mul   (AST a) (AST a)
  | Div   (AST a) (AST a)
  | Pow   (AST a) a
  | Pow_x (AST a) (AST a)
  | Sin (AST a)
  | Cos (AST a)
  | Exp (AST a)
  | Ch (AST a)
  | Sh (AST a)
  deriving (Read, Show)
  
-------------------------------------------------------------------------------
-- "mkFunc": Generating an executable lambda from an AST:                    --
-------------------------------------------------------------------------------
mkFunc :: (Floating a) => AST a -> (a -> a)   -- , Integral a, RealFrac a
mkFunc X             = \x -> x
mkFunc (Const c)     = \_ -> c
mkFunc (Add   e1 e2) = \x -> (mkFunc e1) x + (mkFunc e2) x
mkFunc (Sub   e1 e2) = \x -> (mkFunc e1) x - (mkFunc e2) x
mkFunc (Mul   e1 e2) = \x -> (mkFunc e1) x * (mkFunc e2) x
mkFunc (Div   e1 e2) = \x -> (mkFunc e1) x / (mkFunc e2) x
mkFunc (Pow   e1 n)  = \x -> ((mkFunc e1) x) ** n 
mkFunc (Pow_x e1 n)  = \x -> ((mkFunc e1) x) ** ((mkFunc n) x) 
mkFunc (Sin   e1)    = \x -> sin ((mkFunc e1) x)
mkFunc (Cos   e1)    = \x -> cos ((mkFunc e1) x)
mkFunc (Exp e1)      = \x -> exp ((mkFunc e1) x)
mkFunc (Ch e1)       = \x -> (exp ((mkFunc e1) x) + exp ((mkFunc e1) (-x))) / 2
mkFunc (Sh e1)       = \x -> (exp ((mkFunc e1) x) - exp ((mkFunc e1) (-x))) / 2

f_ast = Pow (Add X (Const 1.0)) 3.5 -- (x+1)^3.5
f = mkFunc f_ast

tgx_ast = Div (Sin X) (Cos X)
tgx = mkFunc tgx_ast

-- hyperbolic tang
tghx_ast = Div (Sh X) (Ch X)
tghx = mkFunc tghx_ast


-- ====================
main :: IO()
main = do
  let f_ast0 = Pow (Add X (Const 1.0)) 3.5 -- (x+1)^3.5
  let x0     = 3
  let func0 = mkFunc f_ast0
  let res0  = func0 x0   -- (mkFunc f_ast0) x0
  putStr $ "\t f(x) = " -- ++ (show f_ast0)
  print f_ast0
  putStrLn $ " f(" ++ (show x0) ++ ") = " ++ (show res0)
  putStrLn ""
  -- 
  putStrLn $ "Input function { for example: Pow (Add X (Const 1.0)) 3.0 }"
  putStr "f(X) = " 
  exprS <- getLine -- "Pow (Add X (Const 1.0)) 3.0" -- (x+1)^3
  putStr "x = " -- print "x = "
  xS    <- getLine
  let f_ast  = read   exprS
  let x    = read   xS
  let func = mkFunc f_ast
  let res  = func x  
  putStrLn $ " f(" ++ (show x) ++ ") = " ++ (show res)