module SequentTree (Expr, SeqTree) where

data Expr = Var String
          | Not Expr
          | Conj Expr Expr
          | Disj Expr Expr
          | Impl Expr Expr
          deriving (Show, Eq)

type Sequent = ([Expr], [Expr])

data SeqTree = Empty
             | Node [Sequent] SeqTree

instance Show SeqTree where
  show Empty = ""
  show (Node [x] tr) = show x ++ show tr
  show (Node [x,y] tr) = show x ++ show y ++ show tr
  show _ = ""

solve :: Sequent -> SeqTree
solve ([], []) = Empty
solve (a, b) = let
                 ((l,r):xs) = simplify (a, b)
                 list = simplify (a, b)
                 
               in solve l r

-- Simplify expression sets. Makes one step --
simplify :: Sequent -> [Sequent]

simplify (Impl a b:xs, ys) = [(b:xs, ys), (xs, a:ys)]
simplify (xs, Impl a b:ys) = [(a:xs, b:ys)]

simplify (Conj a b:xs, ys) = [(a:b:xs, ys)]
simplify (xs, Conj a b:ys) = [(xs, a:ys), (xs, b:ys)]

simplify (Disj a b:xs, ys) = [(a:xs, ys), (b:xs, ys)]
simplify (xs, Disj a b:ys) = [(xs, a:b:ys)]

simplify (Not a:xs, ys) = [(xs, a:ys)]
simplify (xs, Not a:ys) = [(a:xs, ys)]

simplify (x@(Var _:xs), ys) = [(x:xs, ys)]
simplify (xs, y@(Var _):ys) = [(xs, y:ys)]

simplify ([], []) = []


-- Check expressions to be Var or nothing
checkBoth :: [Expr] -> [Expr] -> Bool
checkBoth a b = check a && check b

check :: [Expr] -> Bool
check [] = True
check (x:xs) = (x == Var _) && check xs

-- Tests --
--test = simplify [Not (Var "a")] [Not (Var "b")]
