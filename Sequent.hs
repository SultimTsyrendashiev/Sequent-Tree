module SequentTree (
  Expr,
  SeqTree,
  createTree,
  hasCE,
  isValid) where

data Expr = Var String
          | Not Expr
          | Conj Expr Expr
          | Disj Expr Expr
          | Impl Expr Expr
          deriving Eq

instance Show Expr where
  show (Var a) = a
  show (Not (Var a)) = "-" ++ a
  show (Not e) = "-" ++ "(" ++  show e ++ ")"

  show (Conj (Var a1) (Var a2)) = a1 ++ "&" ++ a2
  show (Conj e1 (Var a2)) = "(" ++ show e1 ++ ")&" ++ a2
  show (Conj (Var a1) e2) = a1 ++ "&(" ++ show e2 ++ ")"
  show (Conj e1 e2) = "(" ++ show e1 ++ ")&(" ++ show e2 ++ ")"

  show (Disj (Var a1) (Var a2)) = a1 ++ "|" ++ a2
  show (Disj e1 (Var a2)) = "(" ++ show e1 ++ ")|" ++ a2
  show (Disj (Var a1) e2) = a1 ++ "|(" ++ show e2 ++ ")"
  show (Disj e1 e2) = "(" ++ show e1 ++ ")|(" ++ show e2 ++ ")"

  show (Impl (Var a1) (Var a2)) = a1 ++ " -> " ++ a2
  show (Impl e1 (Var a2)) = "(" ++ show e1 ++ ") -> " ++ a2
  show (Impl (Var a1) e2) = a1 ++ " -> (" ++ show e2 ++ ")"
  show (Impl e1 e2) = "(" ++ show e1 ++ ") -> (" ++ show e2 ++ ")"

type Sequent = ([Expr], [Expr])

data SeqTree = Empty
             | Node Sequent [SeqTree]

instance Show SeqTree where
  show Empty = ""
  show (Node (xs, ys) [Empty]) = show xs ++ " |- " ++ show ys
  show (Node (xs, ys) tr) = concatMap showSpaced tr ++ "\n-----------\n" ++ show xs ++ " |- " ++ show ys

-- Show with spaces after element --
showSpaced :: Show a => a -> String
showSpaced a = show a ++ "       "

-- Check sequent for validness --
isValid :: Sequent -> Maybe SeqTree
isValid s = let (has, tree) = hasCE s
            in if has
               then Just tree
               else Nothing

-- Check sequent for counter examples --
hasCE :: Sequent -> (Bool, SeqTree)
hasCE s = let tree = createTree s
          in (treeHasCE tree, tree)

-- Check tree for counter examples --
treeHasCE :: SeqTree -> Bool
treeHasCE Empty = True
-- if node is a leaf
treeHasCE (Node (xs,ys) [Empty]) = not $ haveSame xs ys -- if there are no same vars on both sides => CE exist
treeHasCE (Node _ tree) = foldr ((&&) . treeHasCE) True tree

haveSame :: [Expr] -> [Expr] -> Bool
haveSame [] _ = False
haveSame (x:xs) ys | x `elem` ys = True
                   | otherwise   = haveSame xs ys

-- Create sequent tree --
createTree :: Sequent -> SeqTree
createTree ([], []) = Empty
createTree s = let list = procSeq s
          in if null list
             then Node s [Empty]
             else Node s $ map createTree list

-- Returns [] if all members are variables
procSeq :: Sequent -> [Sequent]
procSeq ([], []) = []
procSeq s = if checkSeq s
            then []
            else map reduceSame (simplify s)

-- Reduce same variables in antecedent or succedent
reduceSame :: Sequent -> Sequent
reduceSame (xs, ys) = (reduceSameExps xs, reduceSameExps ys)

reduceSameExps :: [Expr] -> [Expr]
reduceSameExps [] = []
reduceSameExps (x:xs) | x `elem` xs = reduceSameExps xs
                      | otherwise   = x : reduceSameExps xs

-- Simplify sequent. Makes one step --
simplify :: Sequent -> [Sequent]

simplify (Impl a b:xs, ys) = [(b:xs, ys), (xs, a:ys)]
simplify (xs, Impl a b:ys) = [(a:xs, b:ys)]

simplify (Conj a b:xs, ys) = [(a:b:xs, ys)]
simplify (xs, Conj a b:ys) = [(xs, a:ys), (xs, b:ys)]

simplify (Disj a b:xs, ys) = [(a:xs, ys), (b:xs, ys)]
simplify (xs, Disj a b:ys) = [(xs, a:b:ys)]

simplify (Not a:xs, ys) = [(xs, a:ys)]
simplify (xs, Not a:ys) = [(a:xs, ys)]

simplify (x@(Var _):xs, ys) = [(x:xs, ys)]
simplify (xs, y@(Var _):ys) = [(xs, y:ys)]

simplify ([], []) = []


-- Check expressions to be Var or empty
checkSeq :: Sequent -> Bool
checkSeq (xs, ys) = checkExpr xs && checkExpr ys

checkExpr :: [Expr] -> Bool
checkExpr [] = True
checkExpr (x:xs) = isVar x && checkExpr xs

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

-- Tests --
test1 :: Expr
test1 = Not (Var "a")

test2 :: Expr
test2 = Impl (Var "p") (Disj (Var "p") (Var "q"))

test3 :: Expr
test3 = Impl (Not (Conj (Var "p") (Var "q"))) (Var "p")
