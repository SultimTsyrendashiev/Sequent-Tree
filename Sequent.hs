module SequentTree (
  Expr,
  SeqTree,
  createTree,
  hasCE,
  isValid) where

import Data.GraphViz as G
import Data.GraphViz.Attributes.Complete as G
import Data.GraphViz.Types as G
import Data.Text.Lazy.IO as TL

data Expr = Var String
          | Not Expr
          | Conj Expr Expr
          | Disj Expr Expr
          | Impl Expr Expr
          deriving (Eq, Ord)

data VarValue = VarValue String Bool
  deriving Show

instance Show Expr where
  show (Var a) = a
  show (Not (Var a)) = "-" ++ a
  show (Not e) = "-" ++ "(" ++  show e ++ ")"

  show (Conj (Var a1) (Var a2)) = a1 ++ " & " ++ a2
  show (Conj e1 (Var a2)) = "(" ++ show e1 ++ ") & " ++ a2
  show (Conj (Var a1) e2) = a1 ++ " & (" ++ show e2 ++ ")"
  show (Conj e1 e2) = "(" ++ show e1 ++ ") & (" ++ show e2 ++ ")"

  show (Disj (Var a1) (Var a2)) = a1 ++ " | " ++ a2
  show (Disj e1 (Var a2)) = "(" ++ show e1 ++ ") | " ++ a2
  show (Disj (Var a1) e2) = a1 ++ " | (" ++ show e2 ++ ")"
  show (Disj e1 e2) = "(" ++ show e1 ++ ") | (" ++ show e2 ++ ")"

  show (Impl (Var a1) (Var a2)) = a1 ++ " -> " ++ a2
  show (Impl e1 (Var a2)) = "(" ++ show e1 ++ ") -> " ++ a2
  show (Impl (Var a1) e2) = a1 ++ " -> (" ++ show e2 ++ ")"
  show (Impl e1 e2) = "(" ++ show e1 ++ ") -> (" ++ show e2 ++ ")"

type Sequent = ([Expr], [Expr])

data SeqTree = Node Sequent [SeqTree]

-- Tree to graphviz --
getNodes :: SeqTree -> [(String, ())]
getNodes (Node s []) = [(show s, ())]
getNodes (Node s trees) = (show s, ()) : concatMap getNodes trees

getEdges :: SeqTree -> [(String, String, ())]
getEdges (Node _ []) = []
getEdges (Node s trees) = map (connectWithChild s . getSeq) trees ++ concatMap getEdges trees

connectWithChild :: Sequent -> Sequent -> (String, String, ())
connectWithChild s1 s2 = (show s1, show s2, ())

getSeq :: SeqTree -> Sequent
getSeq (Node s _) = s

-- GraphViz params --
graphParams :: G.GraphvizParams String () () () ()
graphParams = G.defaultParams {
  G.fmtNode = \_ -> clrAtr $ G.RGB 0 0 0,
  G.fmtEdge = \_ -> clrAtr $ G.RGB 20 20 20
}
  where clrAtr c = [ G.Color $ G.toColorList [c] ]

-- Check sequent for validness --
isValid :: Sequent -> IO () -- (SeqTree, [VarValue])
isValid s = do
  let tree = createTree s
      ce = findCE tree
      dotGr = G.graphElemsToDot graphParams (getNodes tree) (getEdges tree)
      dotText = G.printDotGraph dotGr
  Prelude.putStrLn $ "Общезначима: " ++ show (Prelude.null ce)
  Prelude.putStrLn $ "Контрпримеры: " ++ show ce
  TL.writeFile "graph.dot" dotText

findCE :: SeqTree -> [VarValue]
findCE (Node (xs,ys) []) = if haveSame xs ys
                           then [] -- no CE
                           else toVars xs True ++ toVars ys False -- found CE
findCE (Node _ ts) = Prelude.head $ Prelude.map findCE ts

toVars :: [Expr] -> Bool -> [VarValue]
toVars [] _ = []
toVars (Var name:xs) b = VarValue name b : toVars xs b
toVars _ _ = []

-- Check sequent for counter examples --
hasCE :: Sequent -> (Bool, SeqTree)
hasCE s = let tree = createTree s
          in (treeHasCE tree, tree)

-- Check tree for counter examples --
treeHasCE :: SeqTree -> Bool
-- if node is a leaf
treeHasCE (Node (xs,ys) []) = not $ haveSame xs ys -- if there are no same vars on both sides => CE exist
treeHasCE (Node _ tree) = Prelude.foldr ((&&) . treeHasCE) True tree

haveSame :: [Expr] -> [Expr] -> Bool
haveSame [] _ = False
haveSame (x:xs) ys = x `elem` ys || haveSame xs ys

-- Create sequent tree --
createTree :: Sequent -> SeqTree
createTree s = Node s $ Prelude.map createTree $ procSeq s

-- Returns [] if all members are variables
procSeq :: Sequent -> [Sequent]
procSeq s = if checkSeq s
            then []
            else Prelude.map reduceSame (simplify s)

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
checkExpr = Prelude.foldr ((&&) . isVar) True

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

-- Tests --
test1 :: Sequent
test1 = ([], [Not (Var "a")])

test2 :: Sequent
test2 = ([], [Impl (Var "p") (Disj (Var "p") (Var "q"))])

test3 :: Sequent
test3 = ([], [Impl (Not (Conj (Var "p") (Var "q"))) (Var "p")])

test4 :: Sequent
test4 = ([],[Conj (Impl (Not (Conj (Var "p") (Var "q"))) (Var "p")) (Conj (Disj (Var "p") (Var "q")) (Impl (Not (Conj (Var "p") (Var "q"))) (Var "p")))])
