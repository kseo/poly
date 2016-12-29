module Eval where

import Syntax

import Data.Function (fix)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

data CExpr
  = CVar Name
  | CApp CExpr CExpr
  | CLam (CExpr -> CExpr)
  | CBool Bool
  | CInt Integer

instance Show CExpr where
  show (CVar n) = show n
  show CApp{} = "<<app>>"
  show CLam{} = "<<closure>>"
  show (CBool n) = show n
  show (CInt n) = show n

desugar :: Expr -> Expr
desugar (App fun arg) = App (desugar fun) (desugar arg)
desugar (Lam x body) = Lam x (desugar body)
desugar (Let x e body) = App (Lam x (desugar body)) (desugar e)
desugar (If cond tr fl) = foldl App (Var "$IF") args
   where args = map desugar [cond, tr, fl]
desugar (Fix e) = App (Var "$FIX") (desugar e)
desugar (Op op a b) = foldl App (Var n) args
  where
    args = map desugar [a, b]
    n = case op of
      Add -> "$ADD"
      Sub -> "$SUB"
      Mul -> "$MUL"
      Eql -> "$EQL"
desugar e = e

compile :: Expr -> CExpr
compile (Var n) = CVar n
compile (App fun arg) = CApp (compile fun) (compile arg)
compile (Lam x body) = abstract x (compile body)
compile (Lit (LInt k)) = CInt k
compile (Lit (LBool k)) = CBool k

abstract :: Name -> CExpr -> CExpr
abstract x (CApp fun arg) = combS (abstract x fun) (abstract x arg)
abstract x (CVar n) | x == n = combI
abstract _ k = combK k

combS :: CExpr -> CExpr -> CExpr
combS f = CApp (CApp (CVar "$S") f)

combK :: CExpr -> CExpr
combK = CApp (CVar "$K")

combI :: CExpr
combI = CVar "$I"

infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CLam f) ! x = f x

primitives :: [(String, CExpr)]
primitives =
  [ ("$I", CLam $ \x -> x)
  , ("$K", CLam $ \x -> CLam $ \_ -> x)
  , ("$S", CLam $ \f -> CLam $ \g -> CLam $ \x -> f!x!(g!x))
  , ("$IF", CLam $ \(CBool cond) -> CLam $ \tr -> CLam $ \fl -> if cond then tr else fl)
  , ("$FIX", CLam $ \(CLam f) -> fix f)
  , ("$ADD", arith (+))
  , ("$SUB", arith (-))
  , ("$MUL", arith (*))
  , ("$EQL", logical (==))
  ]

arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CLam $ \(CInt a) -> CLam $ \(CInt b) -> CInt (op a b)

logical :: (Integer -> Integer -> Bool) -> CExpr
logical op = CLam $ \(CInt a) -> CLam $ \(CInt b) -> if op a b then true else false

true, false :: CExpr
true = CBool True
false = CBool False

type TermEnv = Map.Map String CExpr

emptyTmenv :: TermEnv
emptyTmenv = Map.fromList primitives

link :: TermEnv -> CExpr -> CExpr
link bs (CApp fun arg) = link bs fun ! link bs arg
link bs (CVar n) = fromJust (Map.lookup n bs)
link _ e = e

eval :: TermEnv -> Expr -> CExpr
eval env = link env . compile . desugar

runEval :: TermEnv -> String -> Expr -> (CExpr, TermEnv)
runEval env nm ex =
  let res = eval env ex in
  (res, Map.insert nm res env)
