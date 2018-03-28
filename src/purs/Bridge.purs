module Block.Bridge where

import Prelude
import Data.Monoid
import Data.Array (updateAt, deleteAt, head, length)
import Data.Tuple
import Data.Maybe (fromMaybe, maybe)

import Block.Data
import Block.Data as D
import Block.TypeChecker
import Block.TypeChecker as TC


-- default :: Array Statement
-- default = [BindStmt [Bind ()]]

main_module :: Statements
main_module = typeChecks prelude [BindStmt [Bind bar (epure $ Num 2)],
                                  BindStmt [Bind foo $ app (epure $ Var "negate") one]]

prelude :: Statements
prelude = typeChecks []
    [BindStmt [Bind (epure $ Var "one") (epure $ Num 0)]] <>
    [BindStmt [Bind (idefault (spure $ arrow (tpure $ Id "Int") (tpure $ Id "Int")) (Var "negate")) eempty],
     BindStmt [Bind (idefault (spure (tpure $ TVar $ Named "a")) (Var "hoge")) eempty]]

sampleExprs :: Array Expr
sampleExprs = [idefault (spure' $ Id "Int") (Num 0),
               idefault (spure $ arrow ta tb) (Lambda [idefault ta' $ Var "a"] $ idefault tb' Empty)]

ta = tpure $ TVar $ Named "a"
tb = tpure $ TVar $ Named "b"
ta' = spure ta
tb' = spure tb

exprB = epure $ App exprA (epure $ Var "fuga")
exprA = epure $ App (epure $ Var "hoge") (epure $ Num 0)
bar = epure $ App (epure $ Var "bar") (epure $ Var "x")
foo = epure $ App (epure $ Var "foo") (epure $ Var "y")
one = epure $ Num 1

app a b = epure $ App a b
spure' = spure <<< tpure

typeChecks = TC.typeChecks
typeCheck  = TC.typeCheck

appToArray :: Expr -> Array Expr
appToArray = appToArray_

tappToArray :: Type -> Array Type
tappToArray = tappToArray_

arrowToArray :: Type -> Array Type
arrowToArray = arrowToArray_

toApp :: Expr -> Array Expr -> Expr
toApp = D.toApp

spure = D.spure
eempty = D.eempty
pempty = epure $ Var "_"

bindStmtVar :: Statement -> Expr
bindStmtVar s = case s of
    BindStmt bs -> maybe eempty bindVar $ head bs

assignExpr :: Expr -> Expr -> Expr
assignExpr a@(Info x _ _) b@(Info y _ _) = case Tuple x y of
    Tuple Empty _ -> b
    _ -> b

fillExprWith :: Int -> Expr -> Expr -> Expr
fillExprWith 0 a f = app f a
fillExprWith i a f = app (fillExprWith (i - 1) a f) eempty

econs :: ExprA -> String
econs e = case e of
    Var _      -> "var"
    App _ _    -> "app"
    Num _      -> "num"
    Lambda _ _ -> "lam"
    Empty      -> "emp"

tcons :: TypeA -> String
tcons t = case t of
    Id _     -> "id"
    TVar _   -> "var"
    Arrow    -> "arr"
    TApp (Info (TApp (Info Arrow _ _) _) _ _) _ -> "arr"
    TApp _ _ -> "app"
    Unknown  -> "unk"

errcons :: Error -> String
errcons e = case e of
  EOutOfScopeVar _ -> "var"
  EMisMatch _ _    -> "match"
  EOccursCheck _ _ -> "occurs"

renewI :: forall a. Int -> a -> Array a -> Array a
renewI i x xs = fromMaybe xs $ updateAt i x xs

renewBindStmt :: Int -> Bind -> Statement -> Statement
renewBindStmt i b (BindStmt bs) = BindStmt $ renewI i b bs

renewLeft :: Expr -> Bind -> Bind
renewLeft l (Bind _ r) = Bind l r

renewRight :: Expr -> Bind -> Bind
renewRight r (Bind l _) = Bind l r

renewExpr :: ExprA -> Expr -> Expr
renewExpr e (Info _ t i) = Info e t i

renewArgs :: Int -> Expr -> Array Expr -> Expr -> Expr
renewArgs i a as b = toApp b $ renewI i a as

deleteArg :: Int -> Array Expr -> Expr -> Expr
deleteArg i as b = toApp b $ fromMaybe as $ deleteAt i as

renewLambda :: Int -> Expr -> Array Expr -> Expr -> Expr
renewLambda i a as b = epure $ Lambda (renewI i a as) b

deleteLambda :: Int -> Array Expr -> Expr -> Expr
deleteLambda i as b = lambdaC (fromMaybe as $ deleteAt i as) b

appC    = App
varC    = Var
lambdaC [] b = b
lambdaC as b = epure $ Lambda as b
