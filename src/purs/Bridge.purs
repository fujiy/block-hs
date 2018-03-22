module Block.Bridge where

import Prelude
import Data.Monoid

import Block.Data
-- import Block.Data as Data
import Block.TypeChecker


-- default :: Array Statement
-- default = [BindStmt [Bind ()]]

main_module :: Statements
main_module = typeChecks prelude [BindStmt [Bind bar (epure $ Num 2)],
                                  BindStmt [Bind foo (epure $ Var "bar")]]

prelude :: Statements
prelude = typeChecks [] [BindStmt [Bind (epure $ Var "one") (epure $ Num 0)]]

exprB = epure $ App exprA (epure $ Var "fuga")
exprA = epure $ App (epure $ Var "hoge") (epure $ Num 0)
bar = epure $ App (epure $ Var "bar") (epure $ Var "x")
foo = epure $ App (epure $ Var "foo") (epure $ Var "y")


appToArray :: Expr -> Array Expr
appToArray = appToArray_

tappToArray :: Type -> Array Type
tappToArray = tappToArray_


econs :: ExprA -> String
econs e = case e of
    Var _   -> "var"
    App _ _ -> "app"
    Num _   -> "num"
    Empty   -> "emp"

tcons :: TypeA -> String
tcons t = case t of
    Id _     -> "id"
    TVar _   -> "var"
    Arrow    -> "arr"
    TApp (Info (TApp (Info Arrow _ _) _) _ _) _ -> "arr"
    TApp _ _ -> "app"
    Unknown  -> "unk"
