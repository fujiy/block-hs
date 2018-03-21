module Block.Bridge where

import Prelude
import Data.Monoid

import Block.Data
-- import Block.Data as Data
import Block.TypeChecker


-- default :: Array Statement
-- default = [BindStmt [Bind ()]]

main_module :: Statements
main_module = typeChecks prelude [BindStmt [Bind (epure $ Var "foo") (epure $ Num 2)]]

prelude :: Statements
prelude = typeChecks [] [BindStmt [Bind (epure $ Var "one") (epure $ Num 0)]]

exprB = epure $ App exprA (epure $ Var "fuga")
exprA = epure $ App (epure $ Var "hoge") (epure $ Num 0)


appToArray :: Expr -> Array Expr
appToArray = appToArray_


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
    TApp _ _ -> "app"
    Arrow    -> "arrow"
    Unknown  -> "unknown"
