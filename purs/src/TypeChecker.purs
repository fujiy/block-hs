module Block.TypeChecker where

import Prelude
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Class (ask, local)
import Control.Monad.Reader (Reader, ReaderT, runReader, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Foldable (or)
import Data.Traversable (sequence)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Data.Array(unzip)
import Data.Array.NonEmpty (uncons)
import Data.Maybe(Maybe(..), maybe)
import Data.Map as Map


import Block.Data
import Block.Debug

--------------------------------------------------------------------------------

type Interpreter = Reader Statements
-- type Envir = {binds :: Map String Scheme,
--               classes :: Map String {class :: Constraint,
--                                      members :: Map String {expr :: Expr, scheme :: Scheme, define :: Boolean}},
--               instances :: Map String (Array Scheme),
--               vars :: Map String Type}

type Infer = StateT TypeEnv (ReaderT Envir Interpreter)
type Envir = Map.Map String Scheme
type TypeEnv = {tvars :: Map.Map TVar Scheme, temp :: Int}

-- type Unifier = StateT TypeEnv Infer



-- infer :: forall a. Statements -> Infer a -> Interpreter a
-- infer ss m = evalStateT (runReader m ss) {tvars: Map.empty, temp: 0}

--------------------------------------------------------------------------------

interpret :: forall a. Statements -> Interpreter a -> a
interpret = flip runReader

infer :: forall a. Infer a -> Interpreter a
infer m = runReaderT (evalStateT m {tvars: Map.empty, temp: 0}) Map.empty

localEnv :: forall a. Envir -> Infer a -> Infer a
localEnv env = local ((<>) env)

getBind :: String -> Infer (Maybe Scheme)
getBind s = Map.lookup s <$> ask

-- newTVarT :: Infer Type
-- newTVarT = (\s -> Type (TVar s) Base []) <$> newTVarS

newTVarT :: Infer Type
newTVarT = TVar >>> tpure <$> newTVar

newTVar :: Infer TVar
newTVar = do
    r <- get
    let s = Temp r.temp
    put $ r {tvars = Map.insert s (spure $ tempty) r.tvars,
             temp = r.temp + 1}
    pure s

--------------------------------------------------------------------------------

typeChecks :: Statements -> Array Statement -> Array Statement
typeChecks lib ss =
    let Tuple ss' us =
            unzip $ map (typeCheck (lib <> ss) >>>
                         \{s: s, updated: u} -> Tuple s u) ss
    in if or us then typeChecks lib ss' else ss'

typeCheck :: Statements -> Statement -> {s :: Statement, updated :: Boolean}
typeCheck ss s =
    let s' = interpret ss $ inferStmt s
    in  {s: s', updated: trace s' false}

inferStmt :: Statement -> Interpreter Statement
inferStmt st = case st of
    BindStmt bs -> BindStmt <$> infer (mapM inferBind bs)

inferBind :: Bind -> Infer Bind
inferBind (Bind a b) = do
    let {head: Info e _ _, tail: as} = uncons $ operToArray a
    tv <- newTVarT
    {args: as', expr: b'} <- inferLambda tv as b
    pure $ Bind (appBindVar (idefault (spure tv) e) as') b'
  where
    appBindVar :: Expr -> Array Expr -> Expr
    appBindVar = toApp

inferLambda :: Type -> Array Expr -> Expr -> Infer {args :: Array Expr, expr :: Expr}
inferLambda t as b = do
    b' <- inferExpr t b
    pure {args: as, expr: b'}
    -- case uncons as of
    --     Just {head: a, tail: as'} -> do

inferExpr :: Type -> Expr -> Infer Expr
inferExpr t (Info e _ _) = case e of
    Var s -> do
        ms <- getVar s
        -- case ms of
        --     Just sc' -> do
        --         {type: t', info: i} <- unify t td
        pure $ Info e (spure t) mempty
    App a b -> pure $ Info e (spure t) mempty
    Num _ -> do
        {type: t', infos: i} <- unify t (tpure $ Id "Int")
        pure $ Info e (spure t') mempty
    Empty -> pure $ Info e (spure t) mempty

--------------------------------------------------------------------------------

getVar :: String -> Infer (Maybe Type)
getVar s = getBind s >>= \ms -> sequence $ instantiate <$> ms

instantiate :: Scheme -> Infer Type
instantiate (Forall _ t) = pure t

unify :: Type -> Type -> Infer {type :: Type, infos :: Infos}
unify a b =
    let Info ta _ _ = a
        Info tb _ _ = b
    in case Tuple ta tb of
    Tuple Unknown _ ->
        pure {type: b, infos: mempty}
    Tuple _ Unknown ->
        pure {type: a, infos: mempty}
    _ -> pure {type: b, infos: mempty}

-- unifier :: Array Statement -> Array Statement -> Interpreter (Array Statement)
-- unifier lib ss = do
--     ss' <- mapM (inferStatement lib) ss
--     unifierGo lib ss'
--     -- pure ss'
--
-- unifierGo :: Array Statement -> Array Statement -> Interpreter (Array Statement)
-- unifierGo lib ss = do
--     ss' <- mapM (inferStatement (lib <> ss)) ss
--     if ss == ss' then pure ss'
--                  else unifierGo lib ss'

--------------------------------------------------------------------------------

mapM :: forall m a b. Monad m => (a -> m b) -> Array a -> m (Array b)
mapM f = map f >>> sequence
