module Block.TypeChecker where

import Prelude
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Class (ask, local)
import Control.Monad.Reader (Reader, ReaderT, runReader, runReaderT)
import Control.Monad.State (StateT, evalStateT, runStateT, get, put, modify)
import Data.Foldable (or)
import Data.Traversable (sequence)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))
import Data.Array ((:), (\\), unzip, uncons, foldr, head, toUnfoldable, elem)
import Data.Array.NonEmpty as NE
import Data.List as L
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Map as Map


import Block.Data
import Block.Debug

--------------------------------------------------------------------------------

type Interpreter = Reader Envir
-- type Envir = {binds :: Map String Scheme,
--               classes :: Map String {class :: Constraint,
--                                      members :: Map String {expr :: Expr, scheme :: Scheme, define :: Boolean}},
--               instances :: Map String (Array Scheme),
--               vars :: Map String Type}

type Envir = Map.Map String Scheme

type Infer = StateT TypeEnv Interpreter
type TypeEnv = {tvars :: Map.Map TVar Type, temp :: Int}

-- type Unifier = StateT TypeEnv Infer



-- infer :: forall a. Statements -> Infer a -> Interpreter a
-- infer ss m = evalStateT (runReader m ss) {tvars: Map.empty, temp: 0}

--------------------------------------------------------------------------------

interpret :: forall a. Envir -> Interpreter a -> a
interpret = flip runReader

infer :: forall a. Infer a -> Interpreter a
infer m = do
    Tuple a s <- runStateT m {tvars: Map.empty, temp: 0}
    pure $ trace (Map.toUnfoldable s.tvars :: Array (Tuple TVar Type)) a

localEnv :: forall a. Envir -> Infer a -> Infer a
localEnv env = local ((<>) env)

getBind :: String -> Infer (Maybe Scheme)
getBind s = Map.lookup s <$> ask

-- newTVarT :: Infer Type
-- newTVarT = (\s -> Type (TVar s) Base []) <$> newTVarS

--------------------------------------------------------------------------------

typeChecks :: Statements -> Statements -> Statements
typeChecks lib ss =
    let ss' = interpret (mconcat $ map envirs $ lib <> ss) $ mapM inferStmt ss
            -- unzip $ map (typeCheck (lib <> ss) >>>
            --              \{s: s, updated: u} -> Tuple s u) ss
    in if map envirs ss /= map envirs ss' then typeChecks lib ss' else ss'

        -- {old: (Map.toUnfoldable $ envir ss) :: Array (Tuple String Scheme),
        --  new: (Map.toUnfoldable $ envir ss') :: Array (Tuple String Scheme),
        --  updated: envir ss /= envir ss'}

typeCheck :: Statements -> Statements -> Statement -> {s :: Statement, updated :: Boolean}
typeCheck lib ss s =
    let s' = interpret (mconcat $ map envirs $ lib <> ss) $ inferStmt s
    in  {s: s', updated: (s /= s')}

inferStmt :: Statement -> Interpreter Statement
inferStmt st = case st of
    BindStmt bs -> BindStmt <$> infer (mapM (inferBind >=> showTypes) bs)

inferBind :: Bind -> Infer Bind
inferBind (Bind a b) = do
    let {head: Info e _ _, tail: as} = NE.uncons $ operToArray a
    tv <- newTVarT
    {args: as', expr: b', infos: i} <- inferLambda tv as b
    pure $ Bind (appBindVar (Info e (spure tv) i) as') b'
  where
    appBindVar :: Expr -> Array Expr -> Expr
    appBindVar = toApp

inferLambda :: Type -> Array Expr -> Expr -> Infer {args :: Array Expr, expr :: Expr, infos :: Infos}
inferLambda t as b = case uncons as of
    Just {head: a, tail: bs} -> do
        ta <- newTVarT
        tb <- newTVarT
        a' <- inferParam ta a
        {args: bs', expr: b', infos: i} <-
            localEnv (paramVars a') $ inferLambda tb bs b
        {type: t', infos: j} <- unify t (arrow ta tb)
        pure {args: a':bs', expr: b', infos: i <> j}
    Nothing -> do
        b' <- inferExpr t b
        pure {args: as, expr: b', infos: mempty}
    -- case uncons as of
    --     Just {head: a, tail: as'} -> do

inferParam :: Type -> Expr -> Infer Expr
inferParam t (Info e _ _) = pure $ Info e (spure t) mempty

inferExpr :: Type -> Expr -> Infer Expr
inferExpr t (Info e _ _) = case e of
    Var s -> do
        ms <- getBind s
        case ms of
            Just sc -> do
                ts <- instantiate sc
                {type: t', infos: i} <- unify t ts
                pure $ Info e (spure t') i
            Nothing -> traceWith "error" s $ pure $ Info e (spure t) (Infos{ errors: [OutOfScopeVar] })
    App a b -> do
        ta <- traceId <$> newTVarT
        tb <- newTVarT
        -- {type: t', infos: i} <- unify ta $ arrow tb t
        a' <- inferExpr (arrow tb t) a
        b' <- inferExpr tb b
        pure $ idefault (spure t) $ App a' b'
    Num _ -> do
        {type: t', infos: i} <- unify t (tpure $ Id "Int")
        pure $ Info e (spure t') i
    Empty -> pure $ idefault (spure t) e

--------------------------------------------------------------------------------

envirs :: Statement -> Envir
envirs s = case s of
    BindStmt bs -> maybe mempty binds $ head bs

binds :: Bind -> Envir
binds b = let Info e sc _ = bindVar b
          in case e of
              Var s -> Map.singleton s $ generalizeAll sc
              _     -> Map.empty

-- extend :: forall a. String -> Scheme -> Infer a -> Infer a
-- extend s sc m = local (Map.insert s sc) m

paramVars :: Expr -> Envir
paramVars (Info e sc _) = case e of
    Var s -> Map.singleton s sc
    _     -> Map.empty

toScheme :: Type -> Infer Scheme
toScheme t = pure $ spure t

--------------------------------------------------------------------------------

newTVarT :: Infer Type
newTVarT = TVar >>> tpure <$> newTVar

newTVar :: Infer TVar
newTVar = do
    r <- get
    let s = Temp r.temp
    put $ r {tvars = Map.insert s tempty r.tvars,
             temp = r.temp + 1}
    pure s

getTVar :: TVar -> Infer Type
getTVar v = do
    r <- get
    pure $ maybe tempty id (Map.lookup v r.tvars)

assignTVar :: TVar -> Type -> Infer Unit
assignTVar v t = modify \r -> r {tvars = Map.insert v t r.tvars}

getVar :: String -> Infer (Maybe Type)
getVar s = getBind s >>= \ms -> sequence $ instantiate <$> ms

tvars :: Infer (Array TVar)
tvars = (\r -> L.toUnfoldable $ Map.keys r.tvars) <$> get

evalType :: Type -> Infer Type
evalType x@(Info t k i) = case t of
    TVar v -> do
        x'@(Info t' _ _) <- getTVar v
        case t' of
            Unknown -> pure x
            _       -> evalType x'
    TApp a b -> do
        t' <- TApp <$> evalType a <*> evalType b
        pure $ Info t' k i
    _     -> pure x

deepEvalType :: Type -> Infer Type
deepEvalType = evalType

instantiate :: Scheme -> Infer Type
instantiate (Forall ts t) = do
    vts <- mapM (\v -> Tuple v <$> newTVarT) ts
    pure $ replace (Map.fromFoldable vts) t
  where
    replace :: Map.Map TVar Type -> Type -> Type
    replace m x@(Info ta k i) = case ta of
        TVar v   -> fromMaybe x $ Map.lookup v m
        TApp a b -> Info (TApp (replace m a) (replace m b)) k i
        _        -> x

generalize :: Scheme -> Infer Scheme
generalize (Forall _ t) = do
    tvs <- (\\) (tvarsOf t) <$> tvars
    pure $ Forall tvs t

generalizeAll :: Scheme -> Scheme
generalizeAll (Forall _ t) = Forall (tvarsOf t) t

unify :: Type -> Type -> Infer {type :: Type, infos :: Infos}
unify a b = do
    a'@(Info ta _ _) <- deepEvalType a
    b'@(Info tb _ _) <- deepEvalType b
    case Tuple ta tb of
        Tuple Unknown _ -> success b
        Tuple _ Unknown -> success a
        Tuple (TVar x) (TVar y) | x > y -> do
            -- getCstrs x >>= addCstrs y
            assignTVar x b
            success b
        Tuple (TVar x) (TVar y) | x < y -> do
            -- getCstrs y >>= addCstrs x
            assignTVar y a
            success a
        Tuple (TVar x) _ ->
            if occursCheck x b
            then error b $ EOccursCheck a' b'
            else do
                assignTVar x b
                success b
        Tuple _ (TVar y) ->
            if occursCheck y a
            then error a $ EOccursCheck a' b'
            else do
                assignTVar y a
                success a
        Tuple Arrow Arrow -> success a
        Tuple (Id xs) (Id ys) | xs == ys -> success a
        Tuple (TApp ax ay) (TApp bx by) -> do
            {type: tx, infos: ix} <- unify ax bx
            {type: ty, infos: iy} <- unify ay by
            pure {type: tpure $ TApp tx ty, infos: ix <> iy}
        _ -> error b $ EMisMatch a' b'
  where
    occursCheck :: TVar -> Type -> Boolean
    occursCheck v t = v `elem` tvarsOf t

    success :: Type -> Infer {type :: Type, infos :: Infos}
    success t = pure {type: t, infos: mempty}

    error :: Type -> Error -> Infer {type :: Type, infos :: Infos}
    error t e = pure {type: t, infos: Infos {errors: [e]}}

tvarsOf :: Type -> Array TVar
tvarsOf (Info t _ _) = case t of
    TVar v   -> [v]
    TApp a b -> tvarsOf a <> tvarsOf b
    _        -> []

--------------------------------------------------------------------------------

showTypes :: Bind -> Infer Bind
showTypes (Bind x y) = Bind <$> goExpr x <*> goExpr y
  where
    goExpr :: Expr -> Infer Expr
    goExpr (Info e sc i) = do
        e' <- case e of
            App a b -> App <$> goExpr a <*> goExpr b
            _ -> pure e
        sc' <- goScheme sc
        pure $ Info e' sc' i

    goScheme :: Scheme -> Infer Scheme
    goScheme (Forall ts t)= Forall ts <$> evalType t


--------------------------------------------------------------------------------

mapM :: forall m a b. Monad m => (a -> m b) -> Array a -> m (Array b)
mapM f = map f >>> sequence

mconcat :: forall m. Monoid m => Array m -> m
mconcat = foldr (<>) mempty
