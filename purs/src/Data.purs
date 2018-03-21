module Block.Data where

import Prelude
import Data.Semigroup
import Data.Eq
import Data.Monoid
import Data.Array (foldl)
import Data.Array.NonEmpty (NonEmptyArray, snoc, toArray)


-- Info ------------------------------------------------------------------------

data Info t f = Info (InfoA t f) t Infos
type InfoA t f = f (Info t f)

-- derive instance eqInfo :: (Eq t, Eq a, Eq (f a)) => Eq (Info t f)

-- derive instance eqInfo :: (Eq t, Eq (f (Info t f))) => Eq (Info t f)

-- derive instance eq1Info :: (Eq t, Eq1 f) => Eq1 (Info t f)
instance eqInfo :: (Eq t, Eq1 f) => Eq (Info t f) where
    eq (Info fa ta ia) (Info fb tb ib) = eq1 fa fb && ta == tb && ia == ib

-- instance eq1Info :: (Eq t, Eq1 f) => Eq1 (Info t f) where
--   eq1 (Info iax tx ix) (Info iay ty iy) = iax `eq1` iay && tx == ty && ix == iy

idefault :: forall t f. t -> InfoA t f -> Info t f
idefault t a = Info a t mempty

-- Statement -------------------------------------------------------------------

data Statement = BindStmt (Array Bind)
type Statements = Array Statement

derive instance eqStatement :: Eq Statement

-- Expr ------------------------------------------------------------------------

type Expr  = Info  Scheme ExprC
type ExprA = InfoA Scheme ExprC
data ExprC e = Var String
           -- | Cons String
             | App e e
           -- | Lambda (Array Expr) Expr
             | Num Int
           -- | Oper Expr (Maybe Expr) (Maybe Expr)
           -- | If Expr Expr Expr
           -- | Case Expr (Array (Tuple Expr Expr))
           -- | Let (Array Bind) Expr
           -- | TypeAnnot Expr Scheme
             | Empty
             -- | Arg
data Bind = Bind Expr Expr

instance eq1ExprC :: Eq1 ExprC where
    eq1 (Var xs)    (Var ys)    = xs == ys
    eq1 (App ax bx) (App ay by) = ax == bx && ay == by
    eq1 (Num x)     (Num y)     = x == y
    eq1 Empty       Empty       = true
    eq1 _           _           = false
derive instance eqBind :: Eq Bind

epure :: ExprA -> Expr
epure e = Info e sempty mempty

operToArray :: Expr -> NonEmptyArray Expr
operToArray = appToArray

appToArray :: Expr -> NonEmptyArray Expr
appToArray (Info e t i) = case e of
    App a b -> appToArray a `snoc` b
    _       -> pure $ Info e t i

appToArray_ :: Expr -> Array Expr
appToArray_ = appToArray >>> toArray

toApp :: Expr -> Array Expr -> Expr
toApp = foldl (\a b -> Info (App a b) sempty mempty)

-- Scheme ----------------------------------------------------------------------

data Scheme = Forall (Array TVar) Type

derive instance eqScheme :: Eq Scheme

sempty :: Scheme
sempty = Forall [] tempty

spure :: Type -> Scheme
spure t = Forall [] t

-- Type ------------------------------------------------------------------------

type Type  = Info  Kind TypeC
type TypeA = InfoA Kind TypeC
data TypeC t = Id String
             | TVar TVar
             | TApp t t
             | Arrow
             | Unknown
data TVar = Named String
          | Temp Int
type Constraint = Type

instance eq1TypeC :: Eq1 TypeC where
    eq1 (Id xs)      (Id ys)      = xs == ys
    eq1 (TVar vx)    (TVar vy)    = vx == vy
    eq1 (TApp ax bx) (TApp ay by) = ax == bx && ay == by
    eq1 Arrow        Arrow        = true
    eq1 Unknown      Unknown      = true
    eq1 _            _            = false
derive instance eqTVar :: Eq TVar
derive instance ordTVar :: Ord TVar

tempty :: Type
tempty = idefault Base Unknown

tpure :: TypeA -> Type
tpure t = Info t Base mempty

eqType :: Type -> Type -> Boolean
eqType tx ty = tx == ty

-- Kind ------------------------------------------------------------------------

data Kind = Base
          | TCons Kind Kind
          | Constraint

derive instance eqKind :: Eq Kind

-- Infos -----------------------------------------------------------------------

newtype Infos = Infos { error :: Array String }

derive instance eqInfos :: Eq Infos

instance sInfos :: Semigroup Infos where
    append (Infos{ error: exs }) (Infos{ error: eys }) =
        Infos{ error: exs <> eys }

instance mInfos :: Monoid Infos where
    mempty = Infos{ error: [] }
