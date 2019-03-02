{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module TypeLevelDefunctionalization where

import           Data.Kind       (Constraint, Type)
import qualified GHC.TypeLits as TL

type Exp a = a -> Type

type family Eval' (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b
type instance Eval' (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval' (FromMaybe _1 ('Just a)) = a
type instance Eval' (FromMaybe a 'Nothing)   = a

data ListToMaybe' :: [a] -> Exp (Maybe a)
type instance Eval' (ListToMaybe' '[]) = 'Nothing
type instance Eval' (ListToMaybe' (a ': as)) = 'Just a
-- :kind! Eval' (ListToMaybe' '[1, 2])
-- HOF
data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval' (MapList f '[]) = '[]
type instance Eval' (MapList f (a ': as))
  = Eval' (f a) ': Eval' (MapList f as)

data FoldR :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval' (FoldR _ b '[]) = b
type instance Eval' (FoldR f b (a ': as))
  = Eval' (f a (Eval' (FoldR f b as)))

data AddNat :: TL.Nat -> TL.Nat -> Exp TL.Nat
type instance Eval' (AddNat a a') = a TL.+ a'

-- :kind! (Eval' (FoldR AddNum 0 '[1, 2, 3]))

data Pure :: a -> Exp a
type instance Eval' (Pure x) = x

-- Function aplication at the type level
data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval' (k =<< e) = Eval' (k (Eval' e))
infixr 0 =<<

-- Function composition at the type level
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval' ((f <=< g) x) = Eval' (f (Eval' (g x)))
infixr 1 <=<

type Snd2 = Snd <=< Snd

-- Type equality at the type level
data TyEq :: a -> b -> Exp Bool
type instance Eval' (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval' (Collapse '[]) =
  (() :: Constraint)
type instance Eval' (Collapse (a ': as)) =
  (a, Eval' (Collapse as))

type All (c :: k -> Constraint) (ts :: [k]) =
  Collapse =<< MapList (Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval' (Pure1 f x) = f x

-- AD-hoc polymorphism
data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval' (Map f '[]) = '[]
type instance Eval' (Map f (a ': as)) = Eval' (f a) ': Eval' (Map f as)

type instance Eval' (Map f 'Nothing)  = 'Nothing
type instance Eval' (Map f ('Just a)) = 'Just (Eval' (f a))

type instance Eval' (Map f ('Left x))  = 'Left x
type instance Eval' (Map f ('Right a)) = 'Right (Eval' (f a))

type instance Eval' (Map f ('(a, b))) = '(a , Eval' (f b))

data Mappend :: a -> a -> Exp a
-- type instance Eval' (Mappend '() '()) = '()
-- type instance Eval'
--   (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
-- type instance Eval'
--   (Mappend (a :: [k]) (b :: [k])) = Eval' (a ++ b)
data Mempty :: k -> Exp k
type instance Eval' (Mempty '()) = '()
type instance Eval' (Mempty (c :: Constraint)) = (() :: Constraint)
type instance Eval' (Mempty (l :: [k])) = '[]
