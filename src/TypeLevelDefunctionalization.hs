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
