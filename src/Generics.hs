{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE DataKinds #-}

module Generics where

import GHC.Generics
import Data.Proxy
import Data.Kind (Constraint, Type)

-- Carrier class
class GEq a where
  geq :: a x -> a x -> Bool

instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

-- U1 represents a data constructor with no parameters, in which case it's just ()
instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 b1) (R1 b2) = geq b1 b2
  geq _ _             = False

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance Eq a => GEq (K1 _1 a) where
  geq (K1 a) (K1 b) = a == b

instance GEq U1 where
  geq U1 U1 = True

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

data Foo a b c
  = F0
  | F1 a
  | F2 b c
  deriving (Generic, MyEq, MyOrd)

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  (==) = genericEq

class MyEq a where
  eq :: a -> a -> Bool
  default eq
      :: (Generic a, GEq (Rep a))
      => a
      -> a
      -> Bool
  eq a b = geq (from a) (from b)

-- Exercise Provide a generic instance for the Ord class.

class GOrd a where
  gord :: a x -> a x -> Ordering

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering
genericOrd a b = gord (from a) (from b)

instance (Ord a, Ord b, Ord c) => Ord (Foo a b c) where
  compare = genericOrd

instance GOrd a => GOrd (M1 _x _y a) where
  gord (M1 a1) (M1 a2) = gord a1 a2

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
  gord (L1 a1) (L1 a2) = gord a1 a2
  gord (R1 b1) (R1 b2) = gord b1 b2
  gord (R1 _) (L1 _) = GT
  gord (L1 _) (R1 _) = LT

instance Ord a => GOrd (K1 _1 a) where
  gord (K1 a) (K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
  gord (a1 :*: b1) (a2 :*: b2) =
    case gord a1 a2 of
      EQ -> gord b1 b2
      x -> x

instance GOrd U1 where
  gord U1 U1 = EQ

class MyOrd a where
  ord :: a -> a -> Ordering
  default ord
      :: (Generic a, GOrd (Rep a))
      => a
      -> a
      -> Ordering
  ord a b = gord (from a) (from b)

-- Exercise Use GHC.Generics to implement the function exNihilo :: Maybe a. This function should give a value of Just a if a has exactly one data constructor which takes zero arguments. Otherwise, exNihilo should return Nothing.

tryExNihilo :: Maybe Foo'
tryExNihilo = exNihilo

exNihilo :: forall a. CompareConstructors a => Maybe a
exNihilo = countCons @a

class GCountConstructors a where
  gcountcons :: Maybe (a x)

data Foo' = Asd deriving (Generic, Show)

class CompareConstructors a where
  countCons :: Maybe a

instance CompareConstructors Foo' where
  countCons = genericCountCons @Foo'

genericCountCons :: forall a. (Generic a, GCountConstructors (Rep a)) => Maybe a
genericCountCons = to <$> gcountcons

instance GCountConstructors a => GCountConstructors (M1 _x _u a) where
  gcountcons = M1 <$> gcountcons

instance GCountConstructors U1 where
  gcountcons = Just U1

instance (GCountConstructors a, GCountConstructors b) => GCountConstructors (a :+: b) where
  gcountcons = Nothing

-- instance GCountConstructors (M1 _x _y a) where
--   gcountcons = Nothing
