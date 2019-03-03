{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module OpenProducts where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import Data.Functor.Identity

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f  :: k -> Type)
                 (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

type UniqueKey (key :: k) (ts :: [(k, t)])
  = Eval (Null =<< Filter (TyEq key <=< Fst) ts)

insert
  :: UniqueKey key ts ~ 'True
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck
    =<< FindIndex (TyEq key <=< Fst) ts)

findElem
  :: forall key ts
   . KnownNat (FindElem key ts)
  => Int
findElem = fromIntegral
  . natVal
  $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)]) =
  FromMaybe Stuck =<< Lookup key ts

get
  :: forall key ts f
   . KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> f (Eval (LookupType key ts))
get _ (OpenProduct v) =
    unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  SetIndex (FindElem key ts) '(key, t) ts

update
  :: forall key ts t f
   . KnownNat (FindElem key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Filter (Not <=< TyEq key <=< Fst) ts

delete
  :: forall f key ts
  . KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
  OpenProduct $ vdeleteAt (findElem @key @ts) v

vdeleteAt :: Int -> V.Vector a -> V.Vector a
vdeleteAt idx xs =
  let (hs, ts) = V.splitAt idx xs
  in hs V.++ V.tail ts

sample :: OpenProduct Identity '[ '("2", String), '("1", String)]
sample
  = insert (Key @"2") (Identity "2")
  . insert (Key @"1") (Identity "1")
  $ nil

getOfFoo :: Identity String
getOfFoo = get (Key @"1") sample

deleteOfOne :: OpenProduct Identity '[ '("2", String) ]
deleteOfOne = delete (Key @"1") sample

type Upsert
  (key :: Symbol)
  (t :: k)
  (ts :: [(Symbol, k)])
  = UnMaybe
      (Pure ('(key, t) ': ts))
      (ConstFn (Eval (UpdateElem key t ts)))
      (Eval (FindIndex (TyEq key <=< Fst) ts))

type family Exists'
  (key :: Symbol)
  (ts :: [(Symbol, k)]) where
  Exists' k ts = Eval (FindIndex (TyEq k <=< Fst) ts)

class MaybeKnownNat (n :: Maybe Nat) where
  maybeNat :: Maybe Int

instance MaybeKnownNat 'Nothing where
  maybeNat = Nothing

instance KnownNat a => MaybeKnownNat ('Just a) where
  maybeNat = Just . fromIntegral $ natVal $ Proxy @a

upsert
  :: forall f key t ts
  . MaybeKnownNat (Exists' key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (Upsert key t ts))
upsert k ft (OpenProduct v)
  = case maybeNat @(Exists' key ts) of
      (Just a) -> OpenProduct $ v V.// [(a, Any ft)]
      Nothing -> OpenProduct $ V.cons (Any ft) v

sample' :: OpenProduct Identity '[ '("1", String), '("2", String)]
sample'
  = upsert (Key @"2") (Identity "2")
  . upsert (Key @"1") (Identity "1")
  . upsert (Key @"2") (Identity "3")
  $ nil

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key
