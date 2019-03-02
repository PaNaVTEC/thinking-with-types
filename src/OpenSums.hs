{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module OpenSums where

import Data.Kind             (Type)
import Data.Proxy
import Fcf
import GHC.TypeLits   hiding (type (+))
import Unsafe.Coerce

import Data.Functor.Identity

data OpenSum (f :: k -> Type) (ts :: [k]) where
  -- t is existential
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral
  . natVal
  $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

a :: OpenSum Identity '[String]
a = inj (Identity "1")

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f') =
  if i == findElem @t @ts
  then Just $ unsafeCoerce f'
  else Nothing

b :: Maybe (Identity String)
b = prj a

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 ft) = Left  $ unsafeCoerce ft
decompose (UnsafeOpenSum n ft) = Right $ UnsafeOpenSum (n - 1) ft

c :: Either (Identity String) (OpenSum Identity '[])
c = decompose a

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

d :: OpenSum Identity '[Int, String]
d = weaken a

e :: Either (Identity Int) (OpenSum Identity '[String])
e = decompose d

f :: Either (Identity String) (OpenSum Identity '[])
f = do
      eiIS <- decompose foo
      decompose eiIS
  where
    foo :: OpenSum Identity '[String, String]
    foo = weaken (inj (Identity "1"))

match :: forall f ts b
  . (forall t. f t -> b)
  -> OpenSum f ts
  -> b
match fn (UnsafeOpenSum _ t) = fn t

g :: String
g = match aFn d
  where
    aFn (Identity i) = unsafeCoerce i
--    aFn (Identity (s :: String)) = s
