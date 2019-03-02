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

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
  then Just $ unsafeCoerce f
  else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left  $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

-- TODO
weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken _ = undefined
