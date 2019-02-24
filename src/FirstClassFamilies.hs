{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE InstanceSigs           #-}

module FirstClassFamilies where

import Prelude hiding (fst)

-- functional dependency (t is fully determined by l)
class Eval l t | l -> t where
  eval :: l -> t

newtype Fst a b = Fst (a, b)
instance Eval (Fst a b) a where
  eval (Fst (a, _)) = a

newtype ListToMaybe a = ListToMaybe [a]
instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe [])      = Nothing
  eval (ListToMaybe (a : _)) = Just a

data MapList b a = MapList (a -> b) [a]
instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList _ [])       = []
  eval (MapList f (a : as)) = eval (f a) : eval (MapList f as)
