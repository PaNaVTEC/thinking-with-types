{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module AssociatedTypeFamilies where

import Data.Kind   (Type)
import Data.Monoid ((<>))
import Data.Proxy  (Proxy (..))
import GHC.TypeLits

data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text)
    => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param)
    => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

-- This instance is just to avoid double quoting on strings
instance {-# OVERLAPPING #-} HasPrintf a
    => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ param = format (s <> param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

example :: String
example = printf (Proxy @(Int :<< "+" :<< Int :<< "=3")) 1 2

example2 :: String
example2 = printf (Proxy @(String :<< " world!")) "hello"

example3 :: String
example3 = printf (Proxy @(String :<< Int :<< "")) "Hola" 1
