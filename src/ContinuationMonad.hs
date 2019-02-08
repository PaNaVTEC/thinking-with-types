{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module ContinuationMonad where
import Control.Monad (join)

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r }

instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  fmap ab (Cont arr) = Cont $ \ar -> arr (ar . ab)

instance Applicative Cont where
  pure :: a -> Cont a
  pure a = Cont $ \ar -> ar a

  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  (<*>) (Cont abr) (Cont ar) = Cont $ \a -> abr (a . ar)

instance Monad Cont where
  return = pure

  (>>=) :: Cont a -> (a -> Cont b) -> Cont b
  (>>=) ca abrr = join $ pure abrr <*> ca

idCPS :: a -> (a -> r) -> r
idCPS a ret = ret a

sum1CPS :: Int -> (Int -> Int) -> Int
sum1CPS a f = f a
