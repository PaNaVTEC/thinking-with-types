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

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"

runCont :: (forall r. (a -> r) -> r) -> a
runCont f =
  let callback = id
  in f callback

-- Difference of usage

releaseString :: String
releaseString =
  withVersionNumber $ \version ->
    withTimestamp $ \date ->
      withOS $ \os ->
        os <> "-" <> show version <> "-" <> show date

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
    version <- Cont withVersionNumber
    date    <- Cont withTimestamp
    os      <- Cont withOS
    pure $ os ++ "-" ++ show version ++ "-" ++ show date

newtype ContT m a = ContT { runContT :: forall r. (a -> m r) -> m r }

instance Functor (ContT m) where
  fmap :: (a -> b) -> ContT m a -> ContT m b
  fmap ab (ContT arr) = ContT $ \ar -> arr (ar . ab)

instance Applicative (ContT m) where
  pure :: a -> ContT m a
  pure a = ContT $ \ar -> ar a

  (<*>) :: ContT m (a -> b) -> ContT m a -> ContT m b
  (<*>) (ContT abr) (ContT ar) = ContT $ \a -> abr $ \b -> ar (a . b)

instance Monad (ContT m) where
  return = pure

  (>>=) :: ContT m a -> (a -> ContT m b) -> ContT m b
  (>>=) ca abrr = join $ pure abrr <*> ca
