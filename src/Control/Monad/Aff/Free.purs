module Control.Monad.Aff.Free where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Cont.Trans (ContT())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (ExceptT())
import Control.Monad.Free (Free(), liftF)
import Control.Monad.List.Trans (ListT())
import Control.Monad.Maybe.Trans (MaybeT())
import Control.Monad.Reader.Trans (ReaderT())
import Control.Monad.RWS.Trans (RWST())
import Control.Monad.State.Trans (StateT())
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Trans (WriterT())

import Data.Monoid (class Monoid)

class Affable eff f where
  fromAff :: forall a. Aff eff a -> f a

instance affableAff :: Affable eff (Aff eff) where
  fromAff = id

instance affableFree :: (Affable eff f) => Affable eff (Free f) where
  fromAff = liftF <<< fromAff

instance monadAffContT :: (Affable eff m, Monad m) => Affable eff (ContT r m) where
  fromAff = lift <<< fromAff

instance monadAffExceptT :: (Affable eff m, Monad m) => Affable eff (ExceptT e m) where
  fromAff = lift <<< fromAff

instance monadAffListT :: (Affable eff m, Monad m) => Affable eff (ListT m) where
  fromAff = lift <<< fromAff

instance monadAffMaybe :: (Affable eff m, Monad m) => Affable eff (MaybeT m) where
  fromAff = lift <<< fromAff

instance monadAffReader :: (Affable eff m, Monad m) => Affable eff (ReaderT r m) where
  fromAff = lift <<< fromAff

instance monadAffRWS :: (Affable eff m, Monad m, Monoid w) => Affable eff (RWST r w s m) where
  fromAff = lift <<< fromAff

instance monadAffState :: (Affable eff m, Monad m) => Affable eff (StateT s m) where
  fromAff = lift <<< fromAff

instance monadAffWriter :: (Affable eff m, Monad m, Monoid w) => Affable eff (WriterT w m) where
  fromAff = lift <<< fromAff

fromEff :: forall eff f a. (Affable eff f) => Eff eff a -> f a
fromEff eff = fromAff (liftEff eff :: Aff eff a)
