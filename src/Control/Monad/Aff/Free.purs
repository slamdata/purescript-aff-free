module Control.Monad.Aff.Free where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (Free(), liftF)

class Affable eff f where
  fromAff :: forall a. Aff eff a -> f a

instance affableAff :: Affable eff (Aff eff) where
  fromAff = id

instance affableFree :: (Affable eff f) => Affable eff (Free f) where
  fromAff = liftF <<< fromAff

fromEff :: forall eff f a. (Affable eff f) => Eff eff a -> f a
fromEff eff = fromAff (liftEff eff :: Aff eff a)
