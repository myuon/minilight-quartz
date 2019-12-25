module MiniLight.Quartz where

import Control.Monad.Catch
import qualified Data.Registry as R
import qualified Data.HashMap.Strict as HM
import Control.Lens
import MiniLight

registerComponent
  :: ( HasLoaderEnv env
     , HasLightEnv env
     , MonadIO m
     , MonadMask m
     , ComponentUnit c
     )
  => c
  -> LightT env m Component
registerComponent cu = do
  uuid <- newUID
  comp <- newComponent uuid cu

  reg  <- view _registry
  R.register reg uuid comp

  return comp
