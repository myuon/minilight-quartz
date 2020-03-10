{-# LANGUAGE OverloadedStrings #-}
module MiniLight.Quartz where

import Control.Error
import Control.Monad.Catch
import Control.Monad.State hiding (state)
import qualified Control.Monad.Caster as Caster
import Control.Lens hiding ((??))
import qualified Data.Config.Font as Font
import qualified Data.Component.Layer as Layer
import qualified Data.Component.Basic as Basic
import Data.IORef
import Data.Dynamic
import qualified Data.HashMap.Strict as HM
import qualified Data.Registry as R
import qualified Data.Text  as T
import qualified Data.Primitive.Array as A
import qualified Data.Map as M
import GHC.Exts (IsList(..))
import Language.Quartz
import Language.Quartz.AST
import Language.Quartz.Std
import Language.Quartz.Lexer (AlexPosn)
import MiniLight
import Linear
import qualified SDL
import Paths_minilight_quartz

extend9tiles :: Figure -> V2 Int -> MiniLight Figure
extend9tiles fig size = do
  mrenderer <- view _renderer
  target    <- flip mapM mrenderer $ \renderer -> do
    let siz      = fmap toEnum size
    let Just tex = texture fig
    let texSize  = fmap toEnum $ getFigureSize fig

    tinfo  <- SDL.queryTexture tex

    target <- SDL.createTexture renderer
                                (SDL.texturePixelFormat tinfo)
                                SDL.TextureAccessTarget
                                siz
    SDL.rendererRenderTarget renderer SDL.$= Just target
    SDL.textureBlendMode target SDL.$= SDL.BlendAlphaBlend

    let tileSize = fmap (`div` 3) texSize

    forM_ [0 .. 2] $ \ix -> forM_ [0 .. 2] $ \iy -> do
      let targetSize = V2
            (if ix == 1 then siz ^. _x - 2 * tileSize ^. _x else tileSize ^. _x)
            (if iy == 1 then siz ^. _y - 2 * tileSize ^. _y else tileSize ^. _y)
      let targetLoc = V2
            ( if ix == 0
              then 0
              else if ix == 1
                then tileSize ^. _x
                else siz ^. _x - tileSize ^. _x
            )
            ( if iy == 0
              then 0
              else if iy == 1
                then tileSize ^. _y
                else siz ^. _y - tileSize ^. _y
            )

      SDL.copy renderer
               tex
               (Just $ SDL.Rectangle (SDL.P (tileSize * V2 ix iy)) tileSize)
               (Just $ SDL.Rectangle (SDL.P targetLoc) targetSize)

    SDL.rendererRenderTarget renderer SDL.$= Nothing

    return target

  tex <- maybe (return emptyFigure) fromTexture target
  return tex

registerComponent
  :: ( HasLoaderEnv env
     , HasLightEnv env
     , MonadIO m
     , MonadMask m
     , ComponentUnit c
     )
  => T.Text
  -> c
  -> LightT env m Component
registerComponent name cu = do
  uuid <- newUID
  comp <- newComponent uuid cu

  reg  <- view _registry
  R.register reg uuid comp

  tag <- view _tagRegistry
  liftIO $ modifyIORef' tag $ HM.insert name uuid

  return comp

data QuartzComponentState = QuartzComponentState {
  mousePosition :: V2 Int
} deriving (Eq, Show)

data QuartzComponent = QuartzComponent {
  expr :: String,
  state :: QuartzComponentState,
  counter :: Int
}

newQuartzComponent :: QuartzComponent
newQuartzComponent = QuartzComponent
  { expr    = "func main(): array[Figure] { array![] }"
  , state   = QuartzComponentState {mousePosition = 0}
  , counter = 0
  }

data QuartzEvent
  = SetExpr String

instance EventType QuartzEvent where
  getEventType (SetExpr _) = "set_expr"

instance ComponentUnit QuartzComponent where
  figures comp = evalQuartzComponent (expr comp) (state comp)

  onSignal ev = execStateT $ do
    lift $ Basic.emitBasicSignal ev (Basic.Config { Basic.size = V2 640 480, Basic.position = V2 0 0, Basic.visible = True })

    case asSignal ev of
      Just (SetExpr fs) -> do
        modify $ \qc -> qc { expr = fs, counter = counter qc + 1 }
      _ -> return ()

    case asSignal ev of
      Just (Basic.MouseOver p) -> do
        modify $ \qc -> qc { state = (state qc) { mousePosition = p }, counter = counter qc + 1 }
      _ -> return ()

  useCache c1 c2 = counter c1 == counter c2

minilightBindings
  :: QuartzComponentState
  -> M.Map Id ([Dynamic] -> ExceptT FFIExceptions MiniLight (Expr AlexPosn))
minilightBindings state = M.fromList
  [ ( Id ["ffi_loadPicture"]
    , \[d1] -> do
      e1 <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      case unwrapExpr e1 of
        Lit (StringLit s) -> do
          fig <- lift $ picture s
          return $ srcSpanExpr' e1 $ Any $ Dynamic' Nothing $ toDyn
            (fig :: Figure)
    )
  , ( Id ["ffi_extend9tiles"]
    , \[d1, d2, d3] -> do
      e1 <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      e2 <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      e3 <- (fromDynamic d3 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d3
      case (unwrapExpr e1, unwrapExpr e2, unwrapExpr e3) of
        (Any (Dynamic' _ fig), Lit (IntLit x), Lit (IntLit y)) -> do
          fig <- lift
            $ extend9tiles ((\(Just f) -> f) $ fromDynamic fig) (V2 x y)
          return $ srcSpanExpr e1 e3 $ Any $ Dynamic' Nothing $ toDyn
            (fig :: Figure)
    )
  , ( Id ["ffi_text"]
    , \[d1] -> do
      e1 <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      case unwrapExpr e1 of
        Lit (StringLit s) -> lift $ do
          font <- Font.loadFontFrom $ Font.Config
            (FontDescriptor "IPAGothic" (FontStyle False False))
            24
            255
          textTexture <- text font 255 $ T.pack s
          return $ srcSpanExpr' e1 $ Any $ Dynamic' Nothing $ toDyn
            (textTexture :: Figure)
    )
  , ( Id ["ffi_translate"]
    , \[d1, d2, d3] -> do
      e1 <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      e2 <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      e3 <- (fromDynamic d3 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d3
      case (unwrapExpr e1, unwrapExpr e2, unwrapExpr e3) of
        (Lit (IntLit x), Lit (IntLit y), Any (Dynamic' _ fig)) -> do
          let fig' = translate (V2 x y) $ (\(Just f) -> f) $ fromDynamic fig
          return $ srcSpanExpr e1 e3 $ Any $ Dynamic' Nothing $ toDyn
            (fig' :: Figure)
    )
  , ( Id ["ffi_rectangle"]
    , \[d1, d2, d3, d4, d5, d6] -> do
      e1 <- (fromDynamic d1 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d1
      e2 <- (fromDynamic d2 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d2
      e3 <- (fromDynamic d3 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d3
      e4 <- (fromDynamic d4 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d4
      e5 <- (fromDynamic d5 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d5
      e6 <- (fromDynamic d6 :: Maybe (Expr AlexPosn)) ?? InvalidExpr d6
      case
          ( unwrapExpr e1
          , unwrapExpr e2
          , unwrapExpr e3
          , unwrapExpr e4
          , unwrapExpr e5
          , unwrapExpr e6
          )
        of
          (Lit (IntLit w), Lit (IntLit h), Lit (IntLit r), Lit (IntLit g), Lit (IntLit b), Lit (IntLit a))
            -> do
              fig <- lift
                $ rectangleFilled (fmap fromIntegral $ V4 r g b a) (V2 w h)
              return $ srcSpanExpr e1 e6 $ Any $ Dynamic' Nothing $ toDyn
                (fig :: Figure)
    )
  , ( Id ["ffi_mouse_move"]
    , \[] -> do
      let pos = mousePosition state
      return $ exprPos0 $ RecordOf
        "Position"
        [ ("x", exprPos0 (Lit (IntLit (pos ^. _x))))
        , ("y", exprPos0 (Lit (IntLit (pos ^. _y))))
        ]
    )
  ]

evalQuartzComponent
  :: (HasLightEnv env, MonadIO m, MonadMask m)
  => String
  -> QuartzComponentState
  -> LightT env m [Figure]
evalQuartzComponent content state = do
  lib    <- liftIO $ readFile =<< getDataFileName "minilight.qz"
  result <- liftMiniLight
    $ runModuleWith (minilightBindings state) (lib ++ content)

  case result of
    Left  e -> Caster.err e >> return []
    Right m -> do
      let arr = (\(ExprLoc _ _ (Array marr)) -> getMArray marr) m
      imarr <- liftIO $ A.unsafeFreezeArray arr
      let items = toList imarr

      case
          sequence $ map
            (\(ExprLoc _ _ (Any (Dynamic' _ dyn))) -> fromDynamic dyn)
            items
        of
          Nothing  -> Caster.err ("Nothing for Dynamic" :: String) >> return []
          Just fs' -> return fs'

reload
  :: (HasLoaderEnv env, HasLightEnv env, HasLoopEnv env, MonadIO m, MonadMask m)
  => T.Text
  -> LightT env m ()
reload path = do
  fs <- liftIO $ readFile (T.unpack path)
  path @@! SetExpr fs
