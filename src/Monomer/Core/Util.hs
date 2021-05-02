{-# LANGUAGE LambdaCase #-}

module Monomer.Core.Util where

import Control.Lens ((&), (^.), (.~), (?~))
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (cast)
import Data.Sequence (Seq(..))

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Monomer.Core.BasicTypes
import Monomer.Core.Style
import Monomer.Core.WidgetTypes

import qualified Monomer.Lens as L

globalKeyPath :: WidgetEnv s e -> Text -> Maybe Path
globalKeyPath wenv key = fmap (^. L.info . L.path) node where
  node = Map.lookup (WidgetKey key) (wenv ^. L.globalKeys)

globalKeyWidgetId :: WidgetEnv s e -> Text -> Maybe WidgetId
globalKeyWidgetId wenv key = fmap (^. L.info . L.widgetId) node where
  node = Map.lookup (WidgetKey key) (wenv ^. L.globalKeys)

widgetTreeDesc :: Int -> WidgetNode s e -> String
widgetTreeDesc level node = desc where
  desc = nodeDesc level node ++ "\n" ++ childDesc
  childDesc = foldMap (widgetTreeDesc (level + 1)) (_wnChildren node)

nodeDesc :: Int -> WidgetNode s e -> String
nodeDesc level node = infoDesc (_wnInfo node) where
  spaces = replicate (level * 2) ' '
  infoDesc info =
    spaces ++ "type: " ++ show (_wniWidgetType info) ++ "\n" ++
    spaces ++ "path: " ++ show (_wniPath info) ++ "\n" ++
    spaces ++ "vp: " ++ rectDesc (_wniViewport info) ++ "\n" ++
    spaces ++ "req: " ++ show (_wniSizeReqW info, _wniSizeReqH info) ++ "\n"
  rectDesc r = show (_rX r, _rY r, _rW r, _rH r)

widgetInstTreeDesc :: Int -> WidgetInstanceNode -> String
widgetInstTreeDesc level node = desc where
  desc = nodeInstDesc level node ++ "\n" ++ childDesc
  childDesc = foldMap (widgetInstTreeDesc (level + 1)) (_winChildren node)

nodeInstDesc :: Int -> WidgetInstanceNode -> String
nodeInstDesc level node = infoDesc (_winInfo node) where
  spaces = replicate (level * 2) ' '
  infoDesc info =
    spaces ++ "type: " ++ show (_wniWidgetType info) ++ "\n" ++
    spaces ++ "path: " ++ show (_wniPath info) ++ "\n" ++
    spaces ++ "vp: " ++ rectDesc (_wniViewport info) ++ "\n" ++
    spaces ++ "req: " ++ show (_wniSizeReqW info, _wniSizeReqH info) ++ "\n"
  rectDesc r = show (_rX r, _rY r, _rW r, _rH r)

treeInstDescFromNode :: WidgetEnv s e -> Int -> WidgetNode s e -> String
treeInstDescFromNode wenv level node = widgetInstTreeDesc level nodeInst  where
  nodeInst = widgetGetInstanceTree (node ^. L.widget) wenv node

isResizeWidgets :: WidgetRequest s e -> Bool
isResizeWidgets ResizeWidgets = True
isResizeWidgets _ = False

isRenderOnce :: WidgetRequest s e -> Bool
isRenderOnce RenderOnce{} = True
isRenderOnce _ = False

isRenderEvery :: WidgetRequest s e -> Bool
isRenderEvery RenderEvery{} = True
isRenderEvery _ = False

isRenderStop :: WidgetRequest s e -> Bool
isRenderStop RenderStop{} = True
isRenderStop _ = False

isFocusRequest :: WidgetRequest s e -> Bool
isFocusRequest MoveFocus{} = True
isFocusRequest SetFocus{} = True
isFocusRequest _ = False

isIgnoreParentEvents :: WidgetRequest s e -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: WidgetRequest s e -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isRunTask :: WidgetRequest s e -> Bool
isRunTask RunTask{} = True
isRunTask _ = False

isResizeResult ::  Maybe (WidgetResult s e) -> Bool
isResizeResult result = isJust resizeReq where
  requests = maybe Empty (^. L.requests) result
  resizeReq = Seq.findIndexL isResizeWidgets requests

isMacOS :: WidgetEnv s e -> Bool
isMacOS wenv = _weOs wenv == "Mac OS X"

seqStartsWith :: Eq a => Seq a -> Seq a -> Bool
seqStartsWith prefix seq = Seq.take (length prefix) seq == prefix

seqCatMaybes :: Seq (Maybe a) -> Seq a
seqCatMaybes Empty = Empty
seqCatMaybes (x :<| xs) = case x of
  Just val -> val :<| seqCatMaybes xs
  _ -> seqCatMaybes xs

eventsFromReqs :: Seq (WidgetRequest s e) -> Seq e
eventsFromReqs reqs = seqCatMaybes mevents where
  mevents = flip fmap reqs $ \case
    RaiseEvent ev -> cast ev
    _ -> Nothing

maxNumericValue :: (RealFloat a) => a
maxNumericValue = x where
  n = floatDigits x
  b = floatRadix x
  (_, u) = floatRange x
  x = encodeFloat (b^n - 1) (u - n)

restrictValue :: Ord a => a -> a -> a -> a
restrictValue minVal maxVal value = max minVal (min maxVal value)
