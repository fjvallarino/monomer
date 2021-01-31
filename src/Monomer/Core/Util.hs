module Monomer.Core.Util where

import Control.Lens ((&), (^.), (.~), (?~))
import Data.Maybe
import Data.Text (Text)
import Data.Sequence (Seq(..))

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Monomer.Core.BasicTypes
import Monomer.Core.Style
import Monomer.Core.WidgetTypes

import qualified Monomer.Lens as L

globalKeyPath :: WidgetEnv s e -> Text -> Maybe Path
globalKeyPath wenv key = fmap (^. L.info . L.path) node where
  node = Map.lookup (WidgetKeyGlobal key) (wenv ^. L.globalKeys)

widgetTreeDesc :: Int -> WidgetNode s e -> String
widgetTreeDesc level node = desc where
  desc = nodeDesc level node ++ "\n" ++ childDesc
  childDesc = foldMap (widgetTreeDesc (level + 1)) (_wnChildren node)

nodeDesc :: Int -> WidgetNode s e -> String
nodeDesc level node = infoDesc (_wnInfo node) where
  spaces = replicate (level * 2) ' '
  infoDesc info =
    spaces ++ "type: " ++ unWidgetType (_wniWidgetType info) ++ "\n" ++
    spaces ++ "path: " ++ show (_wniPath info) ++ "\n" ++
    spaces ++ "vp: " ++ rectDesc (_wniViewport info) ++ "\n" ++
    spaces ++ "ra: " ++ rectDesc (_wniRenderArea info) ++ "\n" ++
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
    spaces ++ "type: " ++ unWidgetType (_wniWidgetType info) ++ "\n" ++
    spaces ++ "path: " ++ show (_wniPath info) ++ "\n" ++
    spaces ++ "vp: " ++ rectDesc (_wniViewport info) ++ "\n" ++
    spaces ++ "ra: " ++ rectDesc (_wniRenderArea info) ++ "\n" ++
    spaces ++ "req: " ++ show (_wniSizeReqW info, _wniSizeReqH info) ++ "\n"
  rectDesc r = show (_rX r, _rY r, _rW r, _rH r)

isResizeWidgets :: WidgetRequest s -> Bool
isResizeWidgets ResizeWidgets = True
isResizeWidgets _ = False

isFocusRequest :: WidgetRequest s -> Bool
isFocusRequest MoveFocus{} = True
isFocusRequest SetFocus{} = True
isFocusRequest _ = False

isIgnoreParentEvents :: WidgetRequest s -> Bool
isIgnoreParentEvents IgnoreParentEvents = True
isIgnoreParentEvents _ = False

isIgnoreChildrenEvents :: WidgetRequest s -> Bool
isIgnoreChildrenEvents IgnoreChildrenEvents = True
isIgnoreChildrenEvents _ = False

isRunTask :: WidgetRequest s -> Bool
isRunTask RunTask{} = True
isRunTask _ = False

isResizeResult ::  Maybe (WidgetResult s e) -> Bool
isResizeResult result = isJust resizeReq where
  requests = maybe Empty (^. L.requests) result
  resizeReq = Seq.findIndexL isResizeWidgets requests

isMacOS :: WidgetEnv s e -> Bool
isMacOS wenv = _weOS wenv == "Mac OS X"

seqStartsWith :: Eq a => Seq.Seq a -> Seq.Seq a -> Bool
seqStartsWith prefix seq = Seq.take (length prefix) seq == prefix

maxNumericValue :: (RealFloat a) => a
maxNumericValue = x where
  n = floatDigits x
  b = floatRadix x
  (_, u) = floatRange x
  x = encodeFloat (b^n - 1) (u - n)

restrictValue :: Ord a => a -> a -> a -> a
restrictValue minVal maxVal value = max minVal (min maxVal value)
