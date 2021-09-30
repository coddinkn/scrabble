module App.Attributes where

import qualified Brick.Widgets.List as L
import Brick.AttrMap (AttrMap, attrMap)
import qualified Graphics.Vty as V

attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (L.listAttr, V.defAttr)
    , (L.listSelectedAttr, V.defAttr `V.withForeColor` V.yellow)
    ]
