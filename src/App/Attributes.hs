module App.Attributes where

import qualified Brick.Widgets.List as L
import Brick.AttrMap (AttrMap, attrMap, attrName, AttrName)
import Brick.Util (on)
import qualified Graphics.Vty as V

selectedForPlacingAttr :: AttrName
selectedForPlacingAttr = attrName "selectedForPlacing"

doubleLetterAttr :: AttrName
doubleLetterAttr = attrName "doubleLetter"

tripleLetterAttr :: AttrName
tripleLetterAttr = attrName "tripleLetter"

doubleWordAttr :: AttrName
doubleWordAttr = attrName "doubleWord"

tripleWordAttr :: AttrName
tripleWordAttr = attrName "tripleWord"

defaultAttr :: AttrName
defaultAttr = attrName "default"

attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
    [ (defaultAttr, V.defAttr)
    , (L.listAttr, V.defAttr)
    , (L.listSelectedFocusedAttr, V.defAttr `V.withForeColor` V.yellow)
    , (doubleLetterAttr, V.defAttr `V.withForeColor` V.cyan)
    , (tripleLetterAttr, V.defAttr `V.withForeColor` V.blue)
    , (doubleWordAttr, V.defAttr `V.withForeColor` V.rgbColor (234 :: Int) 167 163)
    , (tripleWordAttr, V.defAttr `V.withForeColor` V.red)
    , (selectedForPlacingAttr, V.black `on` V.yellow)
    ]
