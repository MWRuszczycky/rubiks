module Controller
    ( routeEvent
    , updateTime
    ) where

import qualified Graphics.Gloss.Interface.IO.Interact as G
import qualified Graphics.Gloss                       as G
import qualified Model                                as M

routeEvent :: G.Event -> M.Game -> M.Game
routeEvent _ = id

updateTime :: Float -> M.Game -> M.Game
updateTime dt = id
