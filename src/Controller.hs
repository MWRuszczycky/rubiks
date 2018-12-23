module Controller
    ( routeEvent
    ) where

import qualified Graphics.Gloss.Interface.IO.Interact as G
import qualified Graphics.Gloss                       as G
import qualified Model.Types                          as T
import qualified Model.Geometry                       as M
import Data.List                                           ( foldl' )

routeEvent :: G.Event -> T.Game -> T.Game
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down _ xy    ) g
    = g { T.rotMove = Just xy }
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Up   _ _     ) g
    = g { T.rotMove = Nothing }
routeEvent (G.EventMotion xy)                                        g
    = rotateCube g (T.rotMove g) xy
routeEvent _                                                         g
    = g

rotateCube :: T.Game -> Maybe (Float, Float) -> (Float, Float) -> T.Game
rotateCube g Nothing       _      = g
rotateCube g (Just (x,y)) (x',y') =
    let dtx = (x - x') / 100
        dty = (y' - y) / 100
        r   = foldr M.prodMM (T.rotation g) [ M.rotXMat dty , M.rotYMat dtx ]
      in  g { T.rotMove  = Just (x', y')
            , T.rotation = r
            }
