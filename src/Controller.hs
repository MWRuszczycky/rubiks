module Controller
    ( routeEvent
    ) where

import qualified Graphics.Gloss.Interface.IO.Interact as G
import qualified Graphics.Gloss                       as G
import qualified Model.Types                          as T
import qualified Model.Geometry                       as M
import qualified Model.Graphics                       as M
import Data.List                                           ( foldl' )

routeEvent :: G.Event -> T.Game -> T.Game
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down _ xy    ) g
    = routeClick g xy
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Up   _ _     ) g
    = g { T.mode = T.Idle }
routeEvent (G.EventMotion xy)                                        g
    = rotateCube g (T.mode g) xy
routeEvent _                                                         g
    = g

routeClick :: T.Game -> G.Point -> T.Game
routeClick g (x,y) = g { T.mode = T.RotationMove (x,y) }
    -- let d   = T.toScreen g
    --     sqs = map (project d) . filter ( isFacingviewer d ) $ M.cubeToSquares

rotateCube :: T.Game -> T.Mode -> (Float, Float) -> T.Game
rotateCube g T.Idle       _      = g
rotateCube g (T.RotationMove (x,y)) (x',y') =
    let dtx = (x - x') / 100
        dty = (y' - y) / 100
        r   = foldr M.prodMM (T.rotation g) [ M.rotXMat dty , M.rotYMat dtx ]
      in  g { T.mode  = T.RotationMove (x', y')
            , T.rotation = r
            }
