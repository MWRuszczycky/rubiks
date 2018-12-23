module Controller
    ( routeEvent
    ) where

import qualified Graphics.Gloss.Interface.IO.Interact as G
import qualified Graphics.Gloss                       as G
import qualified Model.Types                          as T
import qualified Model.Geometry                       as M
import qualified Model.Graphics                       as M
import Data.List                                           ( foldl'
                                                           , find   )

routeEvent :: G.Event -> T.Game -> T.Game
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down _ xy    ) g
    = mouseDown xy g
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Up   _ _     ) g
    = g { T.mode = T.Idle }
routeEvent (G.EventMotion xy)                                        g
    = movement (T.mode g) xy g
routeEvent _                                                         g
    = g

mouseDown :: G.Point -> T.Game -> T.Game
mouseDown xy g = let sqs = M.cubeToSquares (M.positionCube g) . T.cube $ g
                 in  case find ( M.isOnSquare (T.toScreen g) xy ) sqs of
                          Nothing -> g { T.mode = T.RotationMove xy }
                          Just sq -> g { T.mode = T.Selected xy (T.locus sq) }

movement :: T.Mode -> (Float, Float) -> T.Game -> T.Game
movement T.Idle            _     = id
movement (T.Selected xy _) _     = id
movement (T.RotationMove xy) xy' = rotateCube xy xy'

rotateCube :: (Float, Float) -> (Float, Float) -> T.Game -> T.Game
rotateCube (x,y) (x',y') g =
    let dtx = (x - x') / 100
        dty = (y' - y) / 100
        r   = foldr M.prodMM (T.rotation g) [ M.rotXMat dty , M.rotYMat dtx ]
    in  g { T.mode  = T.RotationMove (x', y') , T.rotation = r }
