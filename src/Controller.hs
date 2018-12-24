module Controller
    ( routeEvent
    ) where

import qualified Graphics.Gloss.Interface.IO.Interact as G
import qualified Graphics.Gloss                       as G
import qualified Model.Types                          as T
import qualified Model.Geometry                       as M
import qualified Model.Graphics                       as M
import qualified Model.Cube                           as M
import Data.List                                           ( foldl'
                                                           , find   )

routeEvent :: G.Event -> T.Game -> T.Game
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down _ xy ) = mouseDown xy
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Up   _ _  ) = mouseUp
routeEvent (G.EventMotion xy)                                     = movement xy
routeEvent _                                                      = id

mouseUp :: T.Game -> T.Game
mouseUp g = g { T.mode = T.Idle }

mouseDown :: G.Point -> T.Game -> T.Game
mouseDown xy g = case getLocus xy g of
                      Nothing -> g { T.mode = T.RotationMove xy }
                      Just lc -> g { T.mode = T.Selected lc    }

movement :: (Float, Float) -> T.Game -> T.Game
movement xy g = case T.mode g of
                     T.Idle             -> g
                     T.RotationMove xy' -> rotateCube xy' xy g
                     T.Selected lc      -> rotateLayer xy lc g

---------------------------------------------------------------------
-- Helper functions

getLocus :: G.Point -> T.Game -> Maybe T.Locus
getLocus xy g = T.locus <$> find ( M.isOnSquare (T.toScreen g) xy ) sqs
    where sqs = M.cubeToSquares (M.positionCube g) . T.cube $ g

rotateCube :: (Float, Float) -> (Float, Float) -> T.Game -> T.Game
rotateCube (x,y) (x',y') g =
    let dtx = (x' - x) / 100
        dty = (y - y') / 100
        r   = foldr M.prodMM (T.rotation g) [ M.rotXMat dty , M.rotYMat dtx ]
    in  g { T.mode  = T.RotationMove (x', y') , T.rotation = r }

rotateLayer :: G.Point -> T.Locus -> T.Game -> T.Game
rotateLayer xy lc g = maybe g id go
    where go = do lc' <- getLocus xy g
                  r   <- M.getMove lc lc'
                  return g { T.cube = M.rotateLayer r . T.cube $ g
                           , T.mode = T.Selected lc'
                           }
