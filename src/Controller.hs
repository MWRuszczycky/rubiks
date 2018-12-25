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
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down _ xy ) =
    leftMouseDown xy
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Up   _ _  ) =
    leftMouseUp
routeEvent (G.EventKey (G.MouseButton G.RightButton) G.Down _ xy) =
    rightMouseDown xy
routeEvent (G.EventKey (G.MouseButton G.RightButton) G.Up   _ _ ) =
    rightMouseUp
routeEvent (G.EventKey (G.SpecialKey G.KeySpace)     G.Down _ _ ) =
    undoLast
routeEvent (G.EventMotion xy)                                     =
    movement xy
routeEvent _                                                      =
    id

leftMouseUp :: T.Game -> T.Game
leftMouseUp g = g { T.mode = T.Idle }

leftMouseDown :: G.Point -> T.Game -> T.Game
leftMouseDown xy g = case getLocus xy g of
                          Nothing -> g { T.mode = T.RotationMove xy }
                          Just lc -> g { T.mode = T.Selected lc    }

rightMouseUp :: T.Game -> T.Game
rightMouseUp g = g { T.mode = T.Idle }

rightMouseDown :: G.Point -> T.Game -> T.Game
rightMouseDown p g = let s = T.scaling g
                     in  g { T.mode = T.ScalingMove s p }

movement :: (Float, Float) -> T.Game -> T.Game
movement xy g = case T.mode g of
                     T.Idle              -> g
                     T.RotationMove xy'  -> rotateCube xy' xy g
                     T.Selected lc       -> rotateLayer xy lc g
                     T.ScalingMove s xy' -> scaleCube xy' xy s g

undoLast :: T.Game -> T.Game
undoLast g = let c = T.cube g
             in  case T.moves g of
                      []   -> g
                      m:ms -> g { T.cube  = M.rotateLayer (M.undo m) c
                                , T.moves = ms
                                }

---------------------------------------------------------------------
-- Helper functions

getLocus :: G.Point -> T.Game -> Maybe T.Locus
getLocus xy g = T.locus <$> find ( M.isOnSquare (T.toScreen g) xy ) sqs
    where sqs = M.cubeToSquares (M.positionCube g) . T.cube $ g

rotateCube :: G.Point -> G.Point -> T.Game -> T.Game
rotateCube (x,y) (x',y') g =
    let dtx = (x' - x) / 100
        dty = (y - y') / 100
        r   = foldr M.prodMM (T.rotation g) [ M.rotXMat dty , M.rotYMat dtx ]
    in  g { T.mode  = T.RotationMove (x', y') , T.rotation = r }

rotateLayer :: G.Point -> T.Locus -> T.Game -> T.Game
rotateLayer xy lc g = maybe g id go
    where go = do lc' <- getLocus xy g
                  mv  <- M.getMove lc lc'
                  return g { T.cube  = M.rotateLayer mv . T.cube $ g
                           , T.mode  = T.Selected lc'
                           , T.moves = mv : T.moves g
                           }

scaleCube :: G.Point -> G.Point -> Float -> T.Game -> T.Game
scaleCube (_,y) (_,y') s g
    | s' > 0.1  = g { T.scaling = s' }
    | otherwise = g
    where dy = (y - y') / 100
          d  = T.toScreen g
          s' = dy + s
