module Controller
    ( routeEvent
    ) where

-- =============================================================== --
-- Interface with Gloss for handling input
-- =============================================================== --

import qualified Graphics.Gloss.Interface.IO.Interact as G
import qualified Graphics.Gloss                       as G
import qualified Model.Types                          as T
import qualified Model.Geometry                       as M
import qualified Model.Graphics                       as M
import qualified Model.Cube                           as M
import Model.Resources                                     ( solved     )
import Data.Char                                           ( isDigit
                                                           , digitToInt )
import System.Random                                       ( randoms
                                                           , split      )
import Data.List                                           ( foldl'
                                                           , find       )

-- =============================================================== --
-- Event handlers

routeEvent :: G.Event -> T.Game -> T.Game
-- ^Main event router.
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down  _ xy) =
    leftMouseDown xy
routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Up    _ _ ) =
    leftMouseUp
routeEvent (G.EventKey (G.MouseButton G.RightButton) G.Down _ xy) =
    rightMouseDown xy
routeEvent (G.EventKey (G.MouseButton G.RightButton) G.Up   _ _ ) =
    rightMouseUp
routeEvent (G.EventKey (G.SpecialKey G.KeySpace)     G.Down _ _ ) =
    undoLast
routeEvent (G.EventKey (G.Char k)                    G.Down _ _ ) =
    routeCharDown k
routeEvent (G.EventMotion xy)                                     =
    mouseMove xy
routeEvent (G.EventResize wh)                                     =
    changeDimensions wh
routeEvent _                                                      =
    id

---------------------------------------------------------------------
-- Handlers and routers for specific input events

routeCharDown :: Char -> T.Game -> T.Game
-- ^User pressed a character key.
routeCharDown 's' = resetCube
routeCharDown c
    | isDigit c = randomizeCube n
    | otherwise = id
    where n | c == '0'  = 10
            | otherwise = digitToInt c

leftMouseUp :: T.Game -> T.Game
-- ^Left mouse button released.
leftMouseUp g = g { T.mode = T.Idle }

leftMouseDown :: G.Point -> T.Game -> T.Game
-- ^Left mouse button depressed.
leftMouseDown xy g = case getLocus xy g of
                          Nothing -> g { T.mode = T.RotationMove xy }
                          Just lc -> g { T.mode = T.Selected lc    }

rightMouseUp :: T.Game -> T.Game
-- ^Right mouse botton released.
rightMouseUp g = g { T.mode = T.Idle }

rightMouseDown :: G.Point -> T.Game -> T.Game
-- ^Right mouse button depressed.
rightMouseDown p g = let s = T.scaling g
                     in  g { T.mode = T.ScalingMove s p }

mouseMove :: (Float, Float) -> T.Game -> T.Game
-- ^Mouse moved to a new position, so, figure out what to do.
mouseMove xy g = case T.mode g of
                      T.Idle              -> g
                      T.RotationMove xy'  -> rotateCube xy' xy g
                      T.Selected lc       -> rotateLayer xy lc g
                      T.ScalingMove s xy' -> scaleCube xy' xy s g

-- =============================================================== --
-- User actions called via input events

undoLast :: T.Game -> T.Game
-- ^Undo the last layer rotation of the cube.
undoLast g = let c = T.cube g
             in  case T.moves g of
                      []   -> g
                      m:ms -> g { T.cube  = M.rotateLayer (M.undo m) c
                                , T.moves = ms
                                }

getLocus :: G.Point -> T.Game -> Maybe T.Locus
-- ^Determine which cell the user is trying to select if any using
-- the supplied mouse coordinates.
getLocus xy g = T.locus <$> find ( M.isOnSquare (T.toScreen g) xy ) sqs
    where sqs = M.cubeToSquares (M.positionCube g) . T.cube $ g

rotateCube :: G.Point -> G.Point -> T.Game -> T.Game
-- ^Rotate cube based on movement of mouse between two coordinates.
rotateCube (x,y) (x',y') g =
    let dtx = (x' - x) / 100
        dty = (y - y') / 100
        r   = foldr M.prodMM (T.rotation g) [ M.rotXMat dty , M.rotYMat dtx ]
    in  g { T.mode  = T.RotationMove (x', y') , T.rotation = r }

rotateLayer :: G.Point -> T.Locus -> T.Game -> T.Game
-- ^Given a previously selected cell and associated locus, determine
-- whether the mouse has moved to a new cell and if so whether a
-- layer should be rotated. If so, rotate the layer.
rotateLayer xy lc g = maybe g id go
    where go = do lc' <- getLocus xy g
                  mv  <- M.getMove lc lc'
                  return g { T.cube  = M.rotateLayer mv . T.cube $ g
                           , T.mode  = T.Selected lc'
                           , T.moves = mv : T.moves g
                           }

resetCube :: T.Game -> T.Game
-- ^Clear all moves and return cube to the solved state.
resetCube g = g { T.cube  = solved
                , T.moves = []
                }

scaleCube :: G.Point -> G.Point -> Float -> T.Game -> T.Game
-- ^Scale the cube given an initial scale factor a movement of the
-- mouse between two y-coordinates.
scaleCube (_,y) (_,y') s g
    | s' > 0.1  = g { T.scaling = s' }
    | otherwise = g
    where dy = (y - y') / 100
          d  = T.toScreen g
          s' = dy + s

changeDimensions :: (Int, Int) -> T.Game -> T.Game
-- ^Update the known dimensions of the window.
changeDimensions (w,h) g = g { T.dim = (w,h) }

randomizeCube :: Int -> T.Game -> T.Game
-- ^Apply n random moves to the cube in its current state and clear
-- all previous moves from the undo list.
randomizeCube n game = let (g1,g2) = split . T.gen $ game
                           mvs     = take n . randoms $ g1
                           c       = T.cube game
                       in  game { T.gen   = g2
                                , T.cube  = foldr M.rotateLayer c mvs
                                , T.moves = []
                                }
