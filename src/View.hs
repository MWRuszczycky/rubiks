module View
    ( renderGame
    ) where

-- =============================================================== --
-- Interface with Gloss for rendering to output
-- =============================================================== --

import qualified Graphics.Gloss as G
import qualified Model.Types    as T
import qualified Model.Graphics as M
import qualified Model.Geometry as M
import Data.List                     ( sortOn   )

-- =============================================================== --
-- Rendering functions

renderGame :: T.Game -> G.Picture
-- ^Main rendering hub.
renderGame g = renderCube (T.rotation g) . T.cube $ g

---------------------------------------------------------------------
-- Rendering the Rubiks cube

renderCube :: T.Matrix -> T.Cube -> G.Picture
-- ^Given a rotation matrix, convert a Rubiks cube model into a Gloss
--- picture that can be viewed on the screen. The Rubiks cube is
-- pushed back sufficiently far so as not to collide with the view
-- screen after applying the rotation.
renderCube m c =
    let move   = M.moveSquare (M.translatePath (0,0,100) . M.rotatePath m)
        render = renderSquare 250 . move
    in  G.pictures . snd . unzip  -- Cleanup after sorting and render
        . reverse . sortOn fst    -- Sort on the depth cues for z-placement
        . map render              -- Rotate all faces together and push back
        . concat $ [ M.faceToSquares T.XAxis T.Pos c
                   , M.faceToSquares T.XAxis T.Neg c
                   , M.faceToSquares T.YAxis T.Pos c
                   , M.faceToSquares T.YAxis T.Neg c
                   , M.faceToSquares T.ZAxis T.Pos c
                   , M.faceToSquares T.ZAxis T.Neg c
                   ]

---------------------------------------------------------------------
-- Rendering squares
-- Squares are uniform finite planes representing the exposed face
-- each cell in the Rubiks cube. These are used to build up each face
-- of the cube, which are then used to build the cube.

renderSquare :: Float -> T.Square -> (Float, G.Picture)
-- ^Provided a focal distance, convert a renderable square to a Gloss
-- polygon together with a depth cue.
renderSquare d s = ( depth, pic )
    where pic   = colorSquare d s . G.polygon . project d $ T.points s
          depth = minimum [ z | (_,_,z) <- T.points s ]

colorSquare :: Float -> T.Square -> G.Picture -> G.Picture
-- Color the square depending on whether it is facing towards or away
-- from the viewer.
colorSquare d s
    | M.isFacingViewer d s = G.color (renderColor . T.front $ s)
    | otherwise            = G.color (renderColor . T.back  $ s)

---------------------------------------------------------------------
-- Gloss-Model interface functions

project :: Float -> T.Path3D -> G.Path
-- ^Convert a Model path in 3-dimensions to a projected Gloss path.
-- The initial argument is the focal length.
project d = map go
    where go (x,y,z) | z > 0     = ( u * x, u * y)
                     | otherwise = ( x, y )
                     where u = d / ( d + z )

renderColor :: T.Color -> G.Color
-- ^Map Model colors to Gloss colors.
renderColor T.Red    = G.makeColor 0.78 0.15 0.10 1.0
renderColor T.White  = G.makeColor 0.75 0.75 0.75 1.0
renderColor T.Yellow = G.makeColor 1.00 0.85 0.34 1.0
renderColor T.Green  = G.makeColor 0.00 0.54 0.36 1.0
renderColor T.Blue   = G.makeColor 0.00 0.64 1.00 1.0
renderColor T.Orange = G.makeColor 1.00 0.38 0.00 1.0
renderColor T.Hidden = G.makeColor 0.10 0.10 0.10 1.0
