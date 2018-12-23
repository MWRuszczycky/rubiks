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
renderGame = renderCube

---------------------------------------------------------------------
-- Rendering the Rubiks cube

renderCube :: T.Game -> G.Picture
-- ^Given a rotation matrix, convert a Rubiks cube model into a Gloss
-- picture that can be viewed. The Rubiks cube is always pushed back
-- sufficiently far so as to always be fully behind the screen.
renderCube g = let scrDist = T.toScreen g
               in  G.pictures . snd . unzip     -- Render the final cube image
                   . reverse . sortOn fst       -- Rendering order of squares
                   . map (renderSquare scrDist) -- is based on the depth cues.
                   . M.cubeToSquares (M.positionCube g)
                   $ T.cube g

---------------------------------------------------------------------
-- Rendering squares
-- Squares are uniform finite planes representing the exposed face
-- each cell in the Rubiks cube. These are used to build up each face
-- of the cube, which are then used to build the cube.

renderSquare :: Float -> T.Square -> (Float, G.Picture)
-- ^Provided a screen distance, convert a renderable square to a
-- Gloss polygon together with a depth cue. Antialiasing is mimiced
-- by applying a semi-transparent border.
renderSquare d s = ( depth, fill <> border )
    where clr    = squareColor d s
          fill   = G.color clr . G.polygon $ ps
          border = G.color (G.withAlpha 0.5 clr) . G.lineLoop $ ps
          ps     = [ (x, y) | (x,y,_) <- T.points . M.project d $ s ]
          depth  = minimum [ z | (_,_,z) <- T.points s ]

squareColor :: Float -> T.Square -> G.Color -- G.Picture -> G.Picture
-- Determine the Gloss color of the square depending on whether it is
-- facing towards or away from the viewer.
squareColor d s
    | M.isFacingViewer d s = renderColor . T.front $ s
    | otherwise            = renderColor . T.back  $ s

---------------------------------------------------------------------
-- Gloss-Model interface functions

renderColor :: T.Color -> G.Color
-- ^Map Model colors to Gloss colors.
renderColor T.Red    = G.makeColor 0.78 0.15 0.10 1.0
renderColor T.White  = G.makeColor 0.75 0.75 0.75 1.0
renderColor T.Yellow = G.makeColor 1.00 0.85 0.34 1.0
renderColor T.Green  = G.makeColor 0.00 0.54 0.36 1.0
renderColor T.Blue   = G.makeColor 0.00 0.64 1.00 1.0
renderColor T.Orange = G.makeColor 1.00 0.38 0.00 1.0
renderColor T.Hidden = G.makeColor 0.10 0.10 0.10 1.0
