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
-- ^Main rendering function.
renderGame = renderCube

---------------------------------------------------------------------
-- Rendering the Rubiks cube

renderCube :: T.Game -> G.Picture
-- ^Given a rotation matrix, convert a Rubiks cube model into a Gloss
-- picture that can be viewed. The Rubiks cube is always pushed back
-- sufficiently far so as to always be fully behind the screen.
renderCube g = G.pictures . snd . unzip     -- Render the final cube image
               . reverse . sortOn fst       -- Rendering order of squares
               . map (renderSquare g)       -- is based on the depth cues.
               . M.cubeToSquares (M.positionCube g)
               $ T.cube g

---------------------------------------------------------------------
-- Rendering squares
-- Squares are uniform finite planes representing the exposed face
-- each cell in the Rubiks cube. These are used to build up each face
-- of the cube, which are then used to build the cube.

renderSquare :: T.Game -> T.Square -> (Float, G.Picture)
-- ^Provided a screen distance, convert a renderable square to a
-- Gloss polygon together with a depth cue. Antialiasing is mimiced
-- by applying a semi-transparent border.
renderSquare g s = ( depth, fill <> brdr )
    where clr  = squareColor g s
          fill = G.color clr . G.polygon $ ps
          brdr = G.color (G.withAlpha 0.5 clr) . G.lineLoop $ ps
          ps   = [ (x,y) | (x,y,_) <- T.points . M.project (T.toScreen g) $ s ]
          depth = minimum [ z | (_,_,z) <- T.points s ]

squareColor :: T.Game -> T.Square -> G.Color
-- Determine the Gloss color of the square depending on whether it is
-- facing towards or away from the viewer and selected or not.
squareColor g s
    | not . M.isFacingViewer d $ s = T.renderColor g . T.back $ s
    | isSelected                   = G.bright . T.renderColor g . T.front $ s
    | otherwise                    = T.renderColor g . T.front $ s
    where d          = T.toScreen g
          isSelected = case T.mode g of
                            T.Selected lc -> lc == T.locus s
                            otherwise     -> False
