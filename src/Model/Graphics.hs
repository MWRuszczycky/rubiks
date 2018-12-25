module Model.Graphics
    ( -- Rendering to 3D-squares
      cubeToSquares
    , faceToSquares
      -- Base 3D-representations
    , baseFace
      -- 3D-Transformations
    , moveSquare
    , positionCube
    , positionFace
    , project
      -- 3D-Queries
    , isFacingViewer
    , isOnSquare
    ) where

-- =============================================================== --
-- Functions for converting the internal model of the Rubiks cube
-- into a 3D-representation using the Square type.
--
-- The screen uses a left-handed coordinate frame so that positive-z
-- extends into the screen with positive-y up and positive-x right
-- consistent with the coordinate model used by Gloss.
--
-- The cube should always be fully in front of the screen and the
-- viewer should always be fully behind the screen.
-- =============================================================== --

import qualified Model.Geometry as M
import qualified Model.Cube     as M
import Data.List                     ( foldl'         )
import Model.Types                   ( Axis      (..)
                                     , Cell      (..)
                                     , Color     (..)
                                     , Cube      (..)
                                     , Face      (..)
                                     , Game      (..)
                                     , Layer     (..)
                                     , Locus     (..)
                                     , Matrix    (..)
                                     , Pole      (..)
                                     , Rotation  (..)
                                     , Square    (..)
                                     , Transform (..)
                                     , Vec3      (..)
                                     , Path3D    (..) )

---------------------------------------------------------------------
-- Rendering each face of the cube and the cube itself as Squares

cubeToSquares :: Transform -> Cube -> [Square]
-- ^Given a positioning and orienting function for the cube, convert
-- the cube from the model representation to a completed
-- 3D-representation using Squares ready for either interrogation by
-- the Controller or rendering by the View.
cubeToSquares t c = let go = map (moveSquare t)
                    in  concatMap go [ faceToSquares XAxis Pos c
                                     , faceToSquares XAxis Neg c
                                     , faceToSquares YAxis Pos c
                                     , faceToSquares YAxis Neg c
                                     , faceToSquares ZAxis Pos c
                                     , faceToSquares ZAxis Neg c
                                     ]

faceToSquares :: Axis -> Pole -> Cube -> [Square]
-- ^Extract each face from the cube and position accordingly in the
-- initial standard orientation of the model representation.
faceToSquares a p = map (moveSquare (positionFace a p) )
                    . baseFace a p
                    . M.cubeFace a p

---------------------------------------------------------------------
-- Base 3D-representations of cube and cell faces (as Squares) prior
-- to any geometry transforms.

baseFace :: Axis -> Pole -> Face -> [Square]
-- ^Convert a cube face into a list of squares with rendering
-- information. The squares in each face are separated by 4 pixel
-- spacers. Since each square is 40 x 40 pixels, the face is
-- 128 x 128 pixels.
baseFace a p cs = [ baseSquare a p (i,j) (cs !! i !! j) | i <- [0..2]
                                                        , j <- [0..2] ]

baseSquare :: Axis -> Pole -> (Int, Int) -> Color -> Square
-- ^Square flat on the screen moved to its position in the cube face.
-- The points are ordered so that the normal vector points out of
-- the cube in the left-handed coordinate frame.
baseSquare a p (i,j) c = Square ( Locus a p (i,j) ) c Hidden coord
    where toY y = fromIntegral $ 44 * ( 1 - y )
          toX   = negate . toY
          coord = M.translatePath (toX j, toY i, 0) [ ( -20, -20, 0 )
                                                    , ( -20,  20, 0 )
                                                    , (  20,  20, 0 )
                                                    , (  20, -20, 0 )
                                                    ]

---------------------------------------------------------------------
-- Transforms of 3D-representations

positionCube :: Game -> Transform
-- ^Given the current game state, figure out how to position and
-- scale he cube in three dimensions making sure it is always behind
-- the screen.
positionCube g = M.scalePath (scaling g)
                 . M.translatePath cubeCenter
                 . M.rotatePath (rotation g)
    where cubeCenter = (0,0,100)  -- Initial vector from screen to cube center

positionFace :: Axis -> Pole -> Transform
-- ^Position each face of the cube from the standard orientation. The
-- normal vector for the face is assumed to be initially pointing in
-- the negative-z direction.
positionFace XAxis Pos = M.translatePath (66, 0,  0) . M.rotatePath
                             ( M.rotYMat (pi/2) )
positionFace XAxis Neg = M.translatePath (-66, 0, 0) . M.rotatePath
                             ( M.prodMM ( M.rotYMat (-pi/2) )
                                        ( M.rotZMat (-pi/2) ) )
positionFace YAxis Pos = M.translatePath (0, 66,  0) . M.rotatePath
                             ( M.rotXMat (-pi/2) )
positionFace YAxis Neg = M.translatePath (0,-66,  0) . M.rotatePath
                             ( M.prodMM ( M.rotXMat (pi/2) )
                                        ( M.rotZMat (pi/2) ) )
positionFace ZAxis Pos = M.translatePath (0,  0, 66) . M.rotatePath
                             ( M.prodMM ( M.rotXMat pi     )
                                        ( M.rotZMat (pi/2) ) )
positionFace ZAxis Neg = M.translatePath (0,  0,-66)

moveSquare :: Transform -> Square -> Square
-- ^Helper function for positioning a Square according to some
-- transformation function.
moveSquare go s = s { points = go $ points s}

project :: Float -> Square -> Square
-- ^Project a Square in 3D-space onto the screen, which is at z = 0.
-- Squares are assumed to be fully behind the screen.
project d s = s { points = map go . points $ s}
    where go (x,y,z) | z > 0     = ( u * x, u * y, 0 )
                     | otherwise = ( x, y, 0 )
                     where u = d / ( d + z )

---------------------------------------------------------------------
-- 3D-Queries

isFacingViewer :: Float -> Square -> Bool
-- ^Given a screen distance determine whether a square is facing the
-- viewer or facing away from the viewer. This is determined by
-- finding the normal vector of the square face that points in the
-- exposed direction, as well as the vector from the viewer to the
-- center of the square. If the dot-product of these two vectors is
-- negative, then the exposed face is facing the viewer, otherwise
-- the viewer is seeing the back of the square.
isFacingViewer d (Square _ _ _ ps) = M.dot n e < 0
    where (t:u:v:w:_) = map (+ (0,0,d)) ps
          n           = M.cross (u - t) (v - u)
          e           = foldl' (+) (0,0,0) [t, u, v, w]

isOnSquare :: Float -> (Float, Float) -> Square -> Bool
-- ^Determine whether the mouse is on top of a facing square as
-- projected onto the screen.
isOnSquare d xy s = isFacingViewer d s && isInside s
    where isInside = M.inConvex2D xy . points . project d
