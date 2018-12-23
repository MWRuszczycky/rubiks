module Model.Graphics
    ( faceToSquares
    , positionFace
    , baseFace
    , moveSquare
    , baseSquare
    , isFacingViewer
    ) where

-- =============================================================== --
-- Functions for converting the internal model of the Rubiks cube
-- into a 3D-representation and 2D-projection.
-- =============================================================== --

import qualified Model.Geometry as M
import qualified Model.Cube     as M
import Data.List                     ( foldl'        )
import Model.Types                   ( Axis     (..)
                                     , Cell     (..)
                                     , Color    (..)
                                     , Cube     (..)
                                     , Face     (..)
                                     , Layer    (..)
                                     , Matrix   (..)
                                     , Pole     (..)
                                     , Rotation (..)
                                     , Square   (..)
                                     , Vec3     (..)
                                     , Path3D   (..) )

---------------------------------------------------------------------
-- Rendering each face of the cube

faceToSquares :: Axis -> Pole -> Cube -> [Square]
-- ^Extract each face from the cube and position accordingly in the
-- initial unrotated state of the model representation.
faceToSquares a p = map (moveSquare (positionFace a p) )
                    . baseFace
                    . M.cubeFace a p

---------------------------------------------------------------------
-- Base 3D-representations of cube and cell faces (as Squares) prior
-- to any geometry transforms.

baseFace :: Face -> [Square]
-- ^Convert a cube face into a list of squares with rendering
-- information. The face is created in the standard position and thus
-- faces the negative pole of the z-axis and lies in the (x,y)-plane.
-- The squares in each face are separated by 4 pixel spacers. Since
-- each square is 40 x 40 pixels, the face is 128 x 128 pixels.
baseFace cs = map posSq sqs
    where go x          = fromIntegral $ 44 * ( x - 1 )
          posSq (s,i,j) = moveSquare ( M.translatePath (go j, go i, 0) ) s
          sqs           = [ (baseSquare (cs !! i !! j) Hidden, i, j)
                            | i <- [0..2] , j <- [0..2] ]

baseSquare :: Color -> Color -> Square
-- ^Initial standard form the square given its exposed-facing-front
-- color, and its hidden-back color. Squares are 40 x 40 pixels.
baseSquare f b = Square f b [ ( -20, -20, 0 )
                            , (  20, -20, 0 )
                            , (  20,  20, 0 )
                            , ( -20,  20, 0 )
                            ]

---------------------------------------------------------------------
-- Transforms of 3D-representations

positionFace :: Axis -> Pole -> Path3D -> Path3D
-- ^Position each face of the cube in the unrotated state according
-- to its axis and pole. Each face is generated as lying in the (x,y)
-- plane facing the negative pole of the z-axis. It needs to be
-- rotated to the correct pole and then translated to the correct
-- position on the cube. For example, the positve-x face needs to be
-- rotated minus 90-degrees along the y-axis, which points down, and
-- then pushed right half the width of the cube (64 pixels). An extra
-- 2 pixels are added to each face position to prevent orthogonal
-- edges from touching. This looks nicer and also provides better
-- depth cues when rendering the faces in order of depth.
positionFace XAxis Pos = M.translatePath (66, 0,  0)
                         . M.rotatePath ( M.rotYMat (-pi/2) )
positionFace XAxis Neg = M.translatePath (-66, 0, 0)
                         . M.rotatePath ( M.rotYMat (pi/2)  )
positionFace YAxis Pos = M.translatePath (0, 66,  0)
                         . M.rotatePath ( M.rotXMat (pi/2)  )
positionFace YAxis Neg = M.translatePath (0,-66,  0)
                         . M.rotatePath ( M.rotXMat (-pi/2) )
positionFace ZAxis Pos = M.translatePath (0,  0, 66)
                         . M.rotatePath ( M.rotXMat (pi)    )
positionFace ZAxis Neg = M.translatePath (0,  0,-66)

moveSquare :: (Path3D -> Path3D) -> Square -> Square
-- ^Helper function for positioning a Square according to some
-- transformation function.
moveSquare go s = s { points = go $ points s}

---------------------------------------------------------------------
-- 3D-Functions

isFacingViewer :: Float -> Square -> Bool
-- ^Given a screen distance determine whether a square is facing the
-- viewer or facing away from the viewer. This is determined by
-- finding the normal vector of the square face that points in the
-- exposed direction, as well as the vector from the viewer to the
-- center of the square. If the dot-product of these two vectors is
-- negative, then the exposed face is facing the viewer, otherwise
-- the viewer is seeing the back of the square. Remember that
-- positive-y points down, positive-x points right, positive-z
-- points into the screen and the origin as at the screen with the
-- viewer a negative z-distance d back from it.
isFacingViewer d (Square _ _ ps) = M.dot n e < 0
    where (t:u:v:w:_) = map (+ (0,0,d)) ps
          n           = M.cross (w - t) (u - t)
          e           = foldl' (+) (0,0,0) [t, u, v, w]
