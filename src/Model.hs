module Model
    ( -- Getting the faces of the cube
      cellFace
    , cubeFace
      -- Rotating each layer of the cube
    , rotateLayer
      -- Total rotations for visualization
    , prodMM
    , prodMV
    , translatePath
    , rotatePath
    , rotXMat
    , rotYMat
    , rotZMat
    ) where

import Data.List ( transpose     )
import Types     ( Axis     (..)
                 , Cell     (..)
                 , Color    (..)
                 , Cube     (..)
                 , Face     (..)
                 , Layer    (..)
                 , Matrix   (..)
                 , Pole     (..)
                 , Rotation (..)
                 , Vec3     (..)
                 , Path3D   (..) )

-- =============================================================== --
-- Getting the faces of the cube

-- Exported

cellFace :: Axis -> Pole -> Cell -> Color
-- ^Retrieve the color of the cell face given an axis and pole/side.
cellFace XAxis Pos (Cell x _ _ _ _ _) = x
cellFace XAxis Neg (Cell _ x _ _ _ _) = x
cellFace YAxis Pos (Cell _ _ y _ _ _) = y
cellFace YAxis Neg (Cell _ _ _ y _ _) = y
cellFace ZAxis Pos (Cell _ _ _ _ z _) = z
cellFace ZAxis Neg (Cell _ _ _ _ _ z) = z

cubeFace :: Axis -> Pole -> Cube -> Face
-- ^Retrieve the faces of all six cells at the specified pole of the
-- specified axis facing out from the center of the cube.
-- x-positive: pos pole of x-axis, pos z right, pos y down
cubeFace XAxis Pos = (map . map) (cellFace ZAxis Neg)
                     . head . rotateCube YAxis Pos90
-- x-negative: neg pole of x-axis, pos z left,  pos y down
cubeFace XAxis Neg = (map . map) (cellFace ZAxis Neg)
                     . head . rotateCube YAxis Neg90
-- y-positive: pos pole of y-axis, pos x right, pos z down
cubeFace YAxis Pos = (map . map) (cellFace ZAxis Neg)
                     . head . rotateCube XAxis Neg90
-- y-negative: neg pole of y-axis, pos x right, pos z up
cubeFace YAxis Neg = (map . map) (cellFace ZAxis Neg)
                     . head . rotateCube XAxis Pos90
-- z-positive: pos pole of z-axis, pos x right, pos y up
cubeFace ZAxis Pos = (map . map) (cellFace ZAxis Pos)
                     . head . ( \ f -> f . f ) (rotateCube XAxis Pos90)
-- z-negative: neg pole of z-axis, pos x right, pos y down
cubeFace ZAxis Neg = (map . map) (cellFace ZAxis Neg) . head

-- =============================================================== --
-- Modeling cube manipulation via layer rotations

---------------------------------------------------------------------
-- Layer rotations

-- Exported

rotateLayer :: Axis -> Rotation -> Int -> Cube -> Cube
-- ^Rotate the n-th layer along the given axis 90-degrees in the
-- specified direction. Positive rotations are right handed along the
-- positive direction of the axis. This works by first rotating the
-- whole cube so that the axis of rotation is placed along the z-axis,
-- rotating and then going back.
rotateLayer XAxis t n = rotateCube YAxis Pos90
                        . rotateZLayer n t
                        . rotateCube YAxis Neg90
rotateLayer YAxis t n = rotateCube XAxis Neg90
                        . rotateZLayer n t
                        . rotateCube XAxis Pos90
rotateLayer ZAxis t n = rotateZLayer n t

-- Unexported

rotateZLayer :: Int -> Rotation -> Cube -> Cube
-- ^Rotate the specified layer along the z-axis 90-degrees in the
-- specified direction.
rotateZLayer n t c = [ if k == n then go t x else x | (x,k) <- zip c [0..] ]
    where go Pos90 = (map . map) (rotateCell ZAxis Pos90)
                     . map reverse . transpose
          go Neg90 = (map . map) (rotateCell ZAxis Neg90)
                     . transpose . map reverse

--------------------------------------------------------------------
-- 90-degree single-cell rotation helper function

-- Unexported

rotateCell :: Axis -> Rotation -> Cell -> Cell
rotateCell XAxis Pos90 (Cell xp xn yp yn zp zn) = Cell xp xn zn zp yp yn
rotateCell XAxis Neg90 (Cell xp xn yp yn zp zn) = Cell xp xn zp zn yn yp
rotateCell YAxis Pos90 (Cell xp xn yp yn zp zn) = Cell zp zn yp yn xn xp
rotateCell YAxis Neg90 (Cell xp xn yp yn zp zn) = Cell zn zp yp yn xp xn
rotateCell ZAxis Pos90 (Cell xp xn yp yn zp zn) = Cell yn yp xp xn zp zn
rotateCell ZAxis Neg90 (Cell xp xn yp yn zp zn) = Cell yp yn xn xp zp zn

---------------------------------------------------------------------
-- 90-degree Cube rotation helper function
-- These are helper functions used for rotating layers and not for
-- viewing total rotations of the Rubiks cube, which are handled
-- separately by the View using a rotation matrix.

-- Unexported

rotateCube :: Axis -> Rotation -> Cube -> Cube
rotateCube XAxis Pos90 = (map . map . map) (rotateCell XAxis Pos90)
                      . map reverse . transpose
rotateCube XAxis Neg90 = (map . map . map) (rotateCell XAxis Neg90)
                      . transpose . map reverse
rotateCube YAxis Pos90 = (map . map . map) (rotateCell YAxis Pos90)
                      . map transpose . transpose
                      . map reverse . map transpose
rotateCube YAxis Neg90 = (map . map . map) (rotateCell YAxis Neg90)
                      . map transpose . map reverse
                      . transpose . map transpose
rotateCube ZAxis Pos90 = (map . map . map) (rotateCell ZAxis Pos90)
                      . map ( map reverse . transpose )
rotateCube ZAxis Neg90 = (map . map . map) (rotateCell ZAxis Neg90)
                      . map ( transpose . map reverse )

-- =============================================================== --
-- Modeling total rotations of the cube for visualization

---------------------------------------------------------------------
-- Vector and matrix operations

-- Exported

prodMM :: Matrix -> Matrix -> Matrix
-- ^Matrix-Matrix product.
prodMM (r1,r2,r3) m = let (r1',r2',r3') = tr m
                      in  ( ( dot r1 r1', dot r1 r2', dot r1 r3' )
                          , ( dot r2 r2', dot r2 r2', dot r2 r3' )
                          , ( dot r3 r3', dot r3 r2', dot r3 r3' )
                          )

prodMV :: Matrix -> Vec3 -> Vec3
-- ^Matrix-vector product.
prodMV (r1,r2,r3) v = ( dot r1 v, dot r2 v, dot r3 v )

-- Unexported

tr :: Matrix -> Matrix
-- ^Matrix transpose
tr ( (x1, y1, z1), (x2, y2, z2), (x3, y3, z3) ) = ( (x1, x2, x3)
                                                  , (y1, y2, y3)
                                                  , (z1, z2, z3) )

dot :: Vec3 -> Vec3 -> Float
-- ^Vector-Vector dot product.
dot (x1,y1,z1) (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2

---------------------------------------------------------------------
-- Translating and rotating paths

-- Exported

translatePath :: Vec3 -> Path3D -> Path3D
-- ^Translate all vectors in a path by the given vector.
translatePath (dx,dy,dz) = map go
    where go (x,y,z) = (x + dx, y + dy, z + dz)

rotatePath :: Matrix -> Path3D -> Path3D
-- ^Rotate all vectors in a path using the given rotation matrix.
rotatePath m = map ( prodMV m )

---------------------------------------------------------------------
-- Rotation matrices

rotXMat :: Float -> Matrix
-- ^Rotatation about the positive x axis by t ratians.
rotXMat t = ( ( 1,     0,      0 )
            , ( 0, cos t, -sin t )
            , ( 0, sin t,  cos t )
            )

rotYMat :: Float -> Matrix
-- ^Rotation about the positive y axis by t radians.
rotYMat t = ( (  cos t, 0, sin t )
            , (      0, 1,     0 )
            , ( -sin t, 0, cos t )
            )

rotZMat :: Float -> Matrix
-- ^Rotation about the positive z axis by t radians.
rotZMat t = ( ( cos t, -sin t, 0 )
            , ( sin t,  cos t, 0 )
            , (     0,      0, 1 )
            )
