module Model.Cube
    ( -- Getting the faces of the cube
      cellFace
    , cubeFace
      -- Rotating each layer of the cube
    , rotateLayer
    ) where

-- =============================================================== --
-- Manages the internal model representation of the Rubiks cube
-- =============================================================== --

import Data.List    ( transpose     )
import Model.Types  ( Axis     (..)
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
cubeFace ZAxis Pos = (map . map) (cellFace ZAxis Neg)
                     . head . ( \ f -> f . f ) (rotateCube XAxis Pos90)
-- z-negative: neg pole of z-axis, pos x right, pos y down
cubeFace ZAxis Neg = (map . map) (cellFace ZAxis Neg) . head

-- =============================================================== --
-- Modeling cube manipulation via layer rotations
--
-- These functions are not used for visualization of the cube.
-- Instead, they are used to model the configuration of the cube in
-- a standard, unrotated orientation.

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
