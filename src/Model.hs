module Model
    ( Color    (..)
    , Cell     (..)
    , Layer    (..)
    , Face     (..)
    , Cube     (..)
    , Game     (..)
    , Rotation (..)
    , Axis     (..)
    -- Rotating each layer of the cube
    , rotateLayer
    -- Getting the faces of the cube
    , faceXpos
    , faceXneg
    , faceYpos
    , faceYneg
    , faceZpos
    , faceZneg
    -- A solved Rubiks cube
    , solved
    ) where

import Data.List ( transpose )

-- =============================================================== --
-- Types

data Color = Red
           | White
           | Yellow
           | Green
           | Blue
           | Orange
           | Hidden
           deriving ( Eq )

instance Show Color where
    show Red    = "R"
    show White  = "W"
    show Yellow = "Y"
    show Green  = "G"
    show Blue   = "B"
    show Orange = "O"
    show Hidden = "H"

data Axis = XAxis | YAxis | ZAxis deriving ( Eq, Show )

data Rotation = Pos90 | Neg90 deriving ( Eq, Show )

-- |Cells make up a cube and each cell has six faces (see below).
-- The faces are layed out as:
-- x-positive x-negative y-positive y-negative z-positive z-negative
data Cell = Cell Color Color Color Color Color Color deriving ( Eq )

instance Show Cell where
    show (Cell xp xn yp yn zp zn) = show xp

-- |A Layer is all the cells in the (x,y)-plane for some depth of z.
-- To get layers along the x or y axes, you first rotate the cube to
-- place the positive x or y axis along the positive z-axis.
type Layer = [[Cell]]

-- |A cube Face cube is the matrix of corresponding cell face colors.
type Face = [[Color]]

-- |A cube is a stack of layers along the z-axis. The layer 0 is
-- at the negative pole of the z-axis (see model discussion below).
type Cube = [Layer]

data Game = Game { cube     :: Cube
                 , selected :: Maybe (Int, Int)
                 } deriving ( Show )

-- =============================================================== --
-- Modeling the cube and its rotations

-- The screen is modeled with a right-handed coordinate fram where
-- the positive-y axis points down, the positive x-axis points right
-- and the positive-z axis points into the screen:
--
--    -y  positive-z into screen
--     |
-- -x --------------------------------> +x
--     |
--     |  Layer-0 on top in the (x,y)-plane at negative pole of z
--     |  Layer-n at bottom in the (x,y)-plane at positive pole of z
--     |  [ [ (0,0) (0,1) (0,2) ]
--     |    [ (1,0) (1,1) (1,2) ]    <-- positive-x face
--     |    [ (2,0) (2,1) (2,2) ] ]
--     |
--     v      ^                       looking down on negative z-face
--    +y      | positive-y face
--
---------------------------------------------------------------------
-- Total Rotations
--
-- Total rotations of the cube (i.e., rotation of all stacked layers
-- simultaneously) are handled separately as the these do not change
-- the organization of the cube and only its visualization. The cube
-- is thus modeled as orientationally fixed and only individual layer
-- rotations change the cube.
--
---------------------------------------------------------------------
-- Layer rotations
--
-- Right-handed rotations are positive (i.e., thumb in the positive
-- direction of the axis of rotation) and left-handed rotations are
-- negative. Rotations of individual layers are modeled by rotating
-- the cube so that the positive axis points into the screen
-- replacing the positive z-axis, rotating the layer and then
-- rotating the cube back to its original configuration. Each cell is
-- also rotated accordingly.

--------------------------------------------------------------------
-- 90-degree cell rotations

rotCell :: Axis -> Rotation -> Cell -> Cell
rotCell XAxis Pos90 (Cell xp xn yp yn zp zn) = Cell xp xn zn zp yp yn
rotCell XAxis Neg90 (Cell xp xn yp yn zp zn) = Cell xp xn zp zn yn yp
rotCell YAxis Pos90 (Cell xp xn yp yn zp zn) = Cell zp zn yp yn xn xp
rotCell YAxis Neg90 (Cell xp xn yp yn zp zn) = Cell zn zp yp yn xp xn
rotCell ZAxis Pos90 (Cell xp xn yp yn zp zn) = Cell yn yp xp xn zp zn
rotCell ZAxis Neg90 (Cell xp xn yp yn zp zn) = Cell yp yn xn xp zp zn

---------------------------------------------------------------------
-- 90-degree Cube rotations

rotCube :: Axis -> Rotation -> Cube -> Cube
rotCube XAxis Pos90 = (map . map . map) (rotCell XAxis Pos90)
                      . map reverse . transpose
rotCube XAxis Neg90 = (map . map . map) (rotCell XAxis Neg90)
                      . transpose . map reverse
rotCube YAxis Pos90 = (map . map . map) (rotCell YAxis Pos90)
                      . map transpose . transpose
                      . map reverse . map transpose
rotCube YAxis Neg90 = (map . map . map) (rotCell YAxis Neg90)
                      . map transpose . map reverse
                      . transpose . map transpose
rotCube ZAxis Pos90 = (map . map . map) (rotCell ZAxis Pos90)
                      . map ( map reverse . transpose )
rotCube ZAxis Neg90 = (map . map . map) (rotCell ZAxis Neg90)
                      . map ( transpose . map reverse )

---------------------------------------------------------------------
-- Layer rotations

rotLayer :: Int -> Rotation -> Cube -> Cube
rotLayer n t c = [ if k == n then go t x else x | (x,k) <- zip c [0..] ]
    where go Pos90 = (map . map) (rotCell ZAxis Pos90) . map reverse . transpose
          go Neg90 = (map . map) (rotCell ZAxis Neg90) . transpose . map reverse

rotateLayer :: Axis -> Rotation -> Int -> Cube -> Cube
rotateLayer XAxis t n = rotCube YAxis Pos90 . rotLayer n t . rotCube YAxis Neg90
rotateLayer YAxis t n = rotCube XAxis Neg90 . rotLayer n t . rotCube XAxis Pos90
rotateLayer ZAxis t n = rotLayer n t

-- =============================================================== --
-- Getting the faces of the cube

cellXp, cellXn, cellYp, cellYn, cellZp, cellZn :: Cell -> Color
cellXp (Cell x _ _ _ _ _) = x
cellXn (Cell _ x _ _ _ _) = x
cellYp (Cell _ _ y _ _ _) = y
cellYn (Cell _ _ _ y _ _) = y
cellZp (Cell _ _ _ _ z _) = z
cellZn (Cell _ _ _ _ _ z) = z

faceXpos, faceXneg, faceYpos, faceYneg, faceZpos, faceZneg :: Cube -> Face
-- ^The cube and the cells have six faces each (see diagram above).
-- Faces always have the positive axes pointing right and down.
--
-- x-positive: pos pole of x-axis, pos z right, pos y down
-- x-negative: neg pole of x-axis, pos z left,  pos y down
-- y-positive: pos pole of y-axis, pos x right, pos z down
-- y-negative: neg pole of y-axis, pos x right, pos z up
-- z-positive: pos pole of z-axis, pos x right, pos y up
-- z-negative: neg pole of z-axis, pos x right, pos y down
faceXpos = (map . map) cellZn . head . rotCube YAxis Pos90
faceXneg = (map . map) cellZn . head . rotCube YAxis Neg90
faceYpos = (map . map) cellZn . head . rotCube XAxis Neg90
faceYneg = (map . map) cellZn . head . rotCube XAxis Pos90
faceZpos = (map . map) cellZp . head . ( \ f -> f . f ) (rotCube XAxis Pos90)
faceZneg = (map . map) cellZn . head

-- =============================================================== --
-- Testing

solved :: Cube
solved = [ [ [ Cell Hidden Blue   Hidden Orange Hidden Red
             , Cell Hidden Hidden Hidden Orange Hidden Red
             , Cell White  Hidden Hidden Orange Hidden Red
             ]
           , [ Cell Hidden Blue   Hidden Hidden Hidden Red
             , Cell Hidden Hidden Hidden Hidden Hidden Red
             , Cell White  Hidden Hidden Hidden Hidden Red
             ]
           , [ Cell Hidden Blue   Green Hidden Hidden Red
             , Cell Hidden Hidden Green Hidden Hidden Red
             , Cell White  Hidden Green Hidden Hidden Red
             ]
           ]
        ,  [ [ Cell Hidden Blue   Hidden Orange Hidden Hidden
             , Cell Hidden Hidden Hidden Orange Hidden Hidden
             , Cell White  Hidden Hidden Orange Hidden Hidden
             ]
           , [ Cell Hidden Blue   Hidden Hidden Hidden Hidden
             , Cell Hidden Hidden Hidden Hidden Hidden Hidden
             , Cell White  Hidden Hidden Hidden Hidden Hidden
             ]
           , [ Cell Hidden Blue   Green Hidden Hidden Hidden
             , Cell Hidden Hidden Green Hidden Hidden Hidden
             , Cell White  Hidden Green Hidden Hidden Hidden
             ]
           ]
        ,  [ [ Cell Hidden Blue   Hidden Orange Yellow Hidden
             , Cell Hidden Hidden Hidden Orange Yellow Hidden
             , Cell White  Hidden Hidden Orange Yellow Hidden
             ]
           , [ Cell Hidden Blue   Hidden Hidden Yellow Hidden
             , Cell Hidden Hidden Hidden Hidden Yellow Hidden
             , Cell White  Hidden Hidden Hidden Yellow Hidden
             ]
           , [ Cell Hidden Blue   Green Hidden Yellow Hidden
             , Cell Hidden Hidden Green Hidden Yellow Hidden
             , Cell White  Hidden Green Hidden Yellow Hidden
             ]
           ]
        ]
