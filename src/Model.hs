module Model
    ( Color (..)
    , Cell  (..)
    , Layer (..)
    , Face  (..)
    , Cube  (..)
    , Game  (..)
    -- Rotating each layer of the cube
    , rotLayerXp
    , rotLayerXn
    , rotLayerYp
    , rotLayerYn
    , rotLayerZp
    , rotLayerZn
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

data Game = Game { cube :: Cube }

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
--
---------------------------------------------------------------------
-- The cube and the cells have six faces each (see diagram above):
--
-- x-positive: face at the positive pole of the x-axis
-- x-negative: face at the negative pole of the x-axis
-- y-positive: face at the positive pole of the y-axis
-- y-negative: face at the negative pole of the y-axis
-- z-positive: face at the positive pole of the z-axis
-- z-negative: face at the negative pole of the z-axis

--------------------------------------------------------------------
-- 90-degree cell rotations

rotCellXp, rotCellXn :: Cell -> Cell
rotCellXp (Cell xp xn yp yn zp zn) = Cell xp xn zn zp yp yn
rotCellXn (Cell xp xn yp yn zp zn) = Cell xp xn zp zn yn yp

rotCellYp, rotCellYn :: Cell -> Cell
rotCellYp (Cell xp xn yp yn zp zn) = Cell zp zn yp yn xn xp
rotCellYn (Cell xp xn yp yn zp zn) = Cell zn zp yp yn xp xn

rotCellZp, rotCellZn :: Cell -> Cell
rotCellZp (Cell xp xn yp yn zp zn) = Cell yn yp xp xn zp zn
rotCellZn (Cell xp xn yp yn zp zn) = Cell yp yn xn xp zp zn

---------------------------------------------------------------------
-- 90-degree Cube rotations

rotCubeXp, rotCubeXn :: Cube -> Cube
rotCubeXp = (map . map . map) rotCellXp
            . map reverse . transpose
rotCubeXn = (map . map . map) rotCellXn
            . transpose . map reverse

rotCubeYp, rotCubeYn :: Cube -> Cube
rotCubeYp = (map . map . map) rotCellYp
            . map transpose . transpose
            . map reverse . map transpose
rotCubeYn = (map . map . map) rotCellYn
            . map transpose . map reverse
            . transpose . map transpose

rotCubeZp, rotCubeZn :: Cube -> Cube
rotCubeZp = (map . map . map) rotCellZp
            . map ( map reverse . transpose )
rotCubeZn = (map . map . map) rotCellZn
            . map ( transpose . map reverse )

---------------------------------------------------------------------
-- Layer rotations

rotLayerPos, rotLayerNeg :: Layer -> Layer
rotLayerPos = (map . map) rotCellZp . map reverse . transpose
rotLayerNeg = (map . map) rotCellZn . transpose . map reverse

rotLayer :: Int -> (Layer -> Layer) -> Cube -> Cube
rotLayer n go c = [ if k == n then go x else x | (x,k) <- zip c [0..] ]

rotLayerZp, rotLayerZn :: Int -> Cube -> Cube
rotLayerZp n = rotLayer n rotLayerPos
rotLayerZn n = rotLayer n rotLayerNeg

rotLayerXp, rotLayerXn :: Int -> Cube -> Cube
rotLayerXp n = rotCubeYp . rotLayer n rotLayerPos . rotCubeYn
rotLayerXn n = rotCubeYp . rotLayer n rotLayerNeg . rotCubeYn

rotLayerYp, rotLayerYn :: Int -> Cube -> Cube
rotLayerYp n = rotCubeXn . rotLayer n rotLayerPos . rotCubeXp
rotLayerYn n = rotCubeXn . rotLayer n rotLayerNeg . rotCubeXp

-- =============================================================== --
-- Getting the faces of the cube

cellXp, cellXn, cellYp, cellYn, cellZp, cellZn :: Cell -> Color
cellXp (Cell x _ _ _ _ _) = x
cellXn (Cell _ x _ _ _ _) = x
cellYp (Cell _ _ y _ _ _) = y
cellYn (Cell _ _ _ y _ _) = y
cellZp (Cell _ _ _ _ z _) = z
cellZn (Cell _ _ _ _ _ z) = z

faceXpos, faceXneg :: Cube -> Face
faceXpos = (map . map) cellZn . head . rotCubeYp
faceXneg = (map . map) cellZn . head . rotCubeYn
faceYpos = (map . map) cellZn . head . rotCubeXn
faceYneg = (map . map) cellZn . head . rotCubeXp
faceZpos = (map . map) cellZp . last
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
