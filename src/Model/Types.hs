{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Model.Types
 ( -- Modeling game state
   Game      (..)
   -- Modeling the Rubiks cube and layer rotations
 , Cell      (..)
 , Color     (..)
 , Cube      (..)
 , Face      (..)
 , Layer     (..)
   -- Modeling axes and layer rotations
 , Axis      (..)
 , Locus     (..)
 , Pole      (..)
 , Rotation  (..)
   -- Modeling a viewable Rubiks cube
 , Matrix    (..)
 , Path3D    (..)
 , Square    (..)
 , Transform (..)
 , Triple    (..)
 , Vec3      (..)
 ) where

-- =============================================================== --
-- Types for modeling the game state

data Game = Game { cube     :: Cube
                 , rotation :: Matrix
                 , rotMove  :: Maybe (Float, Float)
                 }

-- =============================================================== --
-- Types for modeling the cube and rotations of its layers
--
---------------------------------------------------------------------
-- Basic model
--
-- The screen is modeled with a right-handed coordinate frame where
-- the positive-y axis points down, the positive x-axis points right
-- and the positive-z axis points into the screen. The Rubiks cube
-- is modeled as a stack of matrices of cells called a layer along
-- the z-axis. Each cell in a layer is defined by six faces, which in
-- turn have color values. Layers in other planes (e.g., (z,y)) are
-- accessed by first rotating the cube so that the (z,y)-plane now
-- occupies the previous (x,y)-plane (see below). Total rotations of
-- the Rubiks cube are handled using a separate rotation matrix.
--
--    -y        positive-z into screen
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
-- Cells

-- |Colors of each cell face.
data Color = Red
           | White
           | Yellow
           | Green
           | Blue
           | Orange
           | Hidden
           deriving ( Show, Eq )

-- |Cells are defined by their six faces layed out as:
-- x-positive x-negative y-positive y-negative z-positive z-negative
data Cell = Cell Color Color Color Color Color Color deriving ( Eq )

-- |A cube Face is the matrix of corresponding cell face colors.
type Face = [[Color]]

-- |A Layer is all the cells in the (x,y)-plane for some depth of z.
-- To get layers along the x or y axes, you first rotate the cube to
-- place the positive x or y axis along the positive z-axis.
type Layer = [[Cell]]

-- |A cube is a stack of layers along the z-axis. The layer 0 is
-- at the negative pole of the z-axis.
type Cube = [Layer]

---------------------------------------------------------------------
-- Layer rotations and face accessors
--
-- Right-handed rotations are positive (i.e., thumb in the positive
-- direction of the axis of rotation) and left-handed rotations are
-- negative. Rotations of individual layers are modeled by rotating
-- the cube so that the positive axis points into the screen
-- replacing the positive z-axis, rotating the layer and then
-- rotating the cube back to its original configuration. Each cell is
-- also rotated accordingly. The responsible functions are defined in
-- the Model module.

-- |Coordinate axis
data Axis = XAxis -- Positive axis points to the right
          | YAxis -- Positive axis points down
          | ZAxis -- Positive axis points into the screen
            deriving ( Eq, Show )

-- |Ninety-degree rotations, which are used for rotating layers.
data Rotation = Pos90 | Neg90 deriving ( Eq, Show )

-- |Poles of an axis.
data Pole = Pos | Neg deriving ( Eq, Show )

-- |Position of a cell on the cube.
data Locus = Locus { axis  :: Axis          -- Axis orthogonal to cube face
                   , pole  :: Pole          -- Pole which side of the cube
                   , coord :: (Int, Int)    -- Where in the layer
                   } deriving ( Eq, Show )

-- =============================================================== --
-- Types for modeling the view of the Rubiks cube

---------------------------------------------------------------------
-- Total Rotations
--
-- Total rotations of the cube (i.e., rotation of all stacked layers
-- simultaneously) are handled separately as the these do not change
-- the organization of the cube and only its visualization.

-- |Cell face with rendering information. When rendering, cell faces
-- are first converted to Squares.
data Square = Square { locus  :: Locus  -- How to find the corresponding cell
                     , front  :: Color  -- Color when facing front
                     , back   :: Color  -- Color when facing back
                     , points :: Path3D -- Points for rendering
                     }

-- |Polymorphic triple used to build 3D-vectors and 3x3-matrices.
type Triple a = (a, a, a)

-- |Point in 3-space.
type Vec3 = Triple Float

instance Num Vec3 where
    (+) (x,y,z) (x',y',z') = (x + x', y + y', z + z')
    (-) (x,y,z) (x',y',z') = (x - x', y - y', z - z')
    fromInteger x          = (fromInteger x, fromInteger x, fromInteger x)
    (*) (x,y,z) (x',y',z') = (x * x', y * y', z * z')
    abs (x,y,z)            = (abs x, abs y, abs z)
    signum (x,y,z)         = ( signum x, signum y, signum z )

-- |Path in 3-space.
type Path3D = [Vec3]

-- |Transformations of paths
type Transform = Path3D -> Path3D

-- |3x3-Matrix for rotations.
type Matrix = Triple ( Triple Float )
