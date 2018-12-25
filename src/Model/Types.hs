{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Model.Types
 ( -- Modeling game state
   Game         (..)
 , Mode         (..)
   -- Modeling the Rubiks cube and layer rotations
 , Cell         (..)
 , Color        (..)
 , Cube         (..)
 , Face         (..)
 , Layer        (..)
   -- Modeling axes and layer rotations
 , Axis         (..)
 , Locus        (..)
 , Pole         (..)
 , Rotation     (..)
 , Move         (..)
   -- Modeling a viewable Rubiks cube
 , Matrix       (..)
 , Path3D       (..)
 , Square       (..)
 , Transform    (..)
 , Triple       (..)
 , Vec3         (..)
 ) where

-- =============================================================== --
-- Types for modeling the game state

data Game = Game {
      cube     :: Cube   -- Rubiks cube model state
    , rotation :: Matrix -- User-rotation of the cube
    , mode     :: Mode   -- What the user is doing
    , toScreen :: Float  -- Distance from user to the screen
    , scaling  :: Float  -- Scaling factor for cube size
    , moves    :: [Move] -- All player moves made in the game
    }

-- |Current game state.
data Mode = -- Cube is being rotated with last mouse position.
            RotationMove (Float, Float)
            -- Cube is being scaled for initial scale factor and mouse down.
          | ScalingMove Float (Float, Float)
            -- Layer is being manipulated from a selected cell locus.
          | Selected Locus
            -- Nothing happening
          | Idle
           deriving ( Show )

-- =============================================================== --
-- Types for modeling the cube and rotations of its layers
--
---------------------------------------------------------------------
-- Basic model
-- Left-handed coordinate system. Right-handed rotations about an
-- axis are called positive-rotations.
-- Each face of the cube is at the negative pole of the given axis.
-- Indexing of cells on the face follows:
-- (0,0) .. (0,n)
--   .        .
-- (n,0) .. (n,n)
-- overlaying the following standard face-orientations:
--
-- +y        +z        +x        +x        +y        +z
-- ^  -z     ^  -x     ^  -y     ^  +z     ^  +x     ^  +y
-- |         |         |         |         |         |
-- ---> +x , ---> +y , ---> +z , ---> +y , ---> +z , ---> +x
--
-- The standard cube orientation is the first, facing the negative-z.
-- pole. Other faces are accessed by rotating the cube and taking the
--- top matrix element; however, the cube is assumed to always be in
-- the first orientation. Visualization is handled separately.
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
data Cell = Cell Color Color Color Color Color Color deriving ( Eq, Show )

-- |A cube Face is a matrix of corresponding cell face colors
-- oriented according to the model above.
type Face = [[Color]]

-- |A Layer is all the cells in the horizontal plane at some depth
-- along the positive orthogonal axis. A layer is accessed by
-- rotating the cube to the standard orientation for the given
-- negative face and then taking the layer at the specified depth.
type Layer = [[Cell]]

-- |A cube is a stack of layers along the z-axis, with layer 0 at
-- the negative pole.
type Cube = [Layer]

---------------------------------------------------------------------
-- Layer rotations and face accessors
--
-- Right-handed rotations are positive (i.e., thumb in the positive
-- direction of the axis of rotation) and left-handed rotations are
-- negative. Layers are rotatated based on an axis, 90-degree
-- rotation and depth. The cube is first rotated to the standard
-- orientation for the negative pole of the axis of rotation (see
-- above), the layer is accessed by the depth, with the 0-th most
-- negative, the rotation is applied and then the cube is returned to
-- its standard orientation.

-- |A player's move with everything you need to rotate a layer
type Move = ( Axis     -- Which axis
            , Rotation -- Which way
            , Int      -- Layer depth
            )

-- |Coordinate axis (left-handed, since Gloss has y-pointing up)
data Axis = XAxis -- Positive axis points to the right
          | YAxis -- Positive axis points up
          | ZAxis -- Positive axis points into the screen
            deriving ( Eq, Show )

-- |Ninety-degree rotations, which are used for rotating layers.
data Rotation = Pos90 | Neg90 deriving ( Eq, Show )

-- |Poles of an axis.
data Pole = Pos | Neg deriving ( Eq, Show )

-- |Position of a cell on the cube. Coordinates are based on the
-- standard orientations described above.
data Locus = Locus { axis  :: Axis          -- Axis orthogonal to cube face
                   , pole  :: Pole          -- Which side of the cube
                   , coord :: (Int, Int)    -- Where in the layer
                   } deriving ( Eq, Show )

-- =============================================================== --
-- Types for modeling the view of the Rubiks cube

---------------------------------------------------------------------
-- Total Rotations
--
-- Total rotations of the cube (i.e., rotation of all stacked layers
-- simultaneously) are handled separately as the these do not change
-- the configuration of the cube and only its visualization.

-- |Cell face with rendering information. When rendering, cell faces
-- are first converted to Squares.
data Square = Square { locus  :: Locus  -- How to find the corresponding cell
                     , front  :: Color  -- Color when facing front
                     , back   :: Color  -- Color when facing back
                     , points :: Path3D -- Points for rendering
                     }

-- |Polymorphic triple used to build 3D-vectors and 3x3-matrices.
type Triple a = (a, a, a)

-- |Point in 2-space.
type Vec2 = (Float, Float)

-- |Point in 3-space.
type Vec3 = Triple Float

instance Num Vec3 where
    (+) (x,y,z) (x',y',z') = (x + x', y + y', z + z')
    (-) (x,y,z) (x',y',z') = (x - x', y - y', z - z')
    fromInteger x          = (fromInteger x, fromInteger x, fromInteger x)
    (*) (x,y,z) (x',y',z') = (x * x', y * y', z * z')
    abs (x,y,z)            = (abs x, abs y, abs z)
    signum (x,y,z)         = (signum x, signum y, signum z)

-- |Path in 3-space.
type Path3D = [Vec3]

-- |Transformations of paths
type Transform = Path3D -> Path3D

-- |3x3-Matrix for rotations.
type Matrix = Triple ( Triple Float )
