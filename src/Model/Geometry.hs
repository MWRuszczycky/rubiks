module Model.Geometry
    ( -- Vector and matrix operations
      dot
    , cross
    , prodMM
    , prodMV
    , inConvex2D
      -- Translating and rotating 3D-paths
    , rotatePath
    , scalePath
    , translatePath
      -- Rotation matrices
    , rotXMat
    , rotYMat
    , rotZMat
    ) where

-- =============================================================== --
-- General 3D-matrix and vector operations for a left-handed frame
-- =============================================================== --

import Data.List    ( foldl'         )
import Model.Types  ( Axis      (..)
                    , Cell      (..)
                    , Color     (..)
                    , Cube      (..)
                    , Face      (..)
                    , Layer     (..)
                    , Matrix    (..)
                    , Pole      (..)
                    , Rotation  (..)
                    , Transform (..)
                    , Vec3      (..)
                    , Path3D    (..) )

---------------------------------------------------------------------
-- Vector and matrix operations

-- Exported

dot :: Vec3 -> Vec3 -> Float
-- ^Vector-Vector dot product.
dot (x1,y1,z1) (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Vec3 -> Vec3 -> Vec3
-- ^Compute the cross product of two vectors.
cross (x1,y1,z1) (x2,y2,z2) = ( y1 * z2 - z1 * y2
                              , z1 * x2 - x1 * z2
                              , x1 * y2 - y1 * x2
                              )

prodMM :: Matrix -> Matrix -> Matrix
-- ^Matrix-Matrix product.
prodMM (r1,r2,r3) m = let (r1',r2',r3') = tr m
                      in  ( ( dot r1 r1', dot r1 r2', dot r1 r3' )
                          , ( dot r2 r1', dot r2 r2', dot r2 r3' )
                          , ( dot r3 r1', dot r3 r2', dot r3 r3' )
                          )

prodMV :: Matrix -> Vec3 -> Vec3
-- ^Matrix-vector product.
prodMV (r1,r2,r3) v = ( dot r1 v, dot r2 v, dot r3 v )

inConvex2D :: (Float, Float) -> Path3D -> Bool
-- ^Flatten a 3D path by removing the z-coordinate and assuming the
-- result is convex, determine whether a 2D point lies inside of it.
inConvex2D (x,y) ps = length ps > 2 && ( all (>0) zs || all (<0) zs )
    where ws = [ (a,b,0) | (a,b,_) <- ps ]
          us = map ((-) (x,y,0)) ws
          vs = zipWith (-) (tail ws ++ [head ws]) ws
          zs = zipWith ( \ u v -> ( \ (_,_,z) -> z ) . cross u $ v ) us vs

-- Unexported

tr :: Matrix -> Matrix
-- ^Matrix transpose
tr ( (x1, y1, z1)
   , (x2, y2, z2)
   , (x3, y3, z3) ) = ( (x1, x2, x3)
                      , (y1, y2, y3)
                      , (z1, z2, z3) )

---------------------------------------------------------------------
-- Transforming 3D-paths

-- Exported

rotatePath :: [Matrix] -> Transform
-- ^Rotate all vectors in a path using the given rotation matrix.
rotatePath []     = id
rotatePath (m:ms) = map ( prodMV t )
    where t = foldl' prodMM m ms
-- rotatePath m = map ( prodMV m )

translatePath :: Vec3 -> Transform
-- ^Translate all vectors in a path by the given vector.
translatePath (dx,dy,dz) = map go
    where go (x,y,z) = (x + dx, y + dy, z + dz)

scalePath :: Float -> Transform
scalePath x = map ( prodMV m )
    where m = ( ( x, 0, 0 )
              , ( 0, x, 0 )
              , ( 0, 0, x ) )

---------------------------------------------------------------------
-- Rotation matrices for a left-handed coordinate frame

rotXMat :: Float -> Matrix
-- ^Rotatation about the positive x axis by t ratians.
rotXMat t = ( ( 1,      0,     0 )
            , ( 0,  cos t, sin t )
            , ( 0, -sin t, cos t )
            )

rotYMat :: Float -> Matrix
-- ^Rotation about the positive y axis by t radians.
rotYMat t = ( (  cos t, 0, -sin t )
            , (      0, 1,      0 )
            , (  sin t, 0,  cos t )
            )

rotZMat :: Float -> Matrix
-- ^Rotation about the positive z axis by t radians.
rotZMat t = ( (  cos t, sin t, 0 )
            , ( -sin t, cos t, 0 )
            , (      0,     0, 1 )
            )
