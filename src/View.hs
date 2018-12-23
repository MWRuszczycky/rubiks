module View
    ( renderGame
    ) where

import qualified Graphics.Gloss as G
import qualified Types          as T
import qualified Model          as M
import Data.List                     ( sortOn
                                     , foldl' )

-- =============================================================== --
-- Rendering functions

renderGame :: T.Game -> G.Picture
-- ^Main rendering hub.
renderGame g = renderCube (T.rotation g) . T.cube $ g

---------------------------------------------------------------------
-- Rendering the Rubiks cube

renderCube :: T.Matrix -> T.Cube -> G.Picture
-- ^Given a rotation matrix, convert a Rubiks cube model into a Gloss
--- picture that can be viewed on the screen. The Rubiks cube is
-- pushed back sufficiently far so as not to collide with the view
-- screen after applying the rotation.
renderCube m c =
    let move   = moveSquare (M.translatePath (0,0,100) . M.rotatePath m)
        render = renderSquare 250 . move
    in  G.pictures . snd . unzip  -- Cleanup after sorting and render
        . reverse . sortOn fst    -- Sort on the depth cues for z-placement
        . map render              -- Rotate all faces together and push back
        . concat $ [ faceToSquares T.XAxis T.Pos c
                   , faceToSquares T.XAxis T.Neg c
                   , faceToSquares T.YAxis T.Pos c
                   , faceToSquares T.YAxis T.Neg c
                   , faceToSquares T.ZAxis T.Pos c
                   , faceToSquares T.ZAxis T.Neg c
                   ]

---------------------------------------------------------------------
-- Rendering each face of the cube

faceToSquares :: T.Axis -> T.Pole -> T.Cube -> [T.Square]
-- ^Extract each face from the cube and position accordingly in the
-- initial unrotated state.
faceToSquares a p = map (moveSquare (positionFace a p) )
                    . baseFace
                    . M.cubeFace a p

positionFace :: T.Axis -> T.Pole -> T.Path3D -> T.Path3D
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
positionFace T.XAxis T.Pos = M.translatePath (66, 0,  0)
                             . M.rotatePath ( M.rotYMat (-pi/2) )
positionFace T.XAxis T.Neg = M.translatePath (-66, 0, 0)
                             . M.rotatePath ( M.rotYMat (pi/2)  )
positionFace T.YAxis T.Pos = M.translatePath (0, 66,  0)
                             . M.rotatePath ( M.rotXMat (pi/2)  )
positionFace T.YAxis T.Neg = M.translatePath (0,-66,  0)
                             . M.rotatePath ( M.rotXMat (-pi/2) )
positionFace T.ZAxis T.Pos = M.translatePath (0,  0, 66)
                             . M.rotatePath ( M.rotXMat (pi)    )
positionFace T.ZAxis T.Neg = M.translatePath (0,  0,-66)

baseFace :: T.Face -> [T.Square]
-- ^Convert a cube face into a list of squares with renedring
-- information. The face is created in the standard position and thus
-- faces the negative pole of the z-axis and lies in the (x,y)-plane.
-- The squares in each face are separated by 4 pixel spacers. Since
-- each square is 40 x 40 pixels, the face is 128 x 128 pixels.
baseFace cs = map posSq sqs
    where go x          = fromIntegral $ 44 * ( x - 1 )
          posSq (s,i,j) = moveSquare ( M.translatePath (go j, go i, 0) ) s
          sqs           = [ (baseSquare (cs !! i !! j) T.Hidden, i, j)
                            | i <- [0..2] , j <- [0..2] ]

---------------------------------------------------------------------
-- Rendering squares
-- Squares are uniform finite planes representing the exposed face
-- each cell in the Rubiks cube. These are used to build up each face
-- of the cube, which are then used to build the cube.

renderSquare :: Float -> T.Square -> (Float, G.Picture)
-- ^Provided a focal distance, convert a renderable square to a Gloss
-- polygon together with a depth cue.
renderSquare d s = ( depth, pic )
    where pic   = colorSquare d s . G.polygon . project d $ T.points s
          depth = minimum [ z | (_,_,z) <- T.points s ]

colorSquare :: Float -> T.Square -> G.Picture -> G.Picture
-- ^Determine the color of the square depending on whether it is
-- facing the viewer or facing away from the viewer. This is
-- determined by finding the normal vector of the square face, which
-- points in the exposed direction, as well as d the vector from the
-- viewer to the center of the square. If the dot-product of these
-- two vectors is negative, then the exposed face is facing the
-- viewer, otherwise the viewer is seeing the back of the square.
-- Remember that positive-y points down and positive-x points right.
colorSquare d (T.Square f b ps)
    | M.dot n e < 0 = G.color (renderColor f)
    | otherwise     = G.color (renderColor b)
    where (t:u:v:w:_) = map (+ (0,0,d)) ps
          n           = M.cross (w - t) (u - t)
          e           = foldl' (+) (0,0,0) [t, u, v, w]

moveSquare :: (T.Path3D -> T.Path3D) -> T.Square -> T.Square
-- ^Helper function for positioning a Square according to some
-- transformation function.
moveSquare go s = s { T.points = go $ T.points s}

baseSquare :: T.Color -> T.Color -> T.Square
-- ^Initial standard form the square given its exposed-facing-front
-- color, and its hidden-back color. Squares are 40 x 40 pixels.
baseSquare f b = T.Square f b [ ( -20, -20, 0 )
                              , (  20, -20, 0 )
                              , (  20,  20, 0 )
                              , ( -20,  20, 0 )
                              ]

---------------------------------------------------------------------
-- Gloss-Model interface functions

project :: Float -> T.Path3D -> G.Path
-- ^Convert a Model path in 3-dimensions to a projected Gloss path.
-- The initial argument is the focal length.
project d = map go
    where go (x,y,z) | z > 0     = ( u * x, u * y)
                     | otherwise = ( x, y )
                     where u = d / ( d + z )

renderColor :: T.Color -> G.Color
-- ^Map Model colors to Gloss colors.
renderColor T.Red    = G.makeColor 0.78 0.15 0.10 1.0
renderColor T.White  = G.makeColor 0.75 0.75 0.75 1.0
renderColor T.Yellow = G.makeColor 1.00 0.85 0.34 1.0
renderColor T.Green  = G.makeColor 0.00 0.54 0.36 1.0
renderColor T.Blue   = G.makeColor 0.00 0.64 1.00 1.0
renderColor T.Orange = G.makeColor 1.00 0.38 0.00 1.0
renderColor T.Hidden = G.makeColor 0.10 0.10 0.10 1.0
