module View
    ( renderGame
    ) where

import qualified Graphics.Gloss as G
import qualified Types          as T
import qualified Model          as M
import Data.List                     ( sortOn
                                     , foldl' )

renderGame :: T.Game -> G.Picture
renderGame g = renderCube (T.rotation g) . T.cube $ g

-- =============================================================== --
-- Rendering functions

renderCube :: T.Matrix -> T.Cube -> G.Picture
-- ^Given a rotation matrix, convert a Rubiks cube model into a Gloss
--- picture that can be viewed on the screen.
renderCube m c = G.pictures . snd . unzip $ xs
    where xs = reverse
               . sortOn fst
               . map (renderSquare 250)
               . map ( moveSquare (M.translatePath (0,0,100) . M.rotatePath m) )
               . concat $ [ faceToSquares T.XAxis T.Pos c
                          , faceToSquares T.XAxis T.Neg c
                          , faceToSquares T.YAxis T.Pos c
                          , faceToSquares T.YAxis T.Neg c
                          , faceToSquares T.ZAxis T.Pos c
                          , faceToSquares T.ZAxis T.Neg c
                          ]

faceToSquares :: T.Axis -> T.Pole -> T.Cube -> [T.Square]
faceToSquares a p = map (moveSquare (positionFace a p) )
                    . baseFace
                    . M.cubeFace a p

positionFace :: T.Axis -> T.Pole -> T.Path3D -> T.Path3D
positionFace T.XAxis T.Pos =
    M.translatePath (66, 0,  0) . M.rotatePath ( M.rotYMat (-pi/2) )
positionFace T.XAxis T.Neg =
    M.translatePath (-66, 0, 0) . M.rotatePath ( M.rotYMat (pi/2)  )
positionFace T.YAxis T.Pos =
    M.translatePath (0, 66,  0) . M.rotatePath ( M.rotXMat (pi/2)  )
positionFace T.YAxis T.Neg =
    M.translatePath (0,-66,  0) . M.rotatePath ( M.rotXMat (-pi/2) )
positionFace T.ZAxis T.Pos =
    M.translatePath (0,  0, 66) . M.rotatePath ( M.rotXMat (pi)    )
positionFace T.ZAxis T.Neg =
    M.translatePath (0,  0,-66)

baseFace :: T.Face -> [T.Square]
baseFace cs = map posSq sqs
    where go x          = fromIntegral $ 44 * ( x - 1 )
          posSq (s,i,j) = moveSquare ( M.translatePath (go j, go i, 0) ) s
          sqs           = [ (baseSquare (cs !! i !! j) T.Hidden, i, j)
                            | i <- [0..2] , j <- [0..2] ]

moveSquare :: (T.Path3D -> T.Path3D) -> T.Square -> T.Square
moveSquare go s = let ps = go $ T.points s
                  in  s { T.points = ps }

renderSquare :: Float -> T.Square -> (Float, G.Picture)
-- ^Provided a focal distance, convert a renderable square to a Gloss
-- polygon together with a depth cue.
renderSquare d s@(T.Square f b ps) = ( depth, pic )
    where pic   = colorSquare d s . G.polygon . project d $ ps
          depth = minimum [ z | (_,_,z) <- ps ]

project :: Float -> T.Path3D -> G.Path
-- ^Convert a Model path in 3-dimensions to a projected Gloss path.
-- The initial argument is the focal length.
project d = map go
    where go (x,y,z) | z > 0     = ( u * x, u * y)
                     | otherwise = ( x, y )
                     where u = d / ( d + z )

colorSquare :: Float -> T.Square -> G.Picture -> G.Picture
-- ^Note that positive y is down and positive x is to the right.
colorSquare d (T.Square f b (t:u:v:w:_))
    | M.dot n e < 0 = G.color (renderColor f)
    | otherwise     = G.color (renderColor b)
    where u' = M.sumVV (0,0,d) u
          w' = M.sumVV (0,0,d) w
          t' = M.sumVV (0,0,d) t
          v' = M.sumVV (0,0,d) v
          n = M.cross (M.diffVV w' t') (M.diffVV u' t')
          e = foldl' M.sumVV (0,0,0) [u', w', t', v']

baseSquare :: T.Color -> T.Color -> T.Square
baseSquare f b = T.Square f b [ ( -20, -20, 0 )
                              , (  20, -20, 0 )
                              , (  20,  20, 0 )
                              , ( -20,  20, 0 )
                              ]

renderColor :: T.Color -> G.Color
-- ^Map Model colors to Gloss colors.
renderColor T.Red    = G.makeColor 0.78 0.15 0.10 1.0
renderColor T.White  = G.makeColor 0.75 0.75 0.75 1.0
renderColor T.Yellow = G.makeColor 1.00 0.85 0.34 1.0
renderColor T.Green  = G.makeColor 0.00 0.54 0.36 1.0
renderColor T.Blue   = G.makeColor 0.00 0.64 1.00 1.0
renderColor T.Orange = G.makeColor 1.00 0.38 0.00 1.0
renderColor T.Hidden = G.makeColor 0.10 0.10 0.10 1.0

---------------------------------------------------------------------

-- renderGame' :: M.Game -> G.Picture
-- renderGame' g = ( renderFace . M.faceZneg . M.cube $ g ) <> renderSelected g
--
-- renderFace :: M.Face -> G.Picture
-- renderFace xs = G.pictures . map go $ indexed
--     where indexed    = [ ( (xs !! i) !! j, i, j ) | i <- [0..2], j <- [0..2] ]
--           go (x,r,c) = G.color (renderColor x)
--                        $ renderCellFace G.rectangleSolid r c
--
-- renderSelected :: M.Game -> G.Picture
-- renderSelected g = maybe G.Blank go . M.selected $ g
--     where go (r,c) = G.color G.white $ renderCellFace G.rectangleWire r c
--
-- renderCellFace :: (Float -> Float -> G.Picture) -> Int -> Int -> G.Picture
-- renderCellFace f r c = G.translate (go c) (go r) ( f 20 20 )
--     where go u = fromIntegral $ 22 * (u - 1)
