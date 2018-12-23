module Controller
    ( routeEvent
    , updateTime
    ) where

import qualified Graphics.Gloss.Interface.IO.Interact as G
import qualified Graphics.Gloss                       as G
import qualified Types                                as T

routeEvent :: G.Event -> T.Game -> T.Game
routeEvent _ = id

updateTime :: Float -> T.Game -> T.Game
updateTime dt g = g { T.theta = th }
    where th = T.theta g + dt * 2 * pi / 5

-- routeEvent :: G.Event -> M.Game -> M.Game
-- routeEvent (G.EventKey (G.SpecialKey  G.KeyDown   ) G.Down _ _     ) g
--     = rotate M.XAxis M.Pos90 g
-- routeEvent (G.EventKey (G.SpecialKey  G.KeyUp     ) G.Down _ _     ) g
--     = rotate M.XAxis M.Neg90 g
-- routeEvent (G.EventKey (G.SpecialKey  G.KeyRight  ) G.Down _ _     ) g
--     = rotate M.YAxis M.Neg90 g
-- routeEvent (G.EventKey (G.SpecialKey  G.KeyLeft   ) G.Down _ _     ) g
--     = rotate M.YAxis M.Pos90 g
-- routeEvent (G.EventKey (G.MouseButton G.LeftButton) G.Down _ (r,c) ) g
--     = selectCell r c g
-- routeEvent _                                                         g
--     = g

-- rotate :: M.Axis -> M.Rotation -> M.Game -> M.Game
-- rotate M.XAxis r g = maybe g ( go $ M.cube g ) . M.selected $ g
--     where go c (_,i) = g { M.cube = M.rotateLayer M.XAxis r i c }
-- rotate M.YAxis r g = maybe g ( go $ M.cube g ) . M.selected $ g
--     where go c (i,_) = g { M.cube = M.rotateLayer M.YAxis r i c }
-- rotate M.ZAxis r g = g

-- selectCell :: Float -> Float -> M.Game -> M.Game
-- selectCell r c g = g { M.selected = mbRC }
--     where mbRC = (,) <$> getCoordinate c <*> getCoordinate r

-- getCoordinate :: Float -> Maybe Int
-- getCoordinate t
--     | t < -32           = Nothing
--     | t < -12           = Just 0
--     | t > -10 && t < 10 = Just 1
--     | t >  12 && t < 32 = Just 2
--     | otherwise         = Nothing
