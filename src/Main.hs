import qualified Graphics.Gloss as G
import qualified Model.Types    as T
import Model.Geometry                ( rotXMat    )
import Model.Resources               ( solved     )
import View                          ( renderGame )
import Controller                    ( routeEvent )

main :: IO ()
main = do
    let (g, w) = initGame
    G.play w G.black 0 g renderGame routeEvent ( \ _ -> id )

initGame :: ( T.Game, G.Display )
initGame = ( g, w )
    where w = G.InWindow "Rubiks" (300, 300) (60, 60)
          g = T.Game { T.cube     = solved
                     , T.rotation = rotXMat 0
                     , T.mode     = T.Idle
                     , T.toScreen = 250
                     }
