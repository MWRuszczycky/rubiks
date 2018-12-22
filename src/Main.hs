import qualified Graphics.Gloss as G
import qualified Types          as T
import Resources                     ( solved     )
import View                          ( renderGame )
import Controller                    ( routeEvent
                                     , updateTime )

main :: IO ()
main = do
    let (g, w) = initGame
    G.play w G.black 10 g renderGame routeEvent updateTime

initGame :: ( T.Game, G.Display )
initGame = ( T.Game { T.cube = solved, T.selected = Nothing, T.theta = 0 }
           , G.InWindow "Rubiks" (300, 300) (60, 60)
           )
