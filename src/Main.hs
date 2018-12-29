import qualified Graphics.Gloss as G
import qualified Model.Types    as T
import Model.Geometry                ( rotXMat         )
import Model.Resources               ( solved          )
import View                          ( renderGame      )
import Controller                    ( routeEvent      )

main :: IO ()
main = do
    (w, g) <- initGame
    G.play w G.black 0 g renderGame routeEvent ( \ _ -> id )

---------------------------------------------------------------------
-- Resource acquisition and game initialization

initGame :: IO (G.Display, T.Game)
-- ^Initialize the game state and the window.
initGame = do
    let dimensions = (300, 300)
        window     = G.InWindow "Rubiks" dimensions (60, 60)
    return ( window
           , T.Game { T.cube     = solved
                    , T.rotation = rotXMat 0
                    , T.mode     = T.Idle
                    , T.toScreen = 500
                    , T.scaling  = 1
                    , T.dim      = dimensions
                    , T.moves    = []
                    }
           )
