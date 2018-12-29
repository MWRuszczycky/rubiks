import qualified Graphics.Gloss as G
import qualified Model.Types    as T
import System.Environment            ( getArgs         )
import Model.Geometry                ( rotXMat         )
import View                          ( renderGame      )
import Controller                    ( routeEvent      )
import Model.Resources               ( solved
                                     , helpStr         )

main :: IO ()
main = do
    start <- initGame <$> getArgs
    case start of
         Left msg    -> putStrLn msg
         Right (w,g) -> G.play w G.black 0 g renderGame routeEvent ( \ _ -> id )

---------------------------------------------------------------------
-- Resource acquisition and game initialization

initGame :: [String] -> Either String (G.Display, T.Game)
-- ^Decide what to do based on the user input.
initGame []               = Right newGame
initGame (x:xs)
    | elem x criesForHelp = Left helpStr
    | otherwise           = Left errStr
    where criesForHelp = [ "help", "--help", "-h", "info", "--info" ]
          errStr       = "Unrecognized command.\nTry: rubiks --help"

newGame :: (G.Display, T.Game)
-- ^Provide an initialized game state and window.
newGame = (window, game)
    where dimensions = (300, 300)
          window     = G.InWindow "Rubiks" dimensions (60, 60)
          game       = T.Game { T.cube     = solved
                              , T.rotation = rotXMat 0
                              , T.mode     = T.Idle
                              , T.toScreen = 500
                              , T.scaling  = 1
                              , T.dim      = dimensions
                              , T.moves    = []
                              }
