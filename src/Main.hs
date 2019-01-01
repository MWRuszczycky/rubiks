import qualified Graphics.Gloss as G
import qualified Model.Types    as T
import Data.Version                  ( showVersion     )
import Paths_rubiks                  ( version         )
import System.Environment            ( getArgs         )
import System.Random                 ( getStdGen       )
import Model.Geometry                ( rotXMat         )
import View                          ( renderGame      )
import Controller                    ( routeEvent      )
import Model.Resources               ( solved
                                     , helpStr         )

main :: IO ()
main = do
    start <- getArgs >>= initGame
    case start of
         Left msg    -> putStrLn msg
         Right (w,g) -> G.play w G.black 0 g renderGame routeEvent ( \ _ -> id )

---------------------------------------------------------------------
-- Resource acquisition and game initialization

initGame :: [String] -> IO ( Either String (G.Display, T.Game) )
-- ^Decide what to do based on the user input.
initGame []               = Right <$> newGame
initGame (x:xs)
    | elem x criesForHelp = return . Left $ helpStr
    | elem x versionReqs  = return . Left $ versionStr
    | otherwise           = return . Left $ errStr
    where criesForHelp = [ "help", "--help", "-h", "info", "--info" ]
          versionReqs  = [ "version", "--version", "-v" ]
          errStr       = "Unrecognized command.\nTry: rubiks --help"
          versionStr   = "rubiks-" ++ ( showVersion version )

newGame :: IO (G.Display, T.Game)
-- ^Provide an initialized game state and window.
newGame = do
    let dimensions = (300, 300)
        window     = G.InWindow "Rubiks" dimensions (60, 60)
    newGen <- getStdGen
    return $ (,) window
                 T.Game { T.cube     = solved
                        , T.rotation = rotXMat 0
                        , T.mode     = T.Idle
                        , T.toScreen = 500
                        , T.scaling  = 1
                        , T.dim      = dimensions
                        , T.moves    = []
                        , T.gen      = newGen
                        }
