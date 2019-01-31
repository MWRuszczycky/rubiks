import qualified Graphics.Gloss as G
import qualified Model.Types    as T
import Control.Exception             ( SomeException
                                     , try             )
import Data.Version                  ( showVersion     )
import Paths_rubiks                  ( version         )
import System.Environment            ( getArgs         )
import System.Random                 ( getStdGen       )
import Model.Geometry                ( rotXMat         )
import Model.Graphics                ( makeColorMap
                                     , readRGBHexCode  )
import Controller                    ( routeEvent      )
import View                          ( renderGame      )
import Model.Resources               ( helpStr
                                     , solved          )

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
initGame []               = Right <$> newGame ( makeColorMap Nothing )
initGame (x:xs)
    | elem x criesForHelp = return . Left  $ helpStr
    | elem x versionReqs  = return . Left  $ versionStr
    | elem x colorReqs    = fmap     Right $ newGame =<< getColorMap xs
    | otherwise           = return . Left  $ errStr
    where criesForHelp = [ "help", "--help", "-h", "info", "--info" ]
          versionReqs  = [ "version", "--version", "-v"             ]
          colorReqs    = [ "-c"                                     ]
          errStr       = "Unrecognized command.\nTry: rubiks --help"
          versionStr   = "rubiks-" ++ ( showVersion version )

newGame :: T.ColorMap -> IO (G.Display, T.Game)
-- ^Provide an initialized game state and window.
newGame colorMap = do
    let dimensions = (300, 300)
        window     = G.InWindow "Rubiks" dimensions (60, 60)
    newGen <- getStdGen
    return $ (,) window
                 T.Game { T.cube        = solved
                        , T.rotation    = rotXMat 0
                        , T.renderColor = colorMap
                        , T.mode        = T.Idle
                        , T.toScreen    = 500
                        , T.scaling     = 1
                        , T.dim         = dimensions
                        , T.moves       = []
                        , T.gen         = newGen
                        }

getColorMap :: [String] -> IO T.ColorMap
-- ^Try to read a color map based on file path from the command line
-- arguments. If this fails, then return the default color map.
getColorMap []     = return . makeColorMap $ Nothing
getColorMap (fp:_) = go <$> try (readFile fp)
    where go :: Either SomeException String -> T.ColorMap
          go (Left _)  = makeColorMap Nothing
          go (Right x) = makeColorMap . mapM readRGBHexCode . words $ x
