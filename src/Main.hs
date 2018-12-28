import qualified Graphics.Gloss as G
import qualified Model.Types    as T
import Codec.BMP                     ( BMP
                                     , readBMP         )
import Control.Monad.Except          ( ExceptT    (..)
                                     , liftEither
                                     , liftIO
                                     , runExceptT      )
import Model.Geometry                ( rotXMat         )
import Model.Resources               ( solved          )
import View                          ( renderGame      )
import Controller                    ( routeEvent      )

main :: IO ()
main = do
    start <- runExceptT . initGame $ "resources/buttons.bmp"
    case start of
         Left msg    -> putStrLn msg
         Right (w,g) -> G.play w G.black 0 g renderGame routeEvent ( \ _ -> id )

---------------------------------------------------------------------
-- Resource acquisition and game initialization

initGame :: FilePath -> ExceptT String IO (G.Display, T.Game)
-- ^Initialize the game state and the window.
initGame path = do
    let window = G.InWindow "Rubiks" (300, 300) (60, 60)
    bmp <- readBitMap path
    return ( window
           , T.Game { T.cube     = solved
                    , T.rotation = rotXMat 0
                    , T.mode     = T.Idle
                    , T.toScreen = 500
                    , T.scaling  = 1
                    , T.moves    = []
                    , T.btnSheet = bmp
                    }
           )

readBitMap :: FilePath -> ExceptT String IO BMP
-- ^Wrapper for reading uncompressed 24-bit bitmaps into ExceptT.
readBitMap = ExceptT . fmap (either getErrString Right) . readBMP
    where getErrString e = Left $ "Cannot read bitmaps!\nDetails: " ++ show e
