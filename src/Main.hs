
import qualified Graphics.Gloss.Interface.IO.Interact as G
import qualified Graphics.Gloss                       as G
import qualified Model                                as M
import View                                                ( renderGame )

main :: IO ()
main = do
    let (g, w) = initGame
    G.play w G.black 10 g renderGame routeEvent updateTime

initGame :: ( M.Game, G.Display )
initGame = ( M.Game { M.cube = M.solved }
           , G.InWindow "Rubiks" (300, 300) (60, 60)
           )

routeEvent :: G.Event -> M.Game -> M.Game
routeEvent _ = id

updateTime :: Float -> M.Game -> M.Game
updateTime dt = id

viewRubiks :: M.Cube -> IO ()
viewRubiks c = do
    putStrLn "X-pos" >> (mapM_ print . M.faceXpos) c
    putStrLn "X-neg" >> (mapM_ print . M.faceXneg) c
    putStrLn "Y-pos" >> (mapM_ print . M.faceYpos) c
    putStrLn "Y-neg" >> (mapM_ print . M.faceYneg) c
    putStrLn "Z-pos" >> (mapM_ print . M.faceZpos) c
    putStrLn "Z-neg" >> (mapM_ print . M.faceZneg) c
