module Main where

import qualified Graphics.Gloss as G
import qualified Model          as M

main :: IO ()
main = putStrLn "Rubiks cube simulator!"

viewRubiks :: M.Cube -> IO ()
viewRubiks c = do
    putStrLn "X-pos" >> (mapM_ print . M.faceXpos) c
    putStrLn "X-neg" >> (mapM_ print . M.faceXneg) c
    putStrLn "Y-pos" >> (mapM_ print . M.faceYpos) c
    putStrLn "Y-neg" >> (mapM_ print . M.faceYneg) c
    putStrLn "Z-pos" >> (mapM_ print . M.faceZpos) c
    putStrLn "Z-neg" >> (mapM_ print . M.faceZneg) c
