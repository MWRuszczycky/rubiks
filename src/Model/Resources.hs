module Model.Resources
    ( solved
    , helpStr
    ) where

import Model.Types  ( Cell  (..)
                    , Color (..)
                    , Cube  (..) )

-- =============================================================== --
-- Solved Rubiks cube

-- Exported

helpStr :: String
helpStr = unlines hs
    where hs = [ "Welcome to rubiks! A 3D-rubiks cube simulator!"
               , "rubiks is written in Haskell using Gloss.\n"
               , "Usage:    rubiks [--help]\n"
               , "Commands:"
               , "    * To quit the game, close the window or press Esc."
               , "    * To rotate a layer of the cube, left-click on"
               , "      a cell in the layer and drag it to an adjacent"
               , "      cell in the direction that you want to rotate."
               , "    * To rotate the whole cube, left-click adjacent"
               , "      to the cube and drag up-down or left-right."
               , "    * To scale the cube, right-click anywhere and drag"
               , "      up to reduce the size or down to increase the size."
               , "    * To undo your last move, press the space bar.\n"
               , "rubiks is free, open-source software maintained with full"
               , "documentation and licensing information at:"
               , "    https://github.com/MWRuszczycky/rubiks"
               ]

solved :: Cube
-- ^The cube is specified in the standard orientation.
solved = [ [ [ Cell Hidden Blue   Green  Hidden Hidden Red
             , Cell Hidden Hidden Green  Hidden Hidden Red
             , Cell White  Hidden Green  Hidden Hidden Red
             ]
           , [ Cell Hidden Blue   Hidden Hidden Hidden Red
             , Cell Hidden Hidden Hidden Hidden Hidden Red
             , Cell White  Hidden Hidden Hidden Hidden Red
             ]
           , [ Cell Hidden Blue   Hidden Orange Hidden Red
             , Cell Hidden Hidden Hidden Orange Hidden Red
             , Cell White  Hidden Hidden Orange Hidden Red
             ]
           ]
        ,  [ [ Cell Hidden Blue   Green  Hidden Hidden Hidden
             , Cell Hidden Hidden Green  Hidden Hidden Hidden
             , Cell White  Hidden Green  Hidden Hidden Hidden
             ]
           , [ Cell Hidden Blue   Hidden Hidden Hidden Hidden
             , Cell Hidden Hidden Hidden Hidden Hidden Hidden
             , Cell White  Hidden Hidden Hidden Hidden Hidden
             ]
           , [ Cell Hidden Blue   Hidden Orange Hidden Hidden
             , Cell Hidden Hidden Hidden Orange Hidden Hidden
             , Cell White  Hidden Hidden Orange Hidden Hidden
             ]
           ]
        ,  [ [ Cell Hidden Blue   Green  Hidden Yellow Hidden
             , Cell Hidden Hidden Green  Hidden Yellow Hidden
             , Cell White  Hidden Green  Hidden Yellow Hidden
             ]
           , [ Cell Hidden Blue   Hidden Hidden Yellow Hidden
             , Cell Hidden Hidden Hidden Hidden Yellow Hidden
             , Cell White  Hidden Hidden Hidden Yellow Hidden
             ]
           , [ Cell Hidden Blue   Hidden Orange Yellow Hidden
             , Cell Hidden Hidden Hidden Orange Yellow Hidden
             , Cell White  Hidden Hidden Orange Yellow Hidden
             ]
           ]
        ]
