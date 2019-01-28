module Model.Resources
    ( helpStr
    , solved
    ) where

import Model.Types ( Cell     (..)
                   , Color    (..)
                   , ColorMap (..)
                   , Cube     (..) )

-- =============================================================== --
-- Solved Rubiks cube

-- Exported

helpStr :: String
helpStr = unlines hs
    where hs = [ "Welcome to rubiks! A 3D-rubiks cube simulator!"
               , "rubiks is written in Haskell using Gloss.\n"
               , "Usage: rubiks [ --help | --version | -c PATH ]\n"
               , "Commands:\n"
               , "    * To quit the game, close the window or press Esc.\n"
               , "    * To rotate a layer of the cube, left-click on"
               , "      a cell in the layer and drag it to an adjacent"
               , "      cell in the direction that you want to rotate.\n"
               , "    * To rotate the whole cube, left-click adjacent"
               , "      to the cube and drag up-down or left-right.\n"
               , "    * To scale the cube, right-click anywhere and drag"
               , "      up to reduce the size or down to increase the size.\n"
               , "    * To undo your last move, press the space bar.\n"
               , "    * To return the cube to the solved state, press 's'."
               , "      This clears all previous moves.\n"
               , "    * To add n random moves to the current cube, press n"
               , "      where n is a digit between 0 and 9. Pressing 0 will"
               , "      add 10 random moves. So to add 15 random moves,"
               , "      press 0 then 5. Doing this will clear all previous"
               , "      moves, so you cannot undo it!\n"
               , "    * To play with a custom colored cube, use the -c option"
               , "      followed by the PATH to your color file. A color file"
               , "      should contain exactly six rgb hexcodes for each of"
               , "      the six colors you want to use. For example,"
               , "           #c7c481"
               , "           #e8b776"
               , "           #ff918f"
               , "           #b376e8"
               , "           #82bfff"
               , "           #d6d6d6"
               , "      you can also leave off the leading '#' character in"
               , "      each code (e.g., c7c481 will also work instead of"
               , "      #c7c481). If there is any error reading the colors,"
               , "      then the game will start with the default colors.\n"
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
