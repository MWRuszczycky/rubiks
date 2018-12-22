module Resources
    ( solved
    ) where

import Types ( Cell  (..)
             , Color (..)
             , Cube  (..) )

-- =============================================================== --
-- Solved Rubiks cube

-- Exported

solved :: Cube
solved = [ [ [ Cell Hidden Blue   Hidden Orange Hidden Red
             , Cell Hidden Hidden Hidden Orange Hidden Red
             , Cell White  Hidden Hidden Orange Hidden Red
             ]
           , [ Cell Hidden Blue   Hidden Hidden Hidden Red
             , Cell Hidden Hidden Hidden Hidden Hidden Red
             , Cell White  Hidden Hidden Hidden Hidden Red
             ]
           , [ Cell Hidden Blue   Green Hidden Hidden Red
             , Cell Hidden Hidden Green Hidden Hidden Red
             , Cell White  Hidden Green Hidden Hidden Red
             ]
           ]
        ,  [ [ Cell Hidden Blue   Hidden Orange Hidden Hidden
             , Cell Hidden Hidden Hidden Orange Hidden Hidden
             , Cell White  Hidden Hidden Orange Hidden Hidden
             ]
           , [ Cell Hidden Blue   Hidden Hidden Hidden Hidden
             , Cell Hidden Hidden Hidden Hidden Hidden Hidden
             , Cell White  Hidden Hidden Hidden Hidden Hidden
             ]
           , [ Cell Hidden Blue   Green Hidden Hidden Hidden
             , Cell Hidden Hidden Green Hidden Hidden Hidden
             , Cell White  Hidden Green Hidden Hidden Hidden
             ]
           ]
        ,  [ [ Cell Hidden Blue   Hidden Orange Yellow Hidden
             , Cell Hidden Hidden Hidden Orange Yellow Hidden
             , Cell White  Hidden Hidden Orange Yellow Hidden
             ]
           , [ Cell Hidden Blue   Hidden Hidden Yellow Hidden
             , Cell Hidden Hidden Hidden Hidden Yellow Hidden
             , Cell White  Hidden Hidden Hidden Yellow Hidden
             ]
           , [ Cell Hidden Blue   Green Hidden Yellow Hidden
             , Cell Hidden Hidden Green Hidden Yellow Hidden
             , Cell White  Hidden Green Hidden Yellow Hidden
             ]
           ]
        ]
