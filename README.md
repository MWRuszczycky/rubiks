# rubiks

## Overview

This is a 3D-Rubik's cube simulator written entirely in Haskell using [Gloss](https://hackage.haskell.org/package/gloss). It is still a work in progress, but the basic game works.

![rubiks demo](demos/demo1.gif)

## Play

Right now, the play is pretty rudimentary, but it seems to work! Click on a cell and drag it the way you want to rotate the layer. To rotate the whole cube, click next to the cube and drap up-down or left-right. **To quit the game, press `Esc`.**

## Installation and uninstallation

The game uses the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/), so to clone and compile the repository, try
```sh
git clone https://github.com/MWRuszczycky/rubiks.git
cd rubiks
stack build
```
To run the game from within the repository after building, try
```sh
stack exec rubiks
```
Alternatively, you can locally install it using
```sh
stack install
```
from within the repository and *Stack* will tell you where the binary has been placed. You should now be able to run the game from either the command line or the program menu.

To uninstall, delete the repository and the binary installed by stack if you did a local install. There are no other configuration files.

## Known issues and to-do

1. The cube is rendered mirror-flipped upside-down. It works fine, but this bugs me.
2. Make it easier to quit rather than having to use `Esc`.
3. Implement undos.
4. Implement randomized starts rather than beginning with a solved cube.
5. Implement saving of progress.
6. Maybe allow user to choose their own colors.
7. Write a better README.
