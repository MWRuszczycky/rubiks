# rubiks

## Overview

This is a 3D-Rubik's cube simulator written entirely in Haskell using [Gloss](https://hackage.haskell.org/package/gloss). It is still a work in progress, but the basic game works.

![rubiks demo](demos/demo1.gif)

## Play

Right now, the play is pretty rudimentary, but it seems to work!
* Left-click on a cell and drag it the way you want to rotate the layer.
* To rotate the whole cube, left-click next to the cube and drag up-down or left-right.
* To scale the cube, right-click and drag up-down.
* To undo the last move, press the space bar.
* **To quit the game, press `Esc`.**

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

* Make it easier to quit rather than having to use `Esc`.
* Implement randomized starts rather than beginning with a solved cube.
* Implement saving of progress.
* Maybe allow user to choose their own colors.
* Write a better README.
* Several functions need commenting and the Controller could probably be better refactored.
* Improve the entry point and add help/about functionality.
* Add clickable buttons for undos, quitting, saving, etc.
* Improve the scaling algorithm to better handle perspective.
