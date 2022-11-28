# advent-of-code-2022
A repository for the Advent of Code 2022 puzzle solutions.

Haskell is not my first language (or my second, or my third...) & I am very much inexperienced with it. As a result while my solutions *do* work, the algorithms & syntax employed should **by no means be considered optimal and/or even good**...

The solutions to the puzzles will contain a mix of existing library functions where convenient & bespoke reimplementations of existing functions where I decided I would "reinvent the wheel" just to gain better understanding.

## Building the project

*advent-of-code-2022* is built using the standard legacy **cabal** build steps. It can be configured and built as follows :

```
#!/bin/bash
cabal configure
cabal build
cabal run
```