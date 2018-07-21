    ___    _  _     ___  __      __ ___               _       _
   |   \  | \| |   |_ _| \ \    / /| __|     o O O   (_)     (_)      o      __ _
   | |) | | .` |    | |   \ \/\/ / | _|     o         _       _      o      / o  |
   |___/  |_|\_|   |___|   \_/\_/  |___|   TS__[O]  _(_)_   _(_)_   TS__[O] \__,_|
 _|"""""|_|"""""|_|"""""|_|"""""|_|"""""| {======|_|"""""|_|"""""| {======|_|"""""|
 "`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'"`-0-0-'./o--000'"`-0-0-'"`-0-0-'./o--000'"`-0-0-'

                                 Chasing bottoms!

* Quick start

To run you need GHC (tested with 8.2.2 and 8.4.3). One can use either cabal,
nix-shell or stack to build the project. To use cabal, run:

> cabal run solver -- model.mdl trace.nbt

This produces a file `trace.nbt` with nanobot commands.

To run tests:

> cabal test

* Description

This is a basic solver which uses only one nanobot to build the whole model
with high resonance.

* Feedback

+ First, thank you for organizing this contest! Uphelding tradition of annual
  ICFP contest is important for our team;
+ The task itself is interesting and has many possible approaches;
+ Task specification is formal and complete; however somewhat difficult to
  read;
+ Binary formats for input and output are okay but bit streams are not fun to
  implement.
