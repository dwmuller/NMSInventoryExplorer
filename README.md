# NMS Inventory Explorer

This is an unfinished utility for flexibly viewing character
inventories in No Man's Sky, and exploring crafting possibilities in
relation to the current state of these inventories. It reads your
inventories from the game save file, and thus reflects their state as
of your last save.

This project is a work in progress, and this README currently contains
mostly notes about the development of the software. Eventually there
should be user instructions on the associated GitHub wiki.

I had the idea for this utility while playing the game when I got
frustrated sorting through my inventory in the game No Man's Sky, and
figuring what chain of crafting recipes I could apply to create a
particular product.

## Implementation

The software is written in [Racket], and uses its native GUI library.
To run the software yourself, download and install the DrRacket IDE,
open the gui.rkt file, and press F5 or click the Run button.

The IDE and its debugging tools are capable but not wonderful. But
with it, you can get programming very quickly, and the documentation
for the language and its libraries is quite good.

## Testing

The software currently locates your most recent game save file and
reads that. Thus it assumes that you have a game save file from a
recent version of the game.

TODO:
* Support older versions (without obfuscated JSON tags).
* Allow selection of file, include a test file in project.

## State of the software

Getting a workable data grid to display all selected inventories
turned out to be much harder than expected. (I'm no UI programmer.)
I have something working, sort of. See the lengthy TODO list at the
top of data-table.rkt.

Code exists for doing heuristic search for recipe chains (given a target 
item and count) that can be satisfied by a given inventory, but has not
been wired into any UI yet.



[Racket]: https://racket-lang.org/
