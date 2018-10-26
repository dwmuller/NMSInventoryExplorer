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

## Implementation Notes

The software is written in [Racket], and uses its native GUI library.
To run the software yourself, download and install the DrRacket IDE,
open the NMSInventoryExplorer.rkt file, and press F5 or click the Run
button.

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

## Building

To build a release, simply open the NMSInventoryExplorer.rkt file with
DrRacket, and choose menu item "Racket", then "Create
Executable...". In the dialog, select "Distribution" as the "Type",
and "GRacket" as the "Base".  Make sure to check "Embed DLLs in the
executable?"  Click "Create" and wait. You'll end up with a zip file
containing the executable and supporting subdirectories. Total
unpacked size is on the order of 45MB.

## State of the software

You can currently select the inventories whose contents you want to
view, and see the contents of selected inventories in a reasonably
compact and flexible form. The UI could use a little more tweaking
to make the data grid easier on the eyes.

Code exists for doing heuristic search for recipe chains (given a target 
item and count) that can be satisfied by a given inventory, but has not
been wired into any UI yet.

Hello Games tried to obscure the save file with a release in late 2018
that changed the plain English JSON tags to obfuscated bits of text.
The mapping of these tags in save-file.rkt is incomplete, based on what
I was able to figure out so far from one of my own save files.
Similarly, the information in items.rkt, which enumerates the items in
the game with their base values (where known) is not complete, nor is
the listing of recipes in recipes.rkt.

[Racket]: https://racket-lang.org/
