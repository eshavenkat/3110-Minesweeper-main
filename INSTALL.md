# Installation Instructions

Our project utilizes the `graphics` library and the `camlimages` library in OCaml, so you'll need to install it by running the following in your terminal.

```
opam install graphics
opam install ocamlimages
```

For macos, please follow the following link and download this X11 server:

https://www.xquartz.org/

Once downloaded, launch it and reopen a terminal window.

In order to start the game you should run one of the following in your terminal:

### 1
```
dune exec bin/main.exe
```
Runs the program with the default settings.
### 2
```
dune exec bin/main.exe <difficulty>
```
where difficulty is one of `easy`, `medium`, or `hard`. This setting determines the number of mines that will be placed in the game board.
### 3
```
dune exec bin/main.exe <difficulty> <grid size> <grid depth>
```
where `grid size` is the length and width of the square board, and `grid depth` is the depth of the board