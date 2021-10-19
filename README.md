# Recursive : Let's draw !
> TP2 - Semester 1 - Epita - From 10/01/21 to 10/19/21

## Loading the Graphics module

In this project, we will use the [Graphics](https://ocaml.github.io/graphics/graphics/Graphics/index.html) module of Caml. This library provides a set of portable drawing primitives.

For Caml versions up to 4.08:
```
# load "graphics.cma" ;;
```

For later versions:
```
# use "topfing" ;;
# require "graphics" ;;
```

### Opening the graphics screen
```
Graphics.open_graph " 800x800" ;;
```
