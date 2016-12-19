# The easel compiler
The goal of easel is to create a simple interface for mathematical data and functions to be transformed into images, and so easel is capable of performing both basic and advanced arithmetic, such as trigonometric and logarithmic functions using built-in functions and simple operators. Additionally, functions are the core of the language and so they can be passed as parameters and declared anonymously. Our hope is to be able to make complex images while keeping the programming as simple as possible.

# Installing Dependencies and Running the Compiler
The first step in running easel programs is downloading the proper dependencies. In order output easel drawings, you must have OpenGL installed on your machine. To install all necessary libraries, run the following bash command:
```
$ sudo apt-get install freeglut3 freeglut3-dev libglew-dev gle-graphics
``` 
Run make in the glwrap/ directory and the top directory to build easel.native
```
$ cd glwrap
$ make
$ cd ../
$ make
```
However, ecc.sh, which wraps the compilation pipeline, is recommended to use
```
$ ./ecc.sh
Usage: ecc.sh [options] <.es files>
    -c               Compile to executable (default)
    -l               Output LLVM IR code
    -a               Output pretty-printing of AST
    -o <file name>   Specify output file name
    -h               Print this help
``` 
To compile and run a program:
```
$ ./ecc.sh demo/mandelbrot.es
$ ./mandelbrot 
```

# History
##v1.0
1. Semantic checking, code generation, and the drawing feature are supported!
   
##v0.1
1. The easel language can be parsed and built into an AST. The rules of the grammar are implemented in ast.ml, scanner.mll, and parser.mly. Please refer to the .es source files under tests/ to have a taste of easel program.
2. Steps for playing easel:
   * Say you already have an environment for building OCaml code.
   * $ cd /path/to/easel/compiler/source/
   * $ make
   * $ ./easel.native -a < tests/anonsquare.es
   * The AST of tests/anonsquare.es should be printed on your console
3. An initial testing system is also included, but needs a complete set of test cases and the check-fail functionality.
