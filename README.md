## History
###v0.1
1. The easel language can be parsed and built into an AST. The rules of the grammar are implemented in ast.ml, scanner.mll, and parser.mly. Please refer to the .es source files under tests/ to have a taste of easel program.
2. Steps for playing easel:
   * Say you already have an environment for building OCaml code.
   * $ cd /path/to/easel/compiler/source/
   * $ make
   * $ ./easel.native -a < tests/anonsquare.es
   * The AST of tests/anonsquare.es should be printed on your console
3. An initial testing system is also included, but needs a complete set of test cases and the check-fail functionality.
