function int foo(int x, int y) { return x + y; }

function int bar(function int (int, int) fp) { return fp(9, 5); }

print(bar(foo));
