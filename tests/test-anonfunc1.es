function int foo(function int (int, int) fp) {
    return fp(3, 5);
}

print(foo(function int (int x, int y) {
    return x * y;
}));
