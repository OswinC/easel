int x, _y = 3;
bool b = false;
bool ___z = true;
float f1 = .7, f2 = 5.
    , f3 = 3e-4
    , f4 = 3.14159
    , f5 = .55e66;
pix p;

function void foo() {}

/*
 * comments /*/*
can be*/ nested
*/
*/

function int bar(int a, bool b, int c) { 
    if (b == true) if (a < c) return a + c; else return a - c;
}

int a=_y*4, b, c = bar(x, ___z, _y);
function int main() {
    print(bar(17, false, 25));
    return 0;
}

return main();
