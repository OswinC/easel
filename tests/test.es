int x, _y = 3;
bool b = false;
bool ___z = true;
float f1, f2, f3, f4;
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
