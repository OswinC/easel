int x, _y = 3;
bool b = false;
bool ___z = true;
float f1 = .7, f2 = 5.
    , f3 = 3e-4
    , f4 = 3.14159
    , f5 = .55e66;
pix sp1, canvas[640][480], sp2;

function void foo() {}

/*
 * comments /*/*
can be*/ nested
*/
*/

function int bar(int a, bool b, int c) { 
    if (b == true) if (a < c) return a + c; else return a - c;
}

function pix[][] paint(pix canvas[][], pix color) {
    int x, y;
    /*
    for (y = canvas.min; y < canvas.max; y++) {
        for (x = canvas[y].min; x < canvas[y].max; x++) {
            canvas[x][y] = color;
        }
    }
    */
    if (x > 3 || y < 0 && color != 255) canvas[x--][y^^]++--**//^^;
    return canvas;
}

int a=_y*4, b=3, c = bar(x, ___z, _y);

function int main() {
    print(bar(17, false, 25));
    return 0;
}

return main();
