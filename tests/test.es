int x, _y = 3;
bool b = false;
bool ___z = true;
float f1 = .7, f2 = 5.
    , f3 = 3e-4
    , f4 = 3.14159
    , f5 = .55e66;
pix sp1, canvas[640][480] = [[0]],
    sp2[3] = [#ffffff, #7a8b9c,
    x * y - 30 ^ .5];

function void foo() {}

/*
 * comments /*/*
can be*/ nested
*/
*/

function int bar(int x, int y, bool b, int c[][]) { 
    if (b == true) if (a < c) return a + c; else return a - c;
    if (x > 3 || y < 0 && !b) c[x--][y^^]++--**//^^;
}

function void paint(pix canvas[][], function pix (int, int) painter) {
    int x, y;
    for (y = canvas.min; y < canvas.max; y++) {
        for (x = canvas[y].min; x < canvas[y].max; x++) {
            canvas[x][y] = painter(x, y);
        }
    }
}

int a=_y*4, b=0xF3, c = bar(x, ___z, _y);
a = b = c = 2 * 3;
int h = #FFCC;

function int (float, int) f1;

function int (float, int) f4 = function int (float deg, int rad) {
};

/* An anonymous function returning an anonymous function */
function function int (float, int) (bool) f5 =
    function function int (float, int) (bool flag) {
        if (flag) return function int (float x, int y) { return 10; };
        return function int (float x, int y) { return 3; };
    };

function int main() {
    print(bar(17, false, 25));
    return 0;
}

return main();
