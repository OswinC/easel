pix canvas[960][720];
int W = 960, H = 720;

function void graph(pix[960][] canv, int w, int h, function pix (int, int) painter) {
    int x, y;
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            canv[x][y] = painter(x, y);
        }
    }
}

function int red(int x, int y) {
    float a = 0., b = 0., d, n = 0.;
    while (a * a + (d = b * b) < 4. && n++ < 8192.) {
        b = 2. * a * b + y / 5e4 + .6;
        a = a * a - d + x / 5e4 + .34;
    }
        return (n / 8);
    
}


function int green(int x, int y) {
    return 9 * red(x, y);
}

function int blue(int x, int y) {
    return 16 * red(x, y);
}

function pix paint_mandelbrot(int x, int y) {
    return { red(x, y), green(x, y), blue(x, y), 0 };
}

graph(canvas, W, H, paint_mandelbrot);

draw(canvas, 0, 0);
