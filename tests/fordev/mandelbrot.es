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
    float a = 0., b = 0., c, d, n = 0.;
    while ((c = a * a) + (d = b * b) < 4. && n++ < 880.) {
        b = 2. * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return 255 * (((n - 80.)/800.) ^ 3.);
}

function int green(int x, int y) {
    float a = 0., b = 0., c, d, n = 0.;
    while ((c = a * a) + (d = b * b) < 4. && n++ < 880.) {
        b = 2. * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return 255 * (((n - 80.)/800.) ^ .7);
}

function int blue(int x, int y) {
    float a = 0., b = 0., c, d, n = 0.;
    while ((c = a * a) + (d = b * b) < 4. && n++ < 880.) {
        b = 2. * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return 255 * (((n - 80.)/800.) ^ .5);
}

function pix mandelbrot(int x, int y) {
    pix p;
    p = { red(x, y), green(x, y), blue(x, y), 0 };
    return p;
}

graph(canvas, W, H, mandelbrot);
draw(canvas, 0, 0);
