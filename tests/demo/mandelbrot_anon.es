pix canvas[960][840];
int W = 960, H = 840;

function pix[960][] graph(pix[960][] canv, int w, int h, function pix (int, int) painter) {
    int x, y;
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            canv[x][y] = painter(x, y);
        }
    }
    return canv;
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

draw_size(graph(canvas, W, H, function pix (int x, int y) {
    pix p = 0;
    p.red = red(x, y);
    p.green = green(x, y);
    p.blue = blue(x, y);
    return p;
}), W, H, 200, 0);
