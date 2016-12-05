/*pix COLOR;*/
/*COLOR = 0x3355aa00;*/
/*COLOR = {#33, #55, #aa};*/
pix canvas[960][720];
int W, H;
W = 960;
H = 720;
bool debug;
debug = false;

function int red(int x, int y) {
    float a = 0, b = 0, c, d, n = 0;
    while ((((c = a * a) + (d = b * b)) < 4.) && (n++ < 880.)) {
        b = 2. * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return 255 * ((n - 80.)/800. ^ 3.);
}

function int green(int x, int y) {
    float a = 0, b = 0, c, d, n = 0;
    while ((((c = a * a) + (d = b * b)) < 4.) && (n++ < 880.)) {
        b = 2. * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return 255 * ((n - 80.)/800. ^ .7);
}

function int blue(int x, int y) {
    float a = 0, b = 0, c, d, n = 0;
    while ((((c = a * a) + (d = b * b)) < 4.) && (n++ < 880.)) {
        b = 2. * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return 255 * ((n - 80.)/800. ^ .5);
}

function int draw_mandelbrot(int p_w) {
    int x, y;
    int r, g, b;
    for (y = 0; y < H; y++) {
        for (x = 0; x < W; x++) {
            r = red(x, y);
            g = green(x, y);
            b = blue(x, y);
            if (debug) {
                print(r);
                print(g);
                print(b);
            }
            canvas[x][y] = { r, g, b };
        }
    } 
    return draw(canvas, 0, 0);
}

int ret; 
ret = draw_mandelbrot(100);
return ret;
