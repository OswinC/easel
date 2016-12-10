pix COLOR;
COLOR = 0x3355aa00;
pix canvas[960][720];
int W, H;
W = 960;
H = 720;

function int plaid(int p_w) {
    int x, y;
    for (y = 0; y < H; y++) {
        for (x = 0; x < W; x++) {
            if (((x / p_w) % 2) == ((y / p_w) % 2))
                canvas[x][y] = COLOR;
        }
    } 
    return draw(canvas, 0, 0);
}

int ret; 
ret = plaid(100);
return ret;
