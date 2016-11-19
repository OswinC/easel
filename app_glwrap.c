#define W 480
#define H 480
#include <stdio.h>

int canvas[W][H];

int do_draw(int *c, int w, int h, int x, int y);

int main(int argc, const char *argv[])
{
    int x, y;
    for (x = W / 2; x < W; x++) {
        for (y = H / 2; y < H; y++) { 
            canvas[x][y] = 0x00ff00ff; // RGBA
        }
    }
    do_draw((int *) canvas, W, H, 0, 0);
    return 0;
}

