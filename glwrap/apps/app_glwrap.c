int do_draw(int *canvas, int w, int h, int x, int y);

int main(int argc, const char *argv[])
{
#define DW 960
#define DH 960 
    int c[DW][DH]; 
    int x, y;
    for (x = 0; x < DW; x++) {
        for (y = 0; y < DH; y++) { 
            if (x > DW/2 && y > DH/2)
                c[x][y] = 0xffffff00; // RGBA
            else
                c[x][y] = 0x00000000;
        }
    }
    do_draw((int *) c, DW, DH, 0, 0);
    return 0;
}

