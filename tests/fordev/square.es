int SQ_W = 300;
pix canvas[960][960];

int i;
int j;
for (i=0; i<canvas.size; i++) {
    for(j=0; j<canvas[i].size;j++)
       canvas[i][j]=0; 
}

function pix[][] graph(pix[][] canvas, function pix (int, int) painter) {
    int x;
    int y;
    for (y = 0; y <= canvas.size; y++) {
        for (x = 0; x <= canvas[y].size; x++) {
            canvas[x][y] = painter(x, y);
        }
    }
    return canvas;
}

function pix square(int x, int y) {
    pix red = #ff0000, white = #0;
    int x_lbound = 0 + (canvas[0].size - SQ_W) / 2,
        x_hbound = canvas[0].size - (canvas[0].size - SQ_W) / 2,
        y_lbound = 0 + (canvas.size - SQ_W) / 2,
        y_hbound = canvas.size - (canvas.size - SQ_W) / 2;

    if (x >= x_lbound && x <= x_hbound && y >= y_lbound && y <= y_hbound)
        return red;
    else
        return white;
}

draw(graph(canvas, square), 200, 200);
