int SQ_W = 300;
pix canvas[960][960] = [[0]];

function pix[][] graph(pix[][] canvas, function pix (int, int) painter) {
    int x, y;
    for (y = canvas.min; y <= canvas.max; y++) {
        for (x = canvas[y].min; x <= canvas[y].max; x++) {
            canvas[x][y] = painter(x, y);
        }
    }
    return canvas;
}

function pix square(int x, int y) {
    pix red = #ff0000, white = #0;
    int x_lbound = canvas[0].min + (canvas[0].size - SQ_W) / 2,
        x_hbound = canvas[0].max - (canvas[0].size - SQ_W) / 2,
        y_lbound = canvas.min + (canvas.size - SQ_W) / 2,
        y_hbound = canvas.max - (canvas.size - SQ_W) / 2;

    if (x >= x_lbound && x <= x_hbound && y >= y_lbound && y <= y_hbound)
        return red;
    else
        return white;
}

draw(graph(canvas, square), 200, 200);
