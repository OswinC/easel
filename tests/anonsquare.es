pix canvas[960][960] = [[0]];
int SQ_W = 300;

function pix[][] graph(pix canvas[][], function pix (int, int) painter) {
    int x, y;
    for (y = canvas.min; y <= canvas.max; y++) {
        for (x = canvas[y].min; x <= canvas[y].max; x++) {
            canvas[x][y] = painter(x, y);
        }
    }
    return canvas;
}

draw(graph(canvas, function pix (int x, int y) {
    pix red = #ff0000, white = #0;
    int x_lb = canvas[0].min + (canvas[0].size - SQ_W) / 2,
        x_hb = canvas[0].max - (canvas[0].size - SQ_W) / 2,
        y_lb = canvas.min + (canvas.size - SQ_W) / 2,
        y_hb = canvas.max - (canvas.size - SQ_W) / 2; 
    if (x >= x_lb && x <= x_hb && y >= y_lb && y <= y_hb) return red;
    else return white;
}), 200, 200);
