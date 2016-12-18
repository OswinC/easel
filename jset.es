/* create a julia set 
based on code from codegolf.stackexchange.com/questions/35569/tweetable-mathematical-art */

pix canv[960][720];
int W = 960, H = 720;

function void graph(pix[960][] canvas, int w, int h, function pix (int, int) paint) {
    int x, y;
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            canvas[x][y] = paint(x,y);
        }
    }
}

function float dim(int x) {
    float f = (x - H/2.0)/(H/2.0);
    return f;
}

function int red(int i, int j) {
    float x, y, X, Y, n;
    x = dim(i);
    y = dim(j);
    while (n<200 && (x*x + y*y)<1) {
        X = x*x;
        Y = y*y;
        x = X-Y + 0.36237;
        y = 2*x*y + 0.32;
        n++;
    }
    
    return log(2.718281, n) * 256;
}


function int green(int i, int j) {
    float x, y, X, Y, n;
    x = dim(i);
    y = dim(j);
    while (n<200 && (x*x + y*y)<1) {
        X=x;
        Y=y;
        x = X*X - Y*Y - 0.7;
        y = 2*X*Y+0.27015;
        n++;
    }

    return log(2.718281, n) * 96;
}

function int blue(int i, int j) {
    float x, y, X, Y, n;
    x = dim(i);
    y = dim(j);
    while (n<600 && (x*x + y*y)<1) {
        X=x;
        Y=y;
        x = X*X - Y*Y + 0.36237;
        y = 2*X*Y+0.32;
        n++;
    }

    return log(2.718281, n)*128;
}

function pix paint_jset(int x, int y) {
    return { red(x,y), green(x,y), blue(x,y), 0 };
}

graph(canv, W, H, paint_jset);
draw(canv, 0, 0);

