int i, j, a[3][3];

function int foo(function int (int, int) fp, int x, int y) {
x = x * 10;
y = y + 1;    
return fp(x, y);}

for (i = 0; i < a.size; i++){
    for (j = 0; j < a[0].size; j++){
        print(foo(function int (int x, int y) {
            return a[i][j] = 100 + x + y;}, i, j));
    }
}




