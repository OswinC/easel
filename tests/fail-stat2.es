function int add(int x, int y)
{
    int a;
    a = 5;
    return a + x + y;
    a = 1; /*this statement should not follow the return statement*/
}

add(1,5);
