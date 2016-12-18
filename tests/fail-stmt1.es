function int add(int x, int y)
{
    int a;
    int a; /* duplicate local initialization */
    a = 5;
    return a + x + y;
}

add(1,5);
