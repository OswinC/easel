function int add(int x, int y)
{
    return x + y;
}

int a;
a = add(5,6.0); /* can't pass float argument to the function that accepts integer argument */
print(a);
