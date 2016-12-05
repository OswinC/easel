function int fib(int x, int z)
{
  if (x < 2)
    {return z = 1;}
  else 
  {return z = fib(x-1) + fib(x-2);}

}


int x;
x = 5;
print(fib(x));

