function int fib(int x)
{
  if (x < 3)
      return 1;
  else
      return fib(x-1) + fib(x-2); 
}

int x = 5;
print(fib(x));

