function int fib(int x, int z)
{
  if (x < 2)
    {return z = 1}
  else 
  {return z = fib(x-1) + fib(x-2)};

}

function int main()
{
  int x;
  x = 5;
  print(fib(x));
  return 0;
}

main();
return main();
/* Why return main? */