function int fib(int x){
  int z;
  if (x < 2) return 1;
  else
  z = fib(x-1) + fib(x-2);
  return z;

}


int x;
x = 5;

print(fib(x));

