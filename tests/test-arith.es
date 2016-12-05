function int multiple(int a, int b)
{
  return a * b;
}

function int divide(int a, int b)
{
  return a / b;
}

function int mod(int a, int b)
{
  return a % b;
}

function float exp(float a, int b)
{
  return a ^ b;
}

function int main()
{
  int a;
  int b;
  int c;
  float d;
  a = multiple(2, 3);
  b = divide(9, 3);
  c = mod(10, 3);
  d = exp(3., 4);
  print(a);
  print(b);
  print(c);
  printfl(d);
  return 0;
}
