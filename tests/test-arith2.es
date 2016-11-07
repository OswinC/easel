function int add(int a)
{
  return a++;
}

function int sub(int a)
{
  return a--;
}

function int div(int a)
{
  return a//;
}

function int mul(int a)
{
  return a**;
}

function int mod(int a)
{
  return a%%;
}

function int exp(int a)
{
  return a^^;
}

function int main()
{
  int a;
  int b;
  int c;
  int d;
  int e;
  int f;
  a = add(3);
  b = sub(3);
  c = div(3);
  d = mul(3);
  e = mod(3);
  f = exp(3);
  print(a);
  print(b);
  print(c);
  print(d);
  print(e);
  print(f);
  return 0;
}
