int gcd(int a, int b) {
  while (a != b) {
    if (a > b) a = a - b;
    else b = b - a;
  }
  return a;
}

int main()
{
  print(gcd(1,166));
  print(gcd(5,35));
  print(gcd(91,117));
  return 0;
}