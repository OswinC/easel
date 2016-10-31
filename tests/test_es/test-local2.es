int fun(float a, int whatever)
{
  float copy;

  copy = a;

  return copy * copy;
}

int main() {
 print(fun(1.1, 101));
 return 0;
}