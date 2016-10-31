int a;
int b;

void Add()
{
  print(a + 1);
}


void AddAgain()
{
  a = a + 1;
}

int main()
{
  a = 100;
  print(a);
  Add();
  AddAgain();
  return 0;
}
