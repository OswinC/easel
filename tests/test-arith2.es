function int add(int a)
{
  a++;
  return a;
}

function int sub(int a)
{
  a--;
  return a;
}


int a;
int b;

a = add(3);
b = sub(3);

print(a);
print(b);
