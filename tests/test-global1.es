int a;
int b;

function void Add()
{
  print(a + 1);
}


function void AddAgain()
{
  a = a + 1;
}

  a = 100;
  print(a);
  Add();
  AddAgain();
