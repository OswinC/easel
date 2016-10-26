int x;
int y;
/*/*/* one-line variable definition like int x = 3 is not supported yet */*/*/
x = 3;

function void foo() {}

function int bar(int a, bool b, int c) { return a + c; }

function int main()
{
  print(bar(17, false, 25));
  return 0;
}

return main();
