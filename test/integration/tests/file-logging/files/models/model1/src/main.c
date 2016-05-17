#include <stdio.h>

int main (void)
{
  char buf[6];

  fgets(buf, 6, stdin);

  fputs(buf, stdout);
  fputs(buf, stderr);
  return 0;
}
