#include "example_lib1.h"
#include "example_lib2.h"
#include "libs/user_lib.h"
#include "libs/extralib/extra_lib.h"

int main (void)
{
  userLibFunc();
  extraLibFunc();
  exampleLib1Func();
  exampleLib2Func();
  return 0;
}
