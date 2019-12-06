#include <stdio.h>

int main (void)
{

  int a = 5;
  int b = 10;
  double c = 2.5;
  int d = 0;
  int e = 0;
  double f = 0;
  int g = 0;
  double pi = 3.14592654;

  d = a+b;
  e = (int) b-c;
  f = (double)b-c;
  g = (char) a+b;

  printf("The value of a+b: %i\n", d);
  printf("The value of int b-c: %i\n", e);
  printf("The value of f b-c: %f\n", f);
  printf("The value of c a+b: %c\n", g);
  printf("Pi to 5 digits: %0.5f\n", pi);

  return 0;

}
