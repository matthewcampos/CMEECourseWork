#include <stdio.h>
#include <limits.h>

int main (void)
{
    unsigned int a = 0;
    unsigned int b = 1;
    unsigned int c = 0;

    /*
      0:00000000
      1:00000001
      2:00000010
      3:00000011
      4:00000100
      5:00000101
      6:00000110
      7:00000111
    */

    // Set c to 2 using left shift 1:
    c = b << 1;

    printf("%i\n", c);

    // Set c to 5:
    c = 0;
    c = (b << 2) | b;

    printf("%i\n", c);

    // One's complement unary operator:
    c = 0;
    c =  ~c;

    printf("%u\n", c);
    printf("%u\n", UINT_MAX); //maximum size of unsigned int
}
