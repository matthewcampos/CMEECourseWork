#include <stdio.h>

int main (void)
{
    int a = 7;
    int b = 2;
    int c = 0;

    c = a + 3;
    printf("a+3 = %i\n", c);
    printf("a+3 = %i\n", a+3);
    printf("a * b = %i\n", a*b);
    printf("PEMDAS %i\n", a*(b+3)/2);

    // Modulus operator %

    printf("the modulus of a and b: %i\n", a % b);

    return 0;
}
