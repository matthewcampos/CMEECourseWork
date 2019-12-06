#include <stdio.h>

int main (void)
{
    int x = 6;
    int y = 0;

    //Postfix incrementaiton:
    y = ++x;
    printf("y is: %i\n", y);
    printf("x is: %i\n", x);

    //Prefix incrementation:
    y = x++;
    printf("y is: %i\n", y);
    printf("x is: %i\n", x);

    //Deincrement x
    int z = x--;
    printf("z will be: %i\n", z);
    printf("x will be: %i\n", x);  

    return 0;

}
