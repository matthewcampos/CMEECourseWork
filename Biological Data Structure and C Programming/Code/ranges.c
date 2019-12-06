#include <stdio.h>

int main (void)
{
    int x = 0;
    char a = '\0';
    char b = 0;
    char c = '0';

    /* // '0', 0, and '\0' as a char
    printf("as char %c\n", a);
    printf("as char %c\n", b);
    printf("as char %c\n", c);

    printf("as int %i\n", a);
    printf("as int %i\n", b);
    printf("as int %i\n", c); */

    x = 255;
    a = x;
    x = a;
    printf("255 passed through char back to int %i\n", x);

    x = 256;
    a = x;
    x = a;
    printf("256 passed through char back to int %i\n", x);

    return 0;
}
