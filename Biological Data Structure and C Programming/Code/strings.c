#include <stdio.h>

int main (void)
{
    char carray[] = {'a', ' ', 's', 't', 'r', 'i', 'n', 'g', '!', '\0'};
    char string1[] = "A string!";

    printf("The 9th element of carray: %c\n", carray[9]);
    printf("The 9th element of the string %c\n", string1[9]);
}
