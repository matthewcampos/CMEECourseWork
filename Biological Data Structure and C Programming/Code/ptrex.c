#include <stdio.h>
#include <stdlib.h>

int main (void)
{
    int i = 0;
    int j = 1;
    int *p = NULL;
    int *q = NULL;

    p = &i;
    q = &j;

    printf("value of i before indirection: %i\n", i);
    printf("value of i via indirection: %i\n", *p); //data at p

    i = 4;
    *p = 5;

    printf("value of i after indirection: %i\n", i);

    printf("address of i using & operator: %p\n", &i);
    printf("address of i reading p: %p\n", p);
    printf("another way to read a pointer: %i\n", *(&i));

    printf("arithmetic via pointers: %i\n", *p + *q);

    return 0;

}
