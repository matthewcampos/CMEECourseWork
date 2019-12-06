#include <stdlib.h>
#include <stdio.h>

int main (void)
{
    int integers[] = {2,33,4,10,11};
    int (*aintptr)[] = NULL; //pointer to an array
    int *aintptr2 = NULL; //point to an integers

    aintptr = &integers;

    printf("The value at index 1 in intarray via indirection: %i\n", (*aintptr)[1]);

    aintptr2 = integers;

    printf("dereferencing pointer to an array: %i\n", *aintptr2);

    printf("Get second value by pointer arithmetic: %i\n", *(aintptr2+1) );
    printf("Get second value by array subscripting: %i\n", aintptr2[1]);

    int *endofarray = NULL; //point to a specific value
    endofarray = &integers[4];//points to last element of array
    for (aintptr2 = integers;aintptr2 <= endofarray; ++aintptr2){
        printf("%i\n", *aintptr2);
    }
    printf("\n");

    return 0;
}
