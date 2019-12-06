#include <stdio.h>

void print_int_array(const int array[], const int nelems)
{
    int i = 0;
    for (i = 0; i < nelems; ++i){
        printf("%i", array[i]);
        if (i < (nelems-1)) {
            printf(", ");
        }
    }
    printf("\n");
}

int main (void)
{
    int intarray[] = {8,6,4,2,101,27};

    print_int_array(intarray,6);
}
