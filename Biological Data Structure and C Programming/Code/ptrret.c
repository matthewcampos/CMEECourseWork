#include <stdlib.h>
#include <stdio.h>

int *pos_first_odd(const int*, const unsigned long);

int *pos_first_odd(const int *integers, const unsigned long size) //function that takes an array of integers and returns pointer to integer
{
    int* ret = NULL;
    int c = 0;
    // cast to remove const and can change value
    ret = (int*)integers; //array is an address so no need for &-> only for variables and * would convert it to an integers (for that address do this....)

    while ((*ret % 2)==0 && c < size) {
        ret = ret + 1; //think of like doing index+1 to see next value
        ++c;
    }

    if (c == size){
        --ret;
        if ((*ret % 2) == 0){
            return NULL;
        }
    }

    return ret; //* is an operator
}

int main (void)
{
    int *res = NULL; //pointer address 
    int intarray[] = {2,4,10,21,30};

    res = pos_first_odd(intarray, 5);

    printf("res now points to: %i\n", *res);

    *res = *res -1; //21-1 since post and prefix takes precedence

    res = pos_first_odd(intarray,5);
    if (res!=NULL){
        printf("res now points to %i\n", *res);
    }
}
