#include <stdio.h>

int main (void)
{
    int i = 3, element = 0;
    int myarray[5] = {1,2,3,4,5};
    element = myarray[i] * myarray[i];
    printf("The value at index %i in my array: %i\n", i, element);

    return 0;
}

int main (void)
{

    int array1[] = {1, 2, 3, 4, 5, 6};
    int array2[] = {7, 8, 9};
    int array3[10];

    array3[0] = array1[0];
    array3[1] = array1[1];
    array3[2] = array1[2];
    array3[3] = array1[3];
    array3[4] = array1[4];
    array3[5] = array1[5]; // Notice that we stop at 5 on array 1
    array3[6] = array2[0]; // And we re-begin at 0 on array 2
    array3[7] = array2[1];
    array3[8] = array2[2];

    for (i=0;i<10;i++){
        printf("%i\n", array3[i]);
    }
}

int main (void)
{
    int i = 1;
    int myarray[10];
    for (i;i<10;i++){
        myarray[i] = i;
        printf("element i is:%i\n", i);
    }

}

int main (void)
{
    char array1[] = "The quick brown fox";
    char array2[] = "jumped over the lazy dog";
    printf(array1,array2);
}
