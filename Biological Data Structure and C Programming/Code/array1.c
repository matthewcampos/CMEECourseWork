#include <stdio.h>

int main (void)
{
    int i = 0; // Interpretation: reserve memory with a  read/write size of an int
    int j = 0;
    char c = 'c'; // Reserve memory with a read/write size of a character
    double pi = 3.14; // Reserve memory with a read/write size of double

    int intarray[4] ; // Explicit size declaration
    int intarray2[] = {0, 0, 1, 4}; // Set size through values

    int matrix[2][4]; // Matrices can be specified
    int nmatrix[2][4][3]; // Matrices can be n-dimensional

    // Reading and writing from/to arrays:

    // Example: read from an uninitialised array:
    // Reading/writing in C is zero-based:

    j = intarray[0];
    printf("intarray at position 0: %i\n", j);
    printf("intarray at position 1: %i\n", intarray[1]);
    printf("intarray at position 2: %i\n", intarray[2]);
    printf("intarray at position 3: %i\n", intarray[3]);

    printf("intarray2 at position 0: %i\n", intarray2[0]);
    printf("intarray2 at position 1: %i\n", intarray2[1]);
    printf("intarray2 at position 2: %i\n", intarray2[2]);
    printf("intarray2 at position 3: %i\n", intarray2[3]);

    intarray2[0] = 3;
    intarray2[1] =2;

    printf("After assignment:\n" );
    printf("intarray2 at position 0: %i\n", intarray2[0]);
    printf("intarray2 at position 1: %i\n", intarray2[1]);
    printf("intarray2 at position 2: %i\n", intarray2[2]);
    printf("intarray2 at position 3: %i\n", intarray2[3]);

    printf("Reading out of intarray bounds: %i\n", intarray[4]);


    return 0;
}
