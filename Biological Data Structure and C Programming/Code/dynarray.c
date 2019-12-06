#include <stdlib.h>
#include <stdio.h>

int main (void)
{
    int numsites = 30;
    int *sppcounts = NULL;
    int *sppcounts2 = NULL;

    sppcounts = (int*)malloc(numsites * sizeof(int));

    sppcounts[20] = 44;

    int i = 0;
    for (i = 0; i < numsites; ++i){
        printf("data in site %i is: %i\n", i, *(sppcounts + i));
    }

    sppcounts2 = (int*)calloc(numsites, sizeof(int));

    sppcounts2[20] = 44;

    for (i = 0; i < numsites; ++i) {
        printf("data in site %i is: %i\n", i, sppcounts2); //void pointer points to a block of memory
    }
    }

    //Free memory, return it to the system before overwriting the pointer to that memory:
    free(sppcounts);

    sppcounts = (int*)calloc(numsites, sizeof(int));

    sppcounts[20] = 44;

    for (i=0; i < numsites; ++i){
        printf("data in site %i is: %i\n", i,sppcounts[i]);
    }

    return 0;
}
