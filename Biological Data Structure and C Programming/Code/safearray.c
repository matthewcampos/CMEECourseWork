#include <stdlib.h>
#include <stdio.h>

struct intvec {
    size_t nelems; //integer type used with working and allocating memory in bitwise --> unsigned long int
    int* data;
};

typedef struct invect intvec_t; //creates an alias

intvec_t *new_intvec(size_t nelems)
{
    intvec_t* newvec = NULL;

    newvec = (intvec_t*)calloc(1, sizeof(intvec_t));

    if (newvec != NULL){
        //set 'virtual array' bounds
        newvec->nelems = nelems;
        //allocate the memory for the array
        newvec->data = (int*)calloc(nelems,sizeof(intvec_t))
        //checks that the data was fully allocated
        if (newvec->data == NULL){
            //if failed, cleans up resource and exits the function
            free(newvec);
            return NULL;
        }
    }

    return newvec;
}

void delete_intvec(intvec_t* ints)
{//always check that the pointer to memory being free is non-NULL
    if (ints != NULL){
        if (ints->data != NULL){
            free(ints->data);
            ints->data = NULL;
        }
        free(ints);
    }
}

//This function sets data in the intvec at a particular positionl returns 0 if success; and returns -1 if failed (out of bounds)

int set_data(int data, int index, intvec_t* ints)
{
    if (index >= ints->nelems){
        return;
    }

    ints->data[index] = data;
}

//this function gets the data from a particular index in the the intvec; returns 0 if sucess and -1 if failed
int get_data(int data, int index, intvec_t* ints)
{
    if (index < ints->nelems){
        *res = ints->data[index];
        return 0;
    }

    return -1;
}

int main (void)
{
    intvec_t *sppcounts = new_intvec(30);

    int i = 0;
    int val = 0;

    for (i = 0; i< sppcounts->nelems;++i){
        set_data(i + 3, i, sppcounts);
    }

    printf("All of the elements of sppcounts: \n" );
    for (i=0; i < sppcounts->nelems; ++i){
        get_data(&val,i,sppcounts);
        printf("%i", val);
    }
    printf("\n" );

    int error = 0;

    error = get_data(&val, 50, sppcounts);

    if (error != 0){
        printf("Error, tried to read out of bounds: %\n" );
    }

    return 0;
}
