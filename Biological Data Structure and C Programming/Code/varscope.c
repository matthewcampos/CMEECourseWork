#include <stdio.h>

void double_int(int i)
{
    i = 2 * i;

    return;
}

void add_one_to_all(int array[], int nelems)
{
    int i = 0;

    for (i = 0; i < nelems; ++i) {
        array[i] += 1;
    }

    return ;
}

int main (void)
{
    int i = 4;

    int array[] = {44,77,88,101,22};

    double_int(i);

    printf("Value of i after function call: %i\n", i);

    printf("array after add_one_to_all: \n");
    add_one_to_all(array,5);
    for (i=0; i<5; ++i){
        printf("%i",array[i]);
    }
    printf("\n");

    return 0;

}

int main(void)
{
    int i = 5;

    printf("i in scope of main(): %i\n", i);

    {
    	int i = 10;
	printf("i in local scope: %i\n", i);
    }

    if (i) {
        printf("This i is %i\n", i);
    }

    if (i) {
        int i = 50;
	printf("A new automatic i: %i\n", i);
    }

    return 0;
}
