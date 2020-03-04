  #include <stdio.h>

/*int add_integers(int a, int b)    // Declaring the function
    {
        return a + b;
    }

int main (void)
    {
    	int a = 1;
    	int b = 1;
    	int result = 0;

    	result = add_integers(a, b);
    	printf("%i + %i = %i\n", a, b, result); // Something really complex

        return 0;
    }*/

/*void double_array_values(long double fparray[], long long intarray[], int nelems)
    {
        int i;

        if (fparray) {
            for (i = 0; i < nelems; ++i) {
    	    fparray[i] = fparray[i] * 2;
    	}
        }
        else if (intarray) {
    	for (i = 0; i < nelems; ++i) {
    	    intarray[i] = intarray[i] * 2;
    	}
        }
    }
int nelems = 4;
long long intarray[] = {81, 8, 4, 30};
long double fparray[] = {2.30, 10.1, 10.0, 81.8};

double_array_values(NULL, intarray, nelems);
double_array_values(fparray, NULL, nelems); */

#include <stdio.h>

char *site_names[] =	{"Parking lot",
			 "Cricket lawn",
			 "Manor house",
			 "Silwood Bottom",
			 "The Reactor",
			 "Japanese Garden",
			 };

void print_site_names(void)
{
    int i = 0;

    for (i = 0; i < 6; ++i) {
        printf("%s\n", site_names[i]);
    }
}

int main (void)
{
    print_site_names();

    return 0;
}
