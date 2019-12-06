#include <stdio.h>
int calculate_factorial(int n)
{
    int n_n1;

    if (n) { //checks if non-zero
        n_n1 = calculate_factorial(n-1); //counts down and stores values in stacks 
        printf("%i\n", n_n1);
	return n * n_n1;
    }

    return 1;
}

int main (void)
{
    int result,n=4;
    result = calculate_factorial(n=4);
    printf("%i\n", result);
    return 0;
}
