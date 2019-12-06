#include <stdio.h>

int count_primes_C(int limit)
{
    int prime = 1;
    int divisor = 2;
    int is_prime = 0;
    int n_primes = 0;

    while (prime < limit){
        is_prime = 0;

        for (divisor = 2; divisor <= prime / 2; ++divisor){ //once you exceed half it will not divide evenly
            if (prime % divisor == 0){ //prime starts out as 1 then 2 so less than divisor and ignores the condition and counts them as prime
                is_prime = 1;
                break;
            }

        }

        if (is_prime == 0){
            ++n_primes;
        }

        ++prime;
    }
    return n_primes;
}

int main (void)
{

    printf("Number of primes in 0-10: %i\n", count_primes_C(10));
    printf("Number of primes in 0-10: %i\n", count_primes_C(100));
    printf("Number of primes in 0-10: %i\n", count_primes_C(1000));
    printf("Number of primes in 0-10: %i\n", count_primes_C(10000));
    printf("Number of primes in 0-10: %i\n", count_primes_C(100000));
}
