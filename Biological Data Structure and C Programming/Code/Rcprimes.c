#include <R.h>
#include <Rdefines.h>

SEXP count_primes_C_wrap(SEXP limit)
{
    SEXP result;

    PROTECT(result = NEW_INTEGER(1)); //instructs R to leave this data alone

    int limit_c = 0;
    limit_c = *(INTEGER(limit)); // tells compiler to read the limit argument as a c-type integer

    UNPROTECT(1); //# of protect calls otherwise you get a stack imbalance

    return result;
}
