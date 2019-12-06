#include <R.h>
#include <Rdefines.h>
#include "countprime.h"

SEXP count_primes_C_wrap(SEXP limit)
{
    SEXP result;

    PROTECT(result = NEW_INTEGER(1)); //instructs R to leave this data alone

    int limit_c = 0;
    limit_c = *(INTEGER(limit)); // tells compiler to read the limit argument as a c-type integer

    int c_result = count_primes_C(limit_c);

    *(INTEGER(result)) = c_result;

    UNPROTECT(1); //# of protect calls otherwise you get a stack imbalance

    return result;
}

//opening in R
//setwd("This working directory")
//dyn.load("Rcprimes.so")
//.Call("count_primes_C_wrap", limit = as.integer(10))
