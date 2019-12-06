##############################################################################
# loops vs. list comprehensions: which is faster?
##############################################################################
"""Comparing loops and lists looking at runtime"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

iters = 1000000

import timeit

from profileme import my_squares as my_squares_loops

from profileme2 import my_squares as my_squares_lc

# %timeit my_squares_loops(iters)
# %timeit my_squares_lc(iters)


##############################################################################
# loops vs. the join method for strings: which is faster?
##############################################################################
"""Compares loops and joins looking at runtime"""

mystring = "my string"

from profileme import my_join as my_join_join

from profileme2 import my_join as my_join

# %timeit(my_join_join(iters, mystring))
# %timeit(my_join(iters, mystring))
