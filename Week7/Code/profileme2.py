"""Changed functions to include list comprehensions and see how run time is faster with profiling"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

def my_squares(iters):
    """for range of iters, value of i is raised to the power of 2 each iteration"""
    out = [i ** 2 for i in range(iters)]
    return out

def my_join(iters, string):
    """Repeats string input, iters many times with a comma separating them"""
    out = ''
    for i in range(iters):
        out += ", " + string
    return out

def run_my_funcs(x,y):
    """runs both functions showing input and output"""
    print(x,y)
    my_squares(x)
    my_join(x,y)
    return 0

run_my_funcs(10000000,"My string")
