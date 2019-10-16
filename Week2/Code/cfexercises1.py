#!/usr/bin/env python3

"""Understanding what foo_ function does"""
#docstrings are considered part of the running code (normal comments are
#stripped). Hence, you can access your docstrings at run time.
__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import sys

# What does each foo_x do?
def foo_1(x):
    """for every value of x, calculates x to the power of a half"""
    return x ** 0.5


def foo_2(x,y):
    """compares the value of x and y and returns the larger value"""
    if x>y:
        return x
    return y

def foo_3(x,y,z):
    """rearranges values with tmp as an intermediate variable and returns a set"""
    if x>y:
        tmp=y
        y=x
        x=tmp
    if y>z:
        tmp=z
        z=y
        y=tmp
    return [x,y,z]

def foo_4(x):
    """creates a range of from 1 to x+1 and calculates result * i, with a changing i and result each loop, replacing result each time"""
    result=1
    for i in range(1,x+1):
        result=result*i
    return result

# factorial

def foo_5(x):
    """recurisve function that calculates the factorial of x as it calculates the factorial by counting down until x=1"""
    if x==1:
        return 1
    return x*foo_5(x-1)

def foo_6(x):
    """calculate the factorial of x in a different way by using facto and a while loop, counting down x"""
    facto=1
    while x>=1:
        facto=facto*x
        x=x-1
    return facto

def main(argv):
    """Tries each Foo_ function with different arguments"""
    print(foo_1(12))
    print(foo_2(10,2))
    print(foo_3(10,2,5))
    print(foo_4(12))
    print(foo_5(4))
    print(foo_6(5))
    return 0

if (__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)
