#!/usr/bin/env python3

"""Understanding control flow using if, for and while statements"""
#docstrings are considered part of the running code (normal comments are
#stripped). Hence, you can access your docstrings at run time.
__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import sys

def foo_1(j=12): #j takes on this value if nothing entered
    """function to identify range of values divisible by three"""
    for j in range(0,j):
        if j%3==0:
            print("hello")
"---------------------------------------------------------------------------"
def foo_2(j=18): #value if nothing entered
    """function to identify range of values yielding a remainder of three"""
    for j in range(0,j):
        if j%5==3:
            print("Hello")
        elif j%4==3:
            print("hello")
"---------------------------------------------------------------------------"
def foo_3(z=0):
    """function to print hello if z is less than 15"""
    while z!=15:
        print('hello')
        z=z+3
"---------------------------------------------------------------------------"
def foo_4(z=12):
    """function to print hello if z given two conditions are met"""
    while z<100:
        if z==31:
            for k in range(7):
                print('hello')
        elif z==18:
            print('hello')
        z=z+1

def main(argv):
    """tests the different function using main arguments"""
    print(foo_1(12))
    print(foo_2(18))
    print(foo_3(0))
    print(foo_4(12))
    return 0

if (__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)
