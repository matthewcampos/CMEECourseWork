#!/usr/bin/env python3

"""understanding how sysargv works"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'


import sys

print("This is the name of the script", sys.argv[0])
print("The number of arguments", len(sys.argv))
print("The arguments are", str(sys.argv))
