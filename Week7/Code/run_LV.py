#!/usr/bin/env/python3
#run scripts in iPython
"""script to run the different LV functions from the practical"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import os

os.system("python -m cProfile LV1.py")
os.system("python -m cProfile LV2.py 1 0.1 1.5 0.75")
os.system("python -m cProfile LV3.py")
os.system("python -m cProfile LV4.py")
