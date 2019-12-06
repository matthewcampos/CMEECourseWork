#!/usr/bin/env python 3

""" the two different methods for summing an array """

__author__ = "Matthew Campos (matthew.campos19@imperial.ac.uk)"
__version__ = '0.0.1'

import numpy as np #to generate array and utilise methods
import time

col=10
row=10
M = np.random.rand(col,row)
print(M)

def SumAllElements_loop(M):
    """function that loops through M and sums"""
    Tot = 0
    for i in range(M.shape[0]): #shape[0] takes the value of col
        for j in range(M.shape[1]): #shape[1] takes the value of row
            Tot = Tot + M[i][j]
    return Tot

def SumAllElements_vec(M):
    """function that utilises a package and uses a method"""
    Tot=np.sum(M)
    return Tot

#Bash output
Sum_loop_start = time.time()
SumAllElements_loop(M)
Sum_loop_end = time.time()
print("time spent for loop:")
print(Sum_loop_end - Sum_loop_start)

Sum_vec_start = time.time()
SumAllElements_vec(M)
Sum_vec_end = time.time()
print("time spent for vectorized function:")
print(Sum_vec_end - Sum_vec_start)
