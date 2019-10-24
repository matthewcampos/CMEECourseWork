#!/usr/bin/env python 3

""" Runs the stochastic Ricker model equation, taking into account fluctuations """

__author__ = "Matthew Campos (matthew.campos19@imperial.ac.uk)"
__version__ = '0.0.1'
__license__ = "License for this code/program"

import numpy as np
import time

def stochrick(p0 = np.random.uniform(0.5,1.5,size=10), r=1.2, K=1, sigma=0.2, numyears=5): #p0 generates array with random numbers
    """Runs the stochastic (with gaussian fluctuations) Ricker Eqn"""
    N = np.zeros((numyears, len(p0))) #creates matrix
    N[0,]=p0 #enters p0 values in the first row of matrix N
    for pop in range(0, len(p0)):
        for yr in range(1, numyears):
            N[yr,pop] = N[(yr-1),pop] * np.exp(r*(1-N[(yr-1),pop]/K) + np.random.normal(0,sigma,1)) #calculates stochastic Ricker equation by random sampling from a normal distribution
    return(N)

Stochrick_start = time.time()
stochrick()
Stochrick_end = time.time()
print("The time spent for function is:")
print(Stochrick_end - Stochrick_start)
