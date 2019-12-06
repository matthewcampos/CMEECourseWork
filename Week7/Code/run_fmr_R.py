#!/usr/bin/env/python
"""Python workflow that runs the fmr.R file and informs the user if successful through terminal"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import subprocess

process = subprocess.Popen("Rscript fmr.R", stdout = subprocess.PIPE, stderr = subprocess.PIPE, shell=True)
stdout, stderr = process.communicate()

if stderr:
    print("Error")
    print(stderr.decode())
else:
    print("Success!")
    print(stdout.decode())
#subprocess.Popen("!/usr/bin/env Rscript --verbose fmr.R >../Results/fmr.Rout 2> ../Results/fmr_errFile.Rout",shell=True).wait()
