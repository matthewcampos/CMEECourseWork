#!/usr/bin/env/python
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
