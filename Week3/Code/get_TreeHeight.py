"""Python version of get_TreeHeight.R"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import sys
import pandas as pd
import numpy as np
import re

def open_file(path):
    """Takes input file and outputs the data using pandas"""
    data = pd.read_csv(path) #reads the csv file
    return(data)

def Tree_height(data):
    """calculates tree height of the input file"""
    degrees = data['Angle.degrees']
    distance = data['Distance.m']
    radians = np.deg2rad(degrees) #convert degrees to radians
    height = distance * np.tan(radians)
    data['height'] = height
    return data

def writefile(data, path):
    """formats the file name of the result file"""
    file = re.findall(r"/+([\w\d]+).csv", sys.argv[1]) #finds csv input files
    filename = "Py_" + file[0] + r"_treeheights.csv"
    path = "../Results/" + filename
    data.to_csv(path, index=False) #function that writes it as a csv

def main(argv):
    """takes input file and runs the script"""
    data = open_file(sys.argv[1])
    output = Tree_height(data)
    writefile(output, sys.argv[1])

if (__name__=="__main__"):
    status = main(sys.argv)
    sys.exit("Exit")
