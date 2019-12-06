"""Practice with reading and writing csv file in Python"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import csv

# Read a file containing:
# 'Species','Infraorder','Family','Distribution','Body mass male (Kg)'
f = open('../Data/testcsv.csv','r')

csvread = csv.reader(f)
temp = []
for row in csvread:
    temp.append(tuple(row)) #adds to list per iteration
    print(row)
    print("The species is", row[0])

f.close()

# write a file containing only species name and Body mass
f = open('../Data/testcsv.csv','r') #'r' for read
g = open('../Data/bodymass.csv','w') #'w' for write

csvread = csv.reader(f)
csvwrite = csv.writer(g)
for row in csvread:
    print(row)
    csvwrite.writerow([row[0], row[4]])

f.close()
g.close()
