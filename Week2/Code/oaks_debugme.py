import csv
import sys
import re
import os
os.chdir("/Users/MatthewCampos/Documents/CMEECourseWork/Week2/Code")
import doctest

#Define function
def is_an_oak(name):
    # Question 2
    #print("The result is:", name.lower().startswith('quercus'))
    #return name.lower().startswith('quercus')

    """ Returns True if name it starts with 'quercus'

    >>> is_an_oak('Fagus sylvatica')
    False

    >>> is_an_oak('Quercus')
    True

    >>> is_an_oak('Quercuss')
    True

    >>> is_an_oak('Cwercus')
    False

    >>> is_an_oak('quercusss')
    False

    """
    #Question 4
    if re.match('^quer?cuss?$', name, flags=re.I) !=None:
        #re expressions sets the different posibilities of typos that will still match the condition
        #re.I makes it case insensitive
        #!= sets the conditional--> there is a value 
        return True
    else:
        return False

def main(argv):
    f = open('../Data/TestOaksData.csv','r')
    g = open('../Data/JustOaksData.csv','w')
    next(f)
    taxa = csv.reader(f)
    csvwrite = csv.writer(g)
    oaks = set()
    fieldnames = ['Genus', 'Species'] #creates the field names for the csv file
    csvwrite = csv.DictWriter(g,fieldnames=fieldnames) #writes the csv based on a dictionary
    csvwrite.writeheader()
    for row in taxa:
        print(row)
        print ("The genus is: ")
        print(row[0])
        if is_an_oak(row[0]+" "):
            print('FOUND AN OAK!')
            csvwrite.writerow({'Genus':row[0], 'Species':row[1]}) #assigns each index to the correct header

    return 0

if (__name__ == "__main__"):
    status = main(sys.argv)

doctest.testmod()
