"""Save the elements of a list to a file"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

#############################
# FILE OUTPUT
#############################

list_to_save=range(100)

f=open('../Sandbox/testout.txt', 'w')
for i in list_to_save:
    f.write(str(i)+'\n') ##Add new line at the end

f.close()
