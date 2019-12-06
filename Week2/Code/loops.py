"""FOR loops in python"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

for i in range(5):
    print(i) #prints i five times

my_list=[0,2,"geronimo!",3.0,True,False]
for k in my_list:
    print(k) #prints each of the values in the array

total=0
summands=[0,1,11,111,1111]
for s in summands:
    total=total+s #reassigns the variable total by adding values in summands to it each loop
    print(total)

#WHILE loops in python
z=0
while z<100: #loops until condition is met
    z=z+1 #z increments each iteration
    print(z)

b=True
while b: #infinite loop as b does not change as condition always met
    print("Geronimo! Infinite loop! ctrl+c to stop!")
