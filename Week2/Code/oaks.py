"""Using loops to identify certain taxa that are oak trees from list"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'


## Finds just those taxa that are oak trees from a list of species
taxa = [ 'Quercus robur',
         'Fraxinus excelsior',
         'Pinus sylvestris',
         'Quercus cerris',
         'Quercus petraea',
       ]

def is_an_oak(x):
    """function that identifies taxa that are oak trees"""
    return x.lower().startswith('quercus')

##Using loops
oak_loops=set() #creates a set
for species in taxa: #loops through taxa array
    if is_an_oak(species):
        oak_loops.add(species) #if condition met, it is added to set
print(oak_loops)

##Using list comprehensions
oaks_lc=set([species for species in taxa if is_an_oak(species)])
print(oaks_lc)

##Get names in UPPER CASE using for loops
oak_loops=set()
for species in taxa:
    if is_an_oak(species):
        oak_loops.add(species.upper())
print(oak_loops)

##Get names in UPPER CASE using list comprehensions
oaks_lc=set([species.upper() for species in taxa if is_an_oak(species)])
print(oaks_lc)
