"""Script to extract data from a tuple"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
        )

# Birds is a tuple of tuples of length three: latin name, common name, mass.
# write a (short) script to print these on a separate line or output block by species
# Hints: use the "print" command! You can use list comprehensions!
for i in birds:
    print("Species:", i[0]) # prints the first index of each row
    print("Common names:", i[1]) # prints the second index of each row
    print("Mean body mass:", i[2]) # prints the third index of each row
    print("-------------------") # creates a space between species so easy to differentiate each row
