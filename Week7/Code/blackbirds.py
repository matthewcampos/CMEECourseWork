"""Uses regex to print certain information from the DataFrame"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import re
import pandas as pd

# Read the file (using a different, more python 3 way, just for fun!)
with open('../Data/blackbirds.txt', 'r') as f:
    text = f.read()

# replace \t's and \n's with a spaces:
text = text.replace('\t',' ') #removes tabs
text = text.replace('\n',' ') #removes new lines and just spaces out text

# You may want to make other changes to the text.

# In particular, note that there are "strange characters" (these are accents and
# non-ascii symbols) because we don't care for them, first transform to ASCII:
text = text.encode('ascii', 'ignore') # first encode into ascii bytes
text = text.decode('ascii', 'ignore') # Now decode back to string

# Now extend this script so that it captures the Kingdom, Phylum and Species
# name for each species and prints it out to screen neatly.

my_Kingdom = re.findall(r"(?<=Kingdom)\s+[A-Z]?\w+", text) #?<= means don't include Kingdom, look for one or more space followed by a capital character then one or more alphanumeric characters afterwards
print(my_Kingdom)
print(my_Kingdom[0])
my_Phylum = re.findall(r"(?<=Phylum)\s+[A-Z]?\w+", text) #don't include Phylum, look for one or more space followed by a capital character then one or more alphanumeric characters afterwards
print(my_Phylum)
my_Species = re.findall(r"(?<=Species)\s+[A-Z]?\w+\s+\w+", text) #don't include Species, look for one or more space followed by a capital character then alphanumeric characters afterwards one or more times, followed by space and more characters
print(my_Species)

#create data frame to display data
df = pd.DataFrame(index = range(len(my_Kingdom)),columns = ['Kingdom', 'Phylum','Species'])
print(df)
#add elements columnwise
for i in range(len(my_Kingdom)):
    df.iloc[i][0] = my_Kingdom[i]
    df.iloc[i][1] = my_Phylum[i]
    df.iloc[i][2] = my_Species[i]

print(df)

# Hint: you may want to use re.findall(my_reg, text)... Keep in mind that there
# are multiple ways to skin this cat! Your solution could involve multiple
# regular expression calls (easier!), or a single one (harder!)
