"""opens a csv file and finds the best alignment and saves it in a separate file"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import csv

# Read a file containing:
f = open('../Data/sequences.csv','r')

csvread = csv.reader(f)

seq1=str(next(csvread)) #assigns the first align
seq2=str(next(csvread)) #assigns the second align

seq1="ATCGCCGGATTACGGG"
seq2="CAATTCGGAT"

# Assign the longer sequence s1, and the shorter to s2
# l1 is length of the longest, l2 that of the shortest

l1 = len(seq1)
l2 = len(seq2)
if l1 >= l2:
    s1 = seq1
    s2 = seq2
else:
    s1 = seq2
    s2 = seq1
    l1, l2 = l2, l1 # swap the two lengths

# A function that computes a score by returning the number of matches starting
# from arbitrary startpoint (chosen by user)
def calculate_score(s1, s2, l1, l2, startpoint):
    """computes the score of the different startpoint possibilities"""
    matched = "" # to hold string displaying alignements
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1:
            if s1[i + startpoint] == s2[i]: # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"

    # some formatted output
    print("." * startpoint + matched)
    print("." * startpoint + s2)
    print(s1)
    print(score)
    print(" ")

    return score

# Test the function with some example starting points:
#calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)

# now try to find the best match (highest score) for the two sequences
my_best_align = None
my_best_score = -1

for i in range(l1): # Note that you just take the last alignment with the highest score
    z = calculate_score(s1, s2, l1, l2, i)
    if z > my_best_score:
        my_best_align = "." * i + s2 # adding "." to show where alignment is starting
        my_best_score = z
print(my_best_align)
print(s1)
print("Best score:", my_best_score)

with open('../Results/best_sequence.txt', 'w') as f: #defines data path and makes a new text file called best_sequence
    f.write('Best sequence:\n {}\n {}\n {}\n'.format(my_best_align, s1, my_best_score)) #takes in multiple arguments
