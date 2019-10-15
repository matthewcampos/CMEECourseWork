
# Read a file containing:
#f = open('../data/sequences.csv','r')

#csvread = csv.reader(f)

#seq1=str(next(csvread)) #assigns the first align
#seq2=str(next(csvread)) #assigns the second align

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
    if z >= my_best_score:
        my_best_align = "." * i + s2 # think about what this is doing!
        my_best_score=z

offset=[] # i determines the starting point location of s2 for each best alignment
for i in range(l1): # Note that you just take the last alignment with the highest score
    z = calculate_score(s1, s2, l1, l2, i)
    if z == my_best_score: #checking that z equals the highest from the previous function in order to remove values lower than the highest
        offset.append((i,z))
print(offset)

with open('../Results/better_sequence.txt', 'w') as f: #defines data path and makes a new text file called better_sequence
    for construct in offset:
        f.write('Best sequence:\n {}\n {}\n {}\n'.format("."*construct[0]+s2, s1, my_best_score)) #takes in multiple argument and "."*construct[0]+s2 prints the alignment
