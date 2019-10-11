import sys

def open_fasta(x):
    """Imports fasta files"""
    with open(x, "r") as f:
        fasta=""
        counter=0
        for line in f:
            if counter!=0:
                fasta += line.replace("\n", "") #+= is string concatenation and adds strings in increments
            counter=counter+1 #removes first line from string
    return fasta

#seq1="ATCGCCGGATTACGGG"
#seq2="CAATTCGGAT"

# Assign the longer sequence s1, and the shorter to s2
# l1 is length of the longest, l2 that of the shortest

# A function that computes a score by returning the number of matches starting
# from arbitrary startpoint (chosen by user)

def set_variables(fasta1,fasta2):
        l1 = len(fasta1)
        l2 = len(fasta2)
        print("lengths: l1={} l2={}".format(l1, l2))
        if l1 >= l2:
            s1 = fasta1
            s2 = fasta2
        else:
            s1 = fasta2
            s2 = fasta1
            l1, l2 = l2, l1 # swap the two lengths aside from the sequences
        return s1,s2,l1,l2

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
# calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)

def my_best_align(s1, s2):
    s1, s2, l1, l2 = set_variables(s1, s2) #calls for set_variables function
    # now try to find the best match (highest score) for the two sequences
    best_align = None
    best_score = -1
    for i in range(l1): # Note that you just take the last alignment with the highest score
        z = calculate_score(s1, s2, l1, l2, i) #calls calculate_score function
        if z > best_score:
            best_align = "." * i + s2 # think about what this is doing!
            best_score = z
    print(best_align)
    print(s1)
    print("Best score:", best_score)
    return best_align, s1, best_score

def main(argv): #sets variables and runs the functions above to produce output
    """Based on imported fasta files, takes two of the files as arguents and produced best alignment with score"""
    if len(argv)>=3:
        fasta1 = open_fasta(argv[1])
        fasta2 = open_fasta(argv[2])
    else:
        print("Loading default files")
        fasta1 = open_fasta("../../Week1/Data/407228326.fasta")
        fasta2 = open_fasta("../../Week1/Data/407228412.fasta") #should load default files if no argument provided
    best_align, s1, best_score = my_best_align(fasta1, fasta2)
    with open('../Data/best_fasta_sequence.txt', 'w') as p: #defines data path and makes a new text file called best_sequence
        p.write('Best sequence:\n {}\n {}\n {}\n'.format(best_align, s1, best_score)) #takes in multiple arguments
    return 0

if (__name__ == "__main__"): #calls main(argv)
    status = main(sys.argv)
    sys.exit(status)
