""" This is using_os.py"""

# Use the subprocess.os module to get a list of files and  directories
# in your ubuntu home directory

# Hint: look in subprocess.os and/or subprocess.os.path and/or
# subprocess.os.walk for helpful functions

import subprocess

#################################
#~Get a list of files and
#~directories in your home/ that start with an uppercase 'C'

# Type your code here:

# Get the user's home directory.
home = subprocess.os.path.expanduser("~")
# Create a list to store the results.
FilesDirsStartingWithC = []
# Use a for loop to walk through the home directory.
for (dir, subdir, files) in subprocess.os.walk(home):
    for d in dir:
        if d[0].startswith("C"):
            FilesDirsStartingWithC.append(d)
    for d in subdir:
        if d[0].startswith("C"):
            FilesDirsStartingWithC.append(d)
    for f in files:
        if f[0].startswith("C"):
            FilesDirsStartingWithC.append(f)

print("Files and Directories in home starting with C:")
print(set(FilesDirsStartingWithC))

#################################
# Get files and directories in your home/ that start with either an
# upper or lower case 'C'

# Type your code here:
FilesDirsStartingWithc = [] #list to store results
# Use a for loop to walk through the home directory.
for (dir, subdir, files) in subprocess.os.walk(home):
    for d in dir:
        if d[0].startswith("c"):
            FilesDirsStartingWithc.append(d)
    for d in subdir:
        if d[0].startswith("c"):
            FilesDirsStartingWithc.append(d)
    for f in files:
        if f[0].startswith("c"):
            FilesDirsStartingWithc.append(f)

Files_DirsStartingWithC_c = FilesDirsStartingWithC.join(FilesDirsStartingWithc)

print("Files and Directories in home starting with either C or c:")
print(set(FilesDirsStartingWithC_c))

#################################
# Get only directories in your home/ that start with either an upper or
#~lower case 'C'
# Type your code here:
DirsStartingWithC_c = []
# Use a for loop to walk through the home directory.
for (dir, subdir) in subprocess.os.walk(home):
    for i in dir:
        if i[0].startswith("C"):
            DirsStartingWithc.append(d)
    for i in subdir:
        if i[0].startswith("C"):
            DirsStartingWithc.append(d)
for (dir, subdir) in subprocess.os.walk(home):
    for f in dir:
        if f[0].startswith("c"):
            DirsStartingWithc.append(d)
    for f in subdir:
        if f[0].startswith("c"):
            DirsStartingWithc.append(d)

print("Directories in home starting with either C or c:")
print(set(DirsStartingWithC_c))
