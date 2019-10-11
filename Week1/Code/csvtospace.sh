#!/bin/bash
# Author: Matthew Campos matthew.campos19@imperial.ac.uk
# Script: csvtospace.sh
# Desc: script that takes comma separated values and converts it to space separated values
# saves output into a space separated file
# Arguments: 1-> comma separated value
# Date: Oct 2 2019

echo "Creating a space separated version of ../Data/$1.csv ..."
cat ../Data/$1.csv | tr "," " " > ../Data/$1_space.txt #cat is used to open the files from the Data directory and tr replaces the comma with spaces
echo "Done!"
exit
