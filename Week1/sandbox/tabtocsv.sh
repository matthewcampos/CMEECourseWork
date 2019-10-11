#!/bin/bash
# Author: Matthew Campos mlc19@ic.ac.uk
#Script: tabtocsv.sh
#Desc: substitute the tabs in the files with commas
#
#Arugments: 1-> tab delimited file
#Date: Oct 2019

echo "Creating a comma delimited version of $1 ..."
cat $1 | tr -s "\t" "," >> $1.csv
echo "Done!"
exit