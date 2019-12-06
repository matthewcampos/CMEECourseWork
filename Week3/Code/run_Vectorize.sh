#!/bin/sh
Rscript Vectorize1.R ../Data/trees.csv

python Vectorize1.py ../Data/trees.csv

Rscript Vectorize2.R ../Data/trees.csv

python Vectorize2.py ../Data/trees.csv
