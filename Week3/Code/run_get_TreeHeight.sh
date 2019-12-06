#!/bin/sh
Rscript get_TreeHeight.R ../Data/trees.csv

python get_TreeHeight.py ../Data/trees.csv
