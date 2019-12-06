#!/bin/bash
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
echo "R is about to run"
cp $HOME/mlc19_HPC_2019_main.R $TMPDIR
R --vanilla < $HOME/mlc19_HPC_2019_cluster.R
mv *_.rda /rds/general/user/mlc19/home
echo "R has finished running"
# this is a comment at the end of the file
