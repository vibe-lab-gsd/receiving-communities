#!/bin/bash
#SBATCH -J worthington # Job name
#SBATCH -p shared # Partition
#SBATCH -c 20 # Number of cores
#SBATCH -t 0-00:01:00 # Time (D-HH:MM:SS)
#SBATCH --mem=34G # Memory
#SBATCH -o worthington_%j.o # Name of standard output
file
#SBATCH -e worthington_%j.e # Name of standard error file
# load software environment
module load R/4.3.3-fasrc01
# print a statement
echo "Running Worthington test"
# execute python code
R worthington-test.R
