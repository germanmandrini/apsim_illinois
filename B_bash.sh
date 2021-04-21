#!/bin/bash
#SBATCH -t 5
#SBATCH -p secondary
#SBATCH -n 28

cd /projects/aces
## cd /home/germanm2/scratch
module load singularity ## Load the singularity runtime to your environment

# cell_n=$1 #split the cell string for bash work

bash_n=151

## Code to get the cell_n from qsub or from bash
## if [ -z ${cell+x} ]; then echo "var is unset"; else echo "var is set to '$cell'"; fi
## if [ -z ${cell+x} ]; then cell_n=$1; cell_n=${cell_n};fi

echo "$cell_n"

singularity exec /projects/aces/germanm2/apsim_nov16.simg Rscript /projects/aces/germanm2/n_policy_git/Codes/simA_manager.R $cell_n $bash_n


