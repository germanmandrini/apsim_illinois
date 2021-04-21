#!/bin/bash
module load singularity

cat /projects/aces/germanm2/n_policy_git/id_10_walltime.txt | while read i #each line has the cell and the estimated time to be runed

do
	cell_n="$(cut -d' ' -f1 <<<"$i")" #split the cell string
	walltime_n="$(cut -d' ' -f2 <<<"$i")" #split the time string
	echo $cell_n $walltime_n $bash_n
	#qsub -v cell_n=$cell_n -l walltime=00:$walltime_n:00 -N $cell_n ./B_bash.sh #send the cell_n as argument to the script and the walltime in the command line
	sbatch --job-name=$cell_n --export=ALL,cell_n=$cell_n ./B_bash.sh
done
