#!/bin/bash

#SBATCH -J humann2
#SBATCH -A b1057
#SBATCH --mail-type=ALL
#SBATCH --mail-user=elizabeth.mallott@northwestern.edu
#SBATCH -N 1
#SBATCH -n 8
#SBATCH -t 240:00:00
#SBATCH --output=/home/ekm9460/humann2_array_%A_%a.out
#SBATCH --error=/home/ekm9460/humann2_array_%A_%a.err
#SBATCH -p b1057
#SBATCH --array=1-52%8

module purge all

module load singularity

sleep $(echo "$RANDOM / 36000 * 60" | bc -l | xargs printf "%.0f")

echo "Starting Humann2 job"

file=$(ls /projects/b1057/liz/mouse/shotgun/kneaddata/*kneaddata.fastq | sed -n ${SLURM_ARRAY_TASK_ID}p)

singularity exec -B /projects/b1057 -B /projects/b1057/liz/mouse/shotgun -B /home/ekm9460 -B /projects/b1057/humann2_ref_data /projects/b1057/biobakery_diamondv0822.sif humann2 --input ${file} --output /projects/b1057/liz/mouse/shotgun/humann2/ --threads 8 --nucleotide-database /projects/b1057/humann2_ref_data/chocophlan --protein-database /projects/b1057/humann2_ref_data/uniref50

echo "Finishing Humann2 job"
