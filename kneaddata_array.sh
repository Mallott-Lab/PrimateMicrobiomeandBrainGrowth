#!/bin/bash

#SBATCH -J kneaddata_${P1}
#SBATCH -A b1057
#SBATCH --mail-type=ALL
#SBATCH --mail-user=elizabeth.mallott@northwestern.edu
#SBATCH -N 1
#SBATCH -n 8
#SBATCH --mem=24G
#SBATCH -t 24:00:00
#SBATCH --output=/home/ekm9460/kneaddata_array_%A_%a.out
#SBATCH --error=/home/ekm9460/kneaddata_array_%A_%a.err
#SBATCH -p b1057
#SBATCH --array=1-3%3

module purge all

module load singularity

sleep $(echo "$RANDOM / 36000 * 60" | bc -l | xargs printf "%.0f")

echo "Starting Kneaddata job"

file=$(ls /projects/b1057/liz/mouse/shotgun/*.fastq.gz | sed -n ${SLURM_ARRAY_TASK_ID}p)

singularity exec -B /projects/b1057 -B /projects/b1057/liz/mouse/shotgun -B /home/ekm9460 -B /projects/b1057/liz -B /projects/b1057/humann2_ref_data /projects/b1057/biobakery_diamondv0822.sif kneaddata --input ${file} --threads 8  --reference-db /projects/b1057/humann2_ref_data/Homo_sapiens --output /projects/b1057/liz/mouse/shotgun/kneaddata

echo "Finishing Kneaddata job"
