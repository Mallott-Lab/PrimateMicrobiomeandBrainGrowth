#!/bin/bash

#SBATCH -J humann2_renorm
#SBATCH -A b1057
#SBATCH --mail-type=ALL
#SBATCH --mail-user=elizabeth.mallott@northwestern.edu
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mem=12G
#SBATCH -t 12:00:00
#SBATCH --output=/home/ekm9460/humann2_renorm.out
#SBATCH --error=/home/ekm9460/humann2_renorm.err
#SBATCH -p b1057

module purge all

module load singularity

singularity exec -B /projects/b1057 -B /projects/b1057/liz/mouse -B /home/ekm9460 -B /projects/b1057/humann2_ref_data /projects/b1057/biobakery_diamondv0822.sif humann2_renorm_table --input /projects/b1057/liz/mouse/shotgun/humann2/mouse_genefamilies.tsv --output /projects/b1057/liz/mouse/shotgun/humann2/mouse_genefamilies_cpm.tsv --units cpm

singularity exec -B /projects/b1057 -B /projects/b1057/liz/mouse -B /home/ekm9460 -B /projects/b1057/humann2_ref_data /projects/b1057/biobakery_diamondv0822.sif humann2_renorm_table --input /projects/b1057/liz/mouse/shotgun/humann2/mouse_pathabundance.tsv --output /projects/b1057/liz/mouse/shotgun/humann2/mouse_pathabundance_relab.tsv --units relab




