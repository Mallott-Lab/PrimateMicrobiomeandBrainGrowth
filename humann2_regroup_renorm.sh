#!/bin/bash

#SBATCH -J humann2_regrouprenorm
#SBATCH -A b1057
#SBATCH --mail-type=ALL
#SBATCH --mail-user=elizabeth.mallott@northwestern.edu
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mem=12G
#SBATCH -t 12:00:00
#SBATCH --output=/home/ekm9460/humann2_regrouprenorm.out
#SBATCH --error=/home/ekm9460/humann2_regrouprenorm.err
#SBATCH -p b1057

module purge all

module load singularity

singularity exec -B /projects/b1057 -B /projects/b1057/liz/mouse -B /home/ekm9460 -B /projects/b1057/humann2_ref_data /projects/b1057/biobakery_diamondv0822.sif humann2_regroup_table --input /projects/b1057/liz/mouse/shotgun/humann2/mouse_genefamilies.tsv --custom /home/ekm9460/utility_mapping/map_ko_uniref50.txt.gz  --output /projects/b1057/liz/mouse/shotgun/humann2/mouse_genefamilies_ko.tsv

singularity exec -B /projects/b1057 -B /projects/b1057/liz/mouse -B /home/ekm9460 -B /projects/b1057/humann2_ref_data /projects/b1057/biobakery_diamondv0822.sif humann2_renorm_table --input /projects/b1057/liz/mouse/shotgun/humann2/mouse_genefamilies_ko.tsv --output /projects/b1057/liz/mouse/shotgun/humann2/mouse_genefamilies_ko_cpm.tsv --units cpm

singularity exec -B /projects/b1057 -B /projects/b1057/liz/mouse -B /home/ekm9460 -B /projects/b1057/humann2_ref_data /projects/b1057/biobakery_diamondv0822.sif humann2_split_stratified_table --input /projects/b1057/liz/mouse/shotgun/humann2/mouse_genefamilies_ko_cpm.tsv --output /projects/b1057/liz/mouse/shotgun/humann2/
