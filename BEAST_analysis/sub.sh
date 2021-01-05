#!/bin/bash

#SBATCH --job-name=seg8new_run1
#SBATCH --partition=batch
#SBATCH --constraint=EDR
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=5gb
#SBATCH --cpus-per-task=8
#SBATCH --time=167:00:00
#SBATCH --output=%x_%j.out
#SBATCH --error=%x_%j.err




cd $SLURM_SUBMIT_DIR


ml Beast/1.10.4-GCC-8.3.0

beast -threads 8 -beagle -beagle_SSE -overwrite seg8_emprical.xml
