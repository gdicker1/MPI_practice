#!/bin/bash -l
# LSF batch script to run an MPI application
#
#SBATCH --time=00:15:00
#SBATCH --job-name=MPI_P_T1
#SBATCH --account NTDD0002
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --partition=dav
#SBATCH --output=task1.out
#SBATCH --error=task1.err


THEDIR=/glade/work/gdicker/MPI_practice/task1
FILE=$THEDIR/daxpy

# Ensure that the correct modules can be found
source /glade/u/home/gdicker/scripts/module.sh

# Clean environment and then load modules as needed
module purge
module load PrgEnv/PGI+OpenMPI/2019-04-30
module load ncarenv/1.2
module load cuda/10.1.105.0
module load openmpi/3.1.4

ulimit -s unlimited
module list

cd $THEDIR

./daxpy

module restore D_PREV_MODS
