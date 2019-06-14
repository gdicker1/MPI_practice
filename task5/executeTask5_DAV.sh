#!/bin/bash -l

#
#SBATCH --time=00:15:00
#SBATCH --job-name=MPIPRAC
#SBATCH --account NTDD0002
#SBATCH -N 1
#SBATCH --ntasks-per-node=1
#SBATCH --gres=gpu:v100:1
#SBATCH --partition=dav
#SBATCH --output=task5.out
#SBATCH --error=task5.err

THEDIR=/glade/work/gdicker/MPI_practice/task5

# Ensure that the correct modules can be found
source /glade/u/home/gdicker/scripts/module.sh

# Clean environment and then load modules as needed
module purge
module load PrgEnv/PGI+OpenMPI/2019-04-30
module load ncarenv/1.2
module load cuda/10.1.105.0
module load openmpi/3.1.4

# Move to directory and launch the job
cd $THEDIR
mpirun -x PGI_ACC_TIME=1  -n 72 ./daxpy

