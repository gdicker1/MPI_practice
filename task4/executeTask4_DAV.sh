#!/bin/bash -l
# LSF batch script to run an MPI application
#
#SBATCH --time=00:15:00
#SBATCH --job-name=MPIPRAC
#SBATCH --account NTDD0002
#SBATCH --ntasks=72
#SBATCH --ntasks-per-node=36
#SBATCH --gres=gpu:v100:1
#SBATCH --partition=dav
#SBATCH --output=task4.out
#SBATCH --error=task4.err


THEDIR=/glade/work/gdicker/MPI_practice/task4
FILE=$THEDIR/daxpy

# Ensure that the correct modules can be found
source /glade/u/home/gdicker/scripts/module.sh

# Clean environment and then load modules as needed
module purge
module load PrgEnv/PGI+OpenMPI/2019-04-30
module load ncarenv/1.2
module load pgi/19.4
module load cuda/10.1.105.0
module load openmpi/3.1.4
module load pio/2_4_2
module list

cd $THEDIR
export OMPI_MCA_btl_openib_if_include=mlx5_0

mpirun -x PGI_ACC_TIME=0  -n 72 ./daxpy
