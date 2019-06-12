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

#source /glade/u/home/gdicker/scripts/setPaths.sh

module save D_PREV_MODS

module purge
module load ncarenv/1.2
module load pgi/17.10
module load openmpi/3.1.2
module load ncarcompilers/0.4.1
module load netcdf-mpi/4.6.1
#module load mkl/2017.0.1

ulimit -s unlimited
module list

cd $THEDIR

make cleanall
make

#source /glade/u/home/gdicker/scripts/restorePaths.sh

echo "Running daxpy executable"
echo ""
for i in `seq 1 20`;
do
  ./daxpy
done
echo ""
#source /glade/u/home/gdicker/scripts/restorePaths.sh

module restore D_PREV_MODS
