#!/bin/bash
# LSF batch script to run an MPI application
#
#PBS -A NTDD0002
#PBS -l walltime=00:15:00
#PBS -l select=2:ncpus=36:mpiprocs=36
#PBS -N MPIPRAC
#PBS -o task2.out
#PBS -e task2.err
#PBS -q regular

THEDIR=/glade/work/gdicker/MPI_practice/task3
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
for i in `seq 1 1`;
do
   /usr/bin/time -f "\t%E real,\t%U user,\t%S sys" mpirun -n 36 ./daxpy
done
echo ""
#source /glade/u/home/gdicker/scripts/restorePaths.sh

module restore D_PREV_MODS
