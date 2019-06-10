
THEDIR=/glade/work/gdicker/MPI_practice/task1
FILE=$THEDIR/daxpy

module save D_PREV_MODS
module purge
module load ncarenv/1.2
module load pgi/17.10
module load ncarcompilers/0.4.1
module load netcdf/4.6.1

ulimit -s unlimited
module list

cd $THEDIR

make cleanall
make

echo "Running daxpy executable"
echo ""
for i in `seq 1 1 `;
do
  ./daxpy
done
echo ""

module restore D_PREV_MODS
