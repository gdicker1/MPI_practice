# Makefile
#  Makefile for MPI_practice task1 - Create and parallelize DAXPY code

#Test for location
ifeq ($(D_LOC),)
ifeq ($(NCAR_HOST),)
echo "Error, can't determine location"
endif
D_LOC=$(NCAR_HOST)
endif

OBJ = task4.o task4_m.o openacc_m.o
COMP = mpif90
PROG = daxpy
INCL =
#INCL = -Iglade/work/gdicker/opt/pgi/linux86-64-llvm/2019/include
#INCL = -I/glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/mkl/include/
LIB =
#LIB = -L/glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/mkl/lib/intel64
#LIB = -L/glade/work/gdicker/opt/lib
LIBS = -lmkl_blacs_intelmpi_ilp64 -lmkl_scalapack_ilp64 -lmkl_intel_ilp64 \
       -lmkl_intel_thread -lmkl_core -lmkl_blacs_intelmpi_ilp64 -liomp5 -lpthread -lm -ldl
LIBS = -llapack -lblas
WARNS = 
CHECKS =
INFO = -Minfo=accel -Mneginfo
OPT = 
OPT = -fpic -O3 -acc -ta=tesla:cc70  -Mpreprocess -pgf90libs -Mcache_align -Mconcur -Minline

$(PROG): $(OBJ)
	$(COMP) $(CHECKS) $(WARNS) $(INFO) $(OPT) -o $(PROG) $(OBJ) $(INCL) $(LIB) $(LIBS)

task4_m.o: task4_m.f90
	$(COMP) $(CHECKS) $(WARNS) $(INFO) $(OPT) -c task4_m.f90 $(INCL) $(LIB) $(LIBS) 

task4.o: task4.f90 task4_m.o
	$(COMP) $(CHECKS) $(WARNS) $(INFO) $(OPT) -c task4.f90 $(INCL) $(LIB) $(LIBS) 

openacc_m.o: openacc_m.f90
	$(COMP) $(CHECKS) $(WARNS) $(INFO) $(OPT) -c openacc_m.f90 $(INCL) $(LIB) $(LIBS)

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod $(PROG)

