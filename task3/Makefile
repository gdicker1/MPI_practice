# Makefile
#  Makefile for MPI_practice task1 - Create and parallelize DAXPY code
OBJ = task3.o task3_m.o
COMP = mpif90
PROG = daxpy
INCL = 
INCL = -I/glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/mkl/include/
LIB =
LIB = -L/glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/mkl/lib/intel64
LIBS = -lmkl_blacs_intelmpi_ilp64 -lmkl_scalapack_ilp64 -lmkl_intel_ilp64 \
       -lmkl_intel_thread -lmkl_core -lmkl_blacs_intelmpi_ilp64 -liomp5 -lpthread -lm -ldl
WARNS = 
CHECKS =
OPT = 
OPT = -O3

$(PROG): $(OBJ)
	$(COMP) $(CHECKS) $(WARNS) $(OPT) -o $(PROG) $(OBJ) $(INCL) $(LIB) $(LIBS)

task3_m.mod: task3_m.f90 task3_m.o
	$(COMP) $(CHECKS) $(WARNS) $(OPT) -c task3_m.f90 $(INCL) $(LIB) $(LIBS) 

task3_m.o: task3_m.f90
	$(COMP) $(CHECKS) $(WARNS) $(OPT) -c task3_m.f90 $(INCL) $(LIB) $(LIBS) 

task3.o: task3.f90 task3_m.mod
	$(COMP) $(CHECKS) $(WARNS) $(OPT) -c task3.f90 $(INCL) $(LIB) $(LIBS) 

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod $(PROG)
