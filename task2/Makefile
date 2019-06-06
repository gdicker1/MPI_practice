# Makefile
#  Makefile for MPI_practice task1 - Create and parallelize DAXPY code
OBJ = task2.o task2_m.o
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
OPT = -O2 -qopenmp

$(PROG): $(OBJ)
	$(COMP) $(CHECKS) $(WARNS) $(OPT) -o $(PROG) $(OBJ) $(INCL) $(LIB) $(LIBS)

task2_m.mod: task2_m.f90 task2_m.o
	$(COMP) $(CHECKS) $(WARNS) $(OPT) -c task2_m.f90 $(INCL) $(LIB) $(LIBS) 

task2_m.o: task2_m.f90
	$(COMP) $(CHECKS) $(WARNS) $(OPT) -c task2_m.f90 $(INCL) $(LIB) $(LIBS) 

task2.o: task2.f90 task2_m.mod
	$(COMP) $(CHECKS) $(WARNS) $(OPT) -c task2.f90 $(INCL) $(LIB) $(LIBS) 

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod $(PROG)
