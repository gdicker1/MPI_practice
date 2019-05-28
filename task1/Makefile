# mk_task1
#  Makefile for MPI_practice task1 - Create and parallelize DAXPY code
OBJ = task1.o task1_m.o
COMP = gfortran
PROG = daxpy
INCL = -I /usr/include -L /usr/lib -llapack -lblas

$(PROG): $(OBJ)
	$(COMP) -o $(PROG) $(OBJ) $(INCL)

task1_m.mod: task1_m.f90 task1_m.o
	$(COMP) -c task1_m.f90 $(INCL)

task1_m.o: task1_m.f90
	$(COMP) -c task1_m.f90 $(INCL)

task1.o: task1.f90 task1_m.mod
	$(COMP) -c task1.f90 $(INCL)

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod $(PROG)