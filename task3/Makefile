# Makefile
#  Makefile for MPI_practice task1 - Create and parallelize DAXPY code
OBJ = task3.o task3_m.o
COMP = mpif90
PROG = daxpy
INCL = -I$(D_OPT_MPI_INC) -I$(D_OPT_PGI_INC)
LIB = -L/glade/work/gdicker/opt/lib
LIBS = -llapack -lblas
WARNS = -Wall
CHECKS = -fcheck=all

$(PROG): $(OBJ)
	$(COMP) $(CHECKS) $(WARNS) -o $(PROG) $(OBJ) $(INCL) $(LIB) $(LIBS)

task2_m.mod: task2_m.f90 task2_m.o
	$(COMP) $(CHECKS) $(WARNS) -c task3_m.f90 $(INCL) $(LIB) $(LIBS) 

task2_m.o: task2_m.f90
	$(COMP) $(CHECKS) $(WARNS) -c task3_m.f90 $(INCL) $(LIB) $(LIBS) 

task2.o: task2.f90 task2_m.mod
	$(COMP) $(CHECKS) $(WARNS) -c task3.f90 $(INCL) $(LIB) $(LIBS) 

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod $(PROG)
