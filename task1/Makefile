# Makefile
#  Makefile for MPI_practice task1 - Create and parallelize DAXPY code
OBJ = task1.o task1_m.o
COMP = pgf90
PROG = daxpy
INCL =
#INCL = -I/usr/include
LIB =
#LIB = -L/glade/work/gdicker/opt/lib
LIBS = -llapack -lblas

$(PROG): $(OBJ)
	$(COMP) -o $(PROG) $(OBJ) $(INCL) $(LIB) $(LIBS)

task1_m.mod: task1_m.f90 task1_m.o
	$(COMP) -c task1_m.f90 $(INCL) $(LIB) $(LIBS)

task1_m.o: task1_m.f90
	$(COMP) -c task1_m.f90 $(INCL) $(LIB) $(LIBS)

task1.o: task1.f90 task1_m.mod
	$(COMP) -c task1.f90 $(INCL) $(LIB) $(LIBS)

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod $(PROG)
