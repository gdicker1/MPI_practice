# Makefile
#  Makefile for MPI_practice task1 - Create and parallelize DAXPY code
OBJ = task1.o task1_m.o
COMP = mpif90
PROG = daxpy
INCL =
#INCL = -I/usr/include
LIB =
#LIB = -L/glade/work/gdicker/opt/lib
LIBS = -llapack -lblas
OPT = -fpic -03 -pgf90libs -Mcache_align -Mconcur -Minline

$(PROG): $(OBJ)
	$(COMP) $(OPT) -o $(PROG) $(OBJ) $(INCL) $(LIB) $(LIBS)

task1_m.o: task1_m.f90
	$(COMP) $(OPT) -c task1_m.f90 $(INCL) $(LIB) $(LIBS)

task1.o: task1.f90 task1_m.o
	$(COMP) $(OPT) -c task1.f90 $(INCL) $(LIB) $(LIBS)

clean:
	rm -f *.o *.mod

cleanall:
	rm -f *.o *.mod $(PROG)
