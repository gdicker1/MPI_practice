# Makefile for beginnerTests
COMP_S = gfortran # serial compiler
COMP_P = mpifort # parallel compiler
#OPT_P = 
PROGS = area mpiHello mpiHelloB mpiHelloN

.PHONY: all clean cleanall cleanhello cleanallhello

all: $(PROGS)

area: area.f90 area.o
	$(COMP_S) -o area area.o

area.o: area.f90
	$(COMP_S) -c area.f90

mpiHello: mpi_Hello.f90 mpi_Hello.o
	$(COMP_P) $(OPT_P) -o mpiHello mpi_Hello.o

mpi_Hello.o: mpi_Hello.f90
	$(COMP_P) $(OPT_P) -c mpi_Hello.f90

mpiHelloB: mpi_HelloBsend.f90 mpi_HelloBsend.o
	$(COMP_P) $(OPT_P)-o mpiHelloB mpi_HelloBsend.o

mpi_HelloBsend.o: mpi_HelloBsend.f90
	$(COMP_P) $(OPT_P)-c mpi_HelloBsend.f90

mpiHelloN: mpi_HelloNsend.f90 mpi_HelloNsend.o
	$(COMP_P) $(OPT_P) -o mpiHelloN mpi_HelloNsend.o

mpi_HelloNsend.o: mpi_HelloNsend.f90
	$(COMP_P) $(OPT_P) -c mpi_HelloNsend.f90

clean:
	rm *.o 
	rm *.mod

cleanall:
	rm $(PROGS)
	rm *.o
	rm *.mod
	