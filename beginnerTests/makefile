# Makefile for beginnerTests

# Compilers to be used
COMP_S = gfortran # serial compiler
COMP_P = mpif90 # parallel compiler
#ifdef CHEY
# COMP_P = ifort
#endif

LIB=/usr/lib64/gcc/x86_64-suse-linux/4.8/
INCL=$(MPI_INC)

# Executables created 
PROGS = area mpiQuery mpiHello mpiHelloB mpiHelloN

.PHONY: all clean cleanall cleanhello cleanallhello

all: $(PROGS)

area: area.f90 area.o
	$(COMP_S) -o area area.o

area.o: area.f90
	$(COMP_S) -c area.f90

mpiQuery: mpi_QueryStats.f90 mpi_QueryStats.o
	$(COMP_P) $(OPT_P) -o mpiQuery mpi_QueryStats.o

mpi_QueryStats.o: mpi_QueryStats.f90
	$(COMP_P) $(OPT_P) -c mpi_QueryStats.f90 

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
	rm -rf *.o 
#	rm -rf *.mod

cleanall:
	rm -rf $(PROGS)
	rm -rf *.o
#	rm -rf *.mod
	
