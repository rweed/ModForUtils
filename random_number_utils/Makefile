SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .F90 .f90

FC=ifort
FCFLAGS=-O2 -fPIC

OBJS=random_number_utils.o

random_number_utils.o: random_number_utils.f90 
	$(FC) -c $(FCFLAGS) ./random_number_utils.f90

clean:
	rm -rf ./*.o ./*.mod ./*~ ./*.x
