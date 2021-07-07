SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .F90 .f90

FC=ifort
FCFLAGS=-O2 -fPIC

OBJS=factorial_utils.o

factorial_utils.o: factorial_utils.f90 
	$(FC) -c $(FCFLAGS) ./factorial_utils.f90

clean:
	rm -rf ./*.o ./*.mod ./*~ ./*.x
