SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .F90 .f90

FC=ifort
FCFLAGS=-O2 -fPIC

OBJS=binomial_coefficients.o

binomial_coefficients.o: binomial_coefficients.f90 
	$(FC) -c $(FCFLAGS) ./binomial_coefficients.f90

clean:
	rm -rf ./*.o ./*.mod ./*~ ./*.x
