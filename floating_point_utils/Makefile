SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .F90 .f90

FC=ifort
FCFLAGS=-O2 -fPIC

OBJS=floating_point_utils.o

floating_point_utils.o: ./floating_point_utils.f90 
	$(FC) -c $(FCFLAGS) ./floating_point_utils.f90

clean:
	rm -rf ./*.o ./*.mod ./*~ ./*.x
