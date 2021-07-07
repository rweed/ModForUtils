SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .F90 .f90

FC=ifort
FCFLAGS=-O2 -fPIC

OBJS=quickSort.o \
 sortUtils.o

all: quickSort.o sortUtils.o

quickSort.o: quickSort.F90
	$(FC) -c $(FCFLAGS) ./quickSort.F90

sortUtils.o: sortUtils.F90
	$(FC) -c $(FCFLAGS) ./sortUtils.F90

clean:
	rm -rf ./*.o ./*.mod ./*~ ./*.x
