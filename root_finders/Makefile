SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .F90 .f90

FC=ifort
FCFLAGS=-O2 -fPIC

OBJS=rootFinderUtils.o \
 testRootFuns.o

EXE=testRootFinder.x

testRootFinder: $(EXE)

rootFinderUtils.o: rootFinderUtils.F90
	$(FC) -c $(FCFLAGS) ./rootFinderUtils.F90

testRootFuns.o: testRootFuns.f90
	$(FC) -c $(FCFLAGS) ./testRootFuns.f90

testRootFinder.x: $(OBJS) ./testRootFinder.f90
	rm -rf ./testRootFinder.x
	$(FC) -o ./testRootFinder.x $(FCFLAGS) ./testRootFinder.f90 $(OBJS)

clean:
	rm -rf ./*.o ./*.mod ./*~ ./*.x
