SHELL= /bin/sh

.SUFFIXES:
.SUFFIXES: .o .F90


FC=ifort

EXE= t_findloc_no08.x \
 t_findloc_int.x 

all: $(EXE)

t_findloc_no08.x: testfindloc.F90 findlocUtils.o
	$(FC) -o t_findloc_n08.x -DNO_F2008_FINDLOC -O2 testfindloc.F90 findlocUtils.o 
t_findloc_int.x: testfindloc.F90
	$(FC) -o t_findloc_int.x -O2 testfindloc.F90

findlocUtils.o: findlocUtils.F90
	$(FC) -c -O2 findlocUtils.F90

clean:
	rm -rf *.o *~ *.x *.mod ./core ./core.*
