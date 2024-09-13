SHELL=/bin/sh

.SUFFIXES:
.SUFFIXES: .o .F90 .f90

FC=ifx
FCFLAGS=-O2 -march=native
FXFLAGS=-O0 -march=native

# For Linux OS add -D__linux__ to FCFLAGS and FXFLAGS

OBJS=timers.o

EXE=test_timers.x

testTimers: $(EXE)

timers.o: timers.F90
	$(FC) -c $(FCFLAGS) ./timers.F90

test_timers.x: $(OBJS) ./test_timers.F90
	rm -rf ./test_timers.x
	$(FC) -o ./test_timers.x $(FXFLAGS) ./test_timers.F90 $(OBJS)

clean:
	rm -rf ./*.o ./*.mod ./*~ ./*.x
