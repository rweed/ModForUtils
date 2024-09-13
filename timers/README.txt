  timers.F90 contains several different procedures for timing Fortran codes.

  timer_gtd and timer_cgt are wrappers around the C gettimeofday and 
  clock_gettime routines.
  
  timer_cgt has an option for several different "clocks" defined as
  REALTIME, MONOTONE which should work on most POSIX systems as well
  as TAI, PROCESS_CPUTIME_ID, and THREAD_CPUTIME_ID clocks which are
  specific to Linux. See the test program and CLOCK_ parameters in 
  timers.F90 for usage
 
  timer_dt and timer_sc call the Fortran intrinsic DATE_AND_TIME and 
  SYSTEM_CLOCK routines.

  test_timers.F90 tests the different timer procedures in timers.F90.
  Each function is used to time a summation over 1.E8 iterations. Two
  get the Linux specific timers you must add -D__linux__ to your compiler
  options. test_timers.F90 also tests the Fortran intrinsic CPU_TIME which
  is called directly. 

  The test program defaults to compiling with -O0 optimization to give some
  realisitic time values. Compiling with higher optimization can lead to 
  some of the results returning 0 to 15 decimals places.

  These programs were compiled and tested on a Linux Mint 21.3 system running on  an AMD Ryzen 5 5600x processor with the following compilers


  Intel 
   ifx       2024.1.0
   ifort     2021.12.0

  gfortran

   gfortran  13.1

  Nvidia

   nvfortran 24.7.0

  AMD/AOCC

   flang     4.2


