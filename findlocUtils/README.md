# Fortran 2008 findloc workalikes for compilers that don't support F2008

## Description

The findloc utilities in findlocUtils.F90 are designed to provide workalike
functions for Fortran compilers that haven't implemented the Fortran 2008 
FINDLOC function. Currently 1D, 2D, and 3D arrays of INT32, INT64, REAL32,
REAL64 and LOGICAL types are supported. The functions provide support for the
MASK, BACK, and DIM optional arguments. The test program testfindloc.F90
illustrates how the functions are accessed and used. In particular, two
access the findloc replacements the user should add:

 USE findlocUtils, ONLY: FINDLOC=>findloc_no08

to there codes prior to using the findloc utitlities. findloc_no08 is a
generic interface that overloads the different individual findloc utilities

To compile the code with the default Intel compiler, the user should type

 make

which will build the findlocUtils module and two test programs,

t_findloc_n08.x and t_findloc_int.x. t_findloc_n08.x uses the findlocUtils
versions of findloc. t_findloc_int.x will use the F2008 intrinsic versions
of findloc if they are supported.

To build the code with a different Fortran compiler the user should type

make FC=gfortran

or

make FC=nvfortran

The default optimization level is -O2.

## Contact Info

All questions or bug reports should be sent via email to:

rweedmsu@gmail.com 
