# Welcome to the Modern Fortran Utilities 

## Description

This repository contains several utility programs either written from scratch or refactored to Modern Fortran. Currently the various directories hold modules that support a variety of tasks including computing binomial coefficients, factorials, routines for checking for NaN and Infinity floating point exceptions, root finders, random number utils and array sorters. Future releases will expand on these utilities and add interpolation routines and other utilities.

## Building the utilities

There is a Makefile in each directory that is specific to each utility. The default compiler is the Intel ifort compiler but another Fortran compiler can be selected by typeing (for example)

  cd root_finders
  make FC=gfortran

Currently each utility must be built manually and the required module and object files copied to the users directories.

A global Makefile that will build all the utilities into libraries will be added in a future release. Only the root_finder utilities have a test program that is built along with the utilities. 


## Contact

   Dr. Richard Weed
   Email: rweedmsu@gmail.com

