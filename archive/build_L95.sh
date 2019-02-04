#!/bin/bash

# Lapack95 is included in Intel MKL, but MKL LAPACK95 needs to be compiled for GNU Fortran compiler

FC=gfortran

(

cd LAPACK95/

make clean -C SRC

# no -j due to Makefile syntax...
make double -C SRC FC=$FC
)
