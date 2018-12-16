[![Build Status](https://travis-ci.com/scivision/LAPACK95.svg?branch=master)](https://travis-ci.com/scivision/LAPACK95)

# LAPACK95
Archive / mirror of Netlib LAPACK95


## Build
To build 
[LAPACK95](http://www.netlib.org/lapack95/)
library with any Fortran compiler simply:
```sh
cd LAPACK95/
make clean -C SRC

make double -C SRC 
```
which creates `lapack95.a` in `LAPACK95/`  with "double" precision.
Use "single" or other options described in tne README files under LAPACK95 and LAPACK95/SRC if complex etc. precision is needed.
