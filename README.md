[![Build Status](https://travis-ci.com/scivision/LAPACK95.svg?branch=master)](https://travis-ci.com/scivision/LAPACK95)

# LAPACK95
Archive / mirror of Netlib LAPACK95, with added cmake support.


## Build (makefiles)
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

## Build and install (cmake)
(Note: This compiles the full `single_double_complex_dcomplex` version.)

The standard procedure
```sh
mkdir build && cd build
cmake .. [-DCMAKE_INSTALL_PREFIX=/your/install/path (default: /usr/local)]
make -j$(nproc)
```
gives  `liblapack95.a`, with module files placed in `include/`. Your program can then `use f95_lapack` and be compiled with e.g.
```sh
gfortran yourprogram.f90 -I/path/to/LAPACK95/build/include -L/path/to/LAPACK95/build -llapack -llapack95
```
(assuming ordinary `LAPACK` is in your path).

Install with (still in the `build` folder)
```sh
make install
```
(may require `sudo` depending on install location). Programs can then be compiled with
```sh
gfortran yourprogram.f90 -I/your/install/prefix/include -llapack -llapack95
```
(assuming `/your/install/prefix/lib` is part of `LD_LIBRARY_PATH`.

## Use in a cmake project
This library can be used inside a cmake project by adding this repository as a subdirectory (e.g. with `git submodule add`) and then using a `CMakeLists.txt` along the lines of
```cmake
cmake_minimum_required(VERSION 3.0)

project(myproject Fortran)

find_package(LAPACK REQUIRED)

add_subdirectory(LAPACK95)

add_executable(myexe ${CMAKE_CURRENT_SOURCE_DIR}/myexe.f90)
target_link_libraries(myexe ${LAPACK_LIBRARIES} lapack95)
```
