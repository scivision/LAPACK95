[![Build Status](https://travis-ci.com/scivision/LAPACK95.svg?branch=master)](https://travis-ci.com/scivision/LAPACK95)

# LAPACK95
CMAKE-enhanced mirror of Netlib LAPACK95.


## Build and install

the options `-Drealkind=` sets which precision to build (default `d`):

* `s`: float32
* `d`: float64
* `c`: complex32
* `z`: complex64

```sh
cd LAPACK95/build
cmake -Drealkind=d ../SRC
make -j
```
gives  `liblapack95.a`, with module files placed in `include/`. Your program can then `use f95_lapack` and be compiled with e.g.
```sh
gfortran yourprogram.f90 -I/path/to/LAPACK95/build/include -L/path/to/LAPACK95/build -llapack95 -llapack
```
(assuming ordinary `LAPACK` is in your path).

### Install
The default install location is under ~/.local on Unix-like systems, and can be specified with
```sh
cmake -DCMAKE_INSTALL_PREFIX=/your/install/path` ../SRC
```

```sh
make install
```

Programs can then be compiled with
```sh
gfortran yourprogram.f90 -I~/.local/include -llapack95 -llapack
```
(assuming `~/.local/lib` is in `LD_LIBRARY_PATH`).

## Use in a cmake project
This library can be used inside a cmake project by adding this repository with `add_subdirectory`. One can for example use [FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html):
```cmake
cmake_minimum_required(VERSION 3.11)

project(myproject Fortran)

include(FetchContent)
FetchContent_Declare(
    lapack95
    GIT_REPOSITORY https://github.com/scivision/LAPACK95.git
)

FetchContent_GetProperties(lapack95)
if(NOT lapack95_POPULATED)
    FetchContent_Populate(lapack95)
    add_subdirectory(${lapack95_SOURCE_DIR})
endif()

add_executable(myexe ${CMAKE_CURRENT_SOURCE_DIR}/myexe.f90)
target_link_libraries(myexe ${LAPACK_LIBRARIES} lapack95)

```

## Example program

```fortran
program ex
    ! Double precision
    use la_precision, only: wp => dp
    use f95_lapack, only: la_gesv

    real(wp) :: A(3,3), b(3)

    call random_number(A)
    b(:) = 3*A(:,1) + 2*A(:,2) - A(:,3)

    ! Solve Ax=b, overwrite b with solution
    call la_gesv(A,b)

    write(*,*) b
end program

! Output (exact: 3 2 -1):
! 2.9999999999999978        2.0000000000000018       -1.0000000000000004
```
