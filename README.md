# LAPACK95

[![DOI](https://zenodo.org/badge/162024736.svg)](https://zenodo.org/badge/latestdoi/162024736)
![ci_linux](https://github.com/scivision/lapack95/workflows/ci_linux/badge.svg)

CMake and Meson enhanced mirror of Netlib LAPACK95.
Easy to build and include in most projects and operating system.

## Build

the options `-Darith=` sets which precision to build (default `d`):

* `s`: float32
* `d`: float64
* `c`: complex32
* `z`: complex64

Build with CMake and a Fortran compiler.
The build yields:

* `LAPACK95/liblapack95.a`
* Fortran module files in `LAPACK95/include/*.mod`.

### CMake

```sh
cmake -B build
cmake --build build
```

### Meson

```sh
meson setup build

meson compile -C build
```

## Install

suppose you wish to install under `~/.local/lapack95`

### CMake

```sh
cmake -DCMAKE_INSTALL_PREFIX=$HOME/.local -B build

cmake --build build --parallel

cmake --install build
```

### Meson

```sh
meson setup build --prefix=$HOME/.local

meson install -C build
```

This also installs the PkgConfig generated `~/.local/lib/pkgconfig/lapack95.pc`
Check that this directory is in `echo $PKG_CONFIG_PATH` and if not, add to ~/.bashrc:

```sh
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$HOME/.local/lib/pkgconfig
```

## Use in a cmake project

This library can be used inside a cmake project by adding this repository with `add_subdirectory`.
One can for example use
[FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html) in your existing project:
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

## Examples

```fortran
! Double precision
use la_precision, only: wp => dp
use f95_lapack, only: la_gesv

real(wp) :: A(3,3), b(3)

call random_number(A)
b(:) = 3*A(:,1) + 2*A(:,2) - A(:,3)

! Solve Ax=b, overwrite b with solution
call la_gesv(A,b)

print *, b
end program

! Output (exact: 3 2 -1):
! 2.9999999999999978        2.0000000000000018       -1.0000000000000004
```
