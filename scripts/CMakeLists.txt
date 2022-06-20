# this is to build the Intel MKL LAPACK95 sources
# this is for example using GCC with Intel MKL LAPACK95
cmake_minimum_required(VERSION 3.19)

project(MKLlapack95 LANGUAGES Fortran)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  message(FATAL_ERROR "CMAKE_INSTALL_PREFIX is not set")
endif()

if(NOT MKLROOT)
  set(MKLROOT $ENV{MKLROOT})
  if(NOT MKLROOT)
    message(FATAL_ERROR "Intel MKL environment variable MKLROOT not found")
  endif()
endif()

find_program(MAKE_EXECUTABLE
NAMES make gmake mingw32-make
REQUIRED
)

find_path(BLAS95_MAKE
NAMES makefile
HINTS ${MKLROOT}/interfaces/blas95
NO_DEFAULT_PATH
REQUIRED
)

find_path(LAPACK95_MAKE
NAMES makefile
HINTS ${MKLROOT}/interfaces/lapack95
NO_DEFAULT_PATH
REQUIRED
)

get_filename_component(FC ${CMAKE_Fortran_COMPILER} NAME_WE)

message(STATUS "Building LAPACK95 for ${FC}")

include(ExternalProject)

ExternalProject_Add(MKL_BLAS95
SOURCE_DIR ${BLAS95_MAKE}
CONFIGURE_COMMAND ""
TEST_COMMAND ""
INSTALL_COMMAND ""
BUILD_COMMAND ${MAKE_EXECUTABLE} -C <SOURCE_DIR> libintel64 interface=lp64 INSTALL_DIR=${CMAKE_INSTALL_PREFIX} FC=${FC}
)

ExternalProject_Add(MKL_LAPACK95
SOURCE_DIR ${LAPACK95_MAKE}
CONFIGURE_COMMAND ""
TEST_COMMAND ""
INSTALL_COMMAND ""
BUILD_COMMAND ${MAKE_EXECUTABLE} -C <SOURCE_DIR> libintel64 interface=lp64 INSTALL_DIR=${CMAKE_INSTALL_PREFIX} FC=${FC}
)
