cmake_minimum_required (VERSION 3.12)
project(lapack95demo Fortran C)
enable_testing()

cmake_policy(SET CMP0074 NEW)

if(DEFINED ENV{MKLROOT} OR CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  get_property(languages GLOBAL PROPERTY ENABLED_LANGUAGES)
  
  if(NOT C IN_LIST languages)
    message(FATAL_ERROR "MKL/Intel requires project to also have C language enabled")
  endif()

  set(BLA_VENDOR Intel10_64lp_seq)
  set(USEMKL true)
endif()

set(BLA_F95 ON)
find_package(LAPACK)

if(NOT LAPACK95_FOUND)
  set(USEMKL false)
  # we didn't include BLAS95, so have to use our FindLapack95
  list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/../cmake/Modules/)

  set(LAPACK95_ROOT ${CMAKE_SOURCE_DIR}/../LAPACK95)
  find_package(LAPACK95 REQUIRED)
endif()

add_executable(lp95 DemoLapack95.F90 assert.f90)
target_link_libraries(lp95 PRIVATE ${LAPACK95_LIBRARIES})
target_include_directories(lp95 PRIVATE ${LAPACK95_INCLUDE_DIRS})
target_compile_definitions(lp95 PRIVATE USEMKL=${USEMKL})

add_test(NAME Lapack95 COMMAND lp95)

