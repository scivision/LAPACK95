cmake_minimum_required(VERSION 3.19)

project(DemoLapack95
LANGUAGES C Fortran
)

enable_testing()

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/../cmake)

find_package(LAPACK COMPONENTS LAPACK95 REQUIRED)

if(DEFINED ENV{MKLROOT} OR CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set(mkl true)
else()
  set(mkl false)
endif()

if(LAPACK_d_FOUND)
  add_executable(gbsv la_gbsv_example.F90)
  target_link_libraries(gbsv PRIVATE LAPACK::LAPACK95 LAPACK::LAPACK)
  target_compile_definitions(gbsv PRIVATE $<$<BOOL:${mkl}>:USEMKL>)
  add_test(NAME GBSV COMMAND gbsv ${CMAKE_CURRENT_SOURCE_DIR})

  add_executable(gesvx la_gesvx_example.F90)
  target_link_libraries(gesvx PRIVATE LAPACK::LAPACK95 LAPACK::LAPACK)
  target_compile_definitions(gesvx PRIVATE $<$<BOOL:${mkl}>:USEMKL>)
  add_test(NAME GESVX COMMAND gesvx ${CMAKE_CURRENT_SOURCE_DIR})

  if(NOT mkl)
    add_executable(geqrf la_geqrf_example.f90)
    target_link_libraries(geqrf PRIVATE LAPACK::LAPACK95 LAPACK::LAPACK)
    add_test(NAME GEQRF COMMAND geqrf)
  endif()
endif()
