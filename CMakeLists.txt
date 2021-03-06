cmake_minimum_required(VERSION 3.19...3.21)

project(LAPACK
LANGUAGES C Fortran
VERSION 3.0)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

include(cmake/compilers.cmake)

if(NOT arith)
  # only one precision at a time
  set(arith "d")
endif()

list(LENGTH arith L)
if(NOT L EQUAL 1)
  message(FATAL_ERROR "Lapack95 modules require exactly one arithmetic precision")
endif()

message(STATUS "LAPACK95: building precision: ${arith}")

find_package(LAPACK REQUIRED)

add_library(lapack95)
target_link_libraries(lapack95 PRIVATE LAPACK::LAPACK)
set_target_properties(lapack95 PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
  EXPORT_NAME LAPACK95)
target_include_directories(lapack95
PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include  # for IBM XL
INTERFACE
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>)

add_subdirectory(src)

install(TARGETS lapack95 EXPORT ${PROJECT_NAME}Targets)

install(FILES
${CMAKE_CURRENT_BINARY_DIR}/include/f77_lapack.mod
${CMAKE_CURRENT_BINARY_DIR}/include/f95_lapack.mod
${CMAKE_CURRENT_BINARY_DIR}/include/la_auxmod.mod
${CMAKE_CURRENT_BINARY_DIR}/include/la_precision.mod
TYPE INCLUDE)

# for FetchContent use
add_library(LAPACK::LAPACK95 INTERFACE IMPORTED GLOBAL)
target_link_libraries(LAPACK::LAPACK95 INTERFACE lapack95)

# Test
if(BUILD_TESTING)
  add_subdirectory(tests)
endif()

include(cmake/install.cmake)
