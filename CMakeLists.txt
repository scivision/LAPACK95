cmake_minimum_required(VERSION 3.13...3.20)  # 3.13 target_sources absolute

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

install(TARGETS lapack95
  EXPORT ${PROJECT_NAME}Targets
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/ # don't omit trailing slash
  DESTINATION include)

# for FetchContent use
add_library(LAPACK::LAPACK95 INTERFACE IMPORTED GLOBAL)
target_link_libraries(LAPACK::LAPACK95 INTERFACE lapack95)

# Test
if(BUILD_TESTING)
  add_subdirectory(tests)
endif()

include(cmake/install.cmake)
