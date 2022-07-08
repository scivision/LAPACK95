cmake_minimum_required(VERSION 3.19...3.24)

project(lapack95
LANGUAGES C Fortran
VERSION 3.0
)

enable_testing()

include(cmake/options.cmake)
include(cmake/compilers.cmake)

find_package(LAPACK REQUIRED)

# download lapack95 source
include(cmake/lapack_src.cmake)

if(NOT arith)
  # only one precision at a time
  set(arith "d")
endif()

list(LENGTH arith L)
if(NOT L EQUAL 1)
  message(FATAL_ERROR "Lapack95 modules require exactly one arithmetic precision")
endif()

message(STATUS "LAPACK95: building precision: ${arith}")

add_library(lapack95)
target_link_libraries(lapack95 PRIVATE LAPACK::LAPACK)
set_target_properties(lapack95 PROPERTIES
Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
EXPORT_NAME LAPACK95
)
target_include_directories(lapack95 PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>
)

configure_file(src/CMakeLists.txt ${lapack95_SOURCE_DIR}/SRC/ COPYONLY)
add_subdirectory(${lapack95_SOURCE_DIR}/SRC ${lapack95_BINARY_DIR}/src)

install(TARGETS lapack95 EXPORT ${PROJECT_NAME}-targets)

install(FILES
${CMAKE_CURRENT_BINARY_DIR}/include/f77_lapack.mod
${CMAKE_CURRENT_BINARY_DIR}/include/f95_lapack.mod
${CMAKE_CURRENT_BINARY_DIR}/include/la_auxmod.mod
${CMAKE_CURRENT_BINARY_DIR}/include/la_precision.mod
TYPE INCLUDE)

# for FetchContent use
add_library(LAPACK::LAPACK95 INTERFACE IMPORTED GLOBAL)
target_link_libraries(LAPACK::LAPACK95 INTERFACE lapack95)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(test)
endif()

include(cmake/install.cmake)
