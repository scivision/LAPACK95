cmake_minimum_required(VERSION 3.12)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release" FORCE)
endif()
project(lapack95demo C Fortran)
enable_testing()

cmake_policy(SET CMP0074 NEW)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/)

if(DEFINED ENV{MKLROOT} OR CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(USEMKL 1)
else()
  set(USEMKL 0)
endif()

if(UNIX AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local" CACHE PATH "..." FORCE)
endif()

if(NOT CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  set(LAPACK95_ROOT ${CMAKE_INSTALL_PREFIX} ${CMAKE_CURRENT_SOURCE_DIR}/..)
endif()

find_package(LAPACK COMPONENTS LAPACK95)

if(NOT LAPACK_FOUND)
  # try a local non-installed build
  set(LAPACK95_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/..)
  find_package(LAPACK COMPONENTS LAPACK95 REQUIRED)
endif()

add_executable(lp95 DemoLapack95.F90 assert.f90)
target_link_libraries(lp95 PRIVATE ${LAPACK_LIBRARIES})
target_include_directories(lp95 PRIVATE ${LAPACK_INCLUDE_DIRS})
target_compile_definitions(lp95 PRIVATE USEMKL=${USEMKL})
add_test(NAME Lapack95 COMMAND lp95)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../EXAMPLES1/
  ${CMAKE_CURRENT_BINARY_DIR}/ex1)

