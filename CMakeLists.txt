cmake_minimum_required(VERSION 3.13)  # 3.13 target_sources absolute # 3.7: install
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(LAPACK95 C Fortran)
enable_testing()

include(cmake/compilers.cmake)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
cmake_policy(SET CMP0074 NEW)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include)

if(NOT realkind)
  set(realkind d)
endif()

add_library(lapack95)
add_subdirectory(src)
target_include_directories(lapack95 PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_compile_options(lapack95 PRIVATE ${FFLAGS})
set_target_properties(lapack95 PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/lib)


# Test
set(LAPACK95_ROOT ${CMAKE_CURRENT_SOURCE_DIR})
find_package(LAPACK COMPONENTS LAPACK95)

if(LAPACK_OK)
  add_executable(gbsv ${CMAKE_CURRENT_SOURCE_DIR}/EXAMPLES1/la_gbsv_example.f90)
  target_link_libraries(gbsv PRIVATE lapack95 ${LAPACK_LIBRARIES})
  add_test(NAME GBSV COMMAND $<TARGET_FILE:gbsv> ${CMAKE_CURRENT_SOURCE_DIR}/EXAMPLES1)
endif()

# Install
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  if(WIN32)
    set(HOME $ENV{USERPROFILE})
  else()
    set(HOME $ENV{HOME})
  endif()

  set(CMAKE_INSTALL_PREFIX "${HOME}/.local/lapack95" CACHE PATH "..." FORCE)
endif()

install(TARGETS lapack95
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ # don't omit trailing slash
        DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
