cmake_minimum_required(VERSION 3.7)  # 3.7: install
project(LAPACK95 C Fortran)
enable_testing()

include(cmake/compilers.cmake)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.12)
  cmake_policy(SET CMP0074 NEW)
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include)

if(NOT realkind)
  set(realkind d)
endif()

add_library(lapack95)
add_subdirectory(src)
target_include_directories(lapack95 PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_compile_options(lapack95 PRIVATE ${FFLAGS})
set_target_properties(lapack95 PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})


# Test
set(LAPACK_ROOT ${CMAKE_CURRENT_SOURCE_DIR})
find_package(LAPACK COMPONENTS LAPACK95)

if(LAPACK_FOUND)
  add_executable(gbsv ${CMAKE_CURRENT_SOURCE_DIR}/EXAMPLES1/la_gbsv_example.f90)
  target_link_libraries(gbsv lapack95 ${LAPACK_LIBRARIES})
  add_test(NAME GBSV COMMAND ./gbsv ${CMAKE_CURRENT_SOURCE_DIR}/EXAMPLES1)
endif()

# Install
if(UNIX AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local" CACHE PATH "..." FORCE)
endif()

install(TARGETS lapack95
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ # don't omit trailing slash
        DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
