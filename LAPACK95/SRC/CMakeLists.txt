cmake_minimum_required(VERSION 3.7)  # 3.7: install
project(LAPACK95 Fortran)

include(compilers.cmake)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../include)

if(NOT realkind)
  set(realkind d)
endif()

include(source_list.cmake)
add_library(lapack95 ${lapack95_sources})
target_include_directories(lapack95 PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
target_compile_options(lapack95 PRIVATE ${FFLAGS})
set_target_properties(lapack95 PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/..)
foreach(k ${realkind})
  target_sources(lapack95 PRIVATE ${${k}_sources})
endforeach()

# Install
if(UNIX AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local" CACHE PATH "..." FORCE)
endif()

install(TARGETS lapack95
        LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/lib
        ARCHIVE DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ # don't omit trailing slash
        DESTINATION ${CMAKE_INSTALL_PREFIX}/include)
