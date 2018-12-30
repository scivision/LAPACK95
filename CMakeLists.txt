cmake_minimum_required(VERSION 3.0) # No idea

project(LAPACK95 Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

set(srcdir ${CMAKE_CURRENT_SOURCE_DIR}/LAPACK95/SRC)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
set(LAPACK95_INCLUDE_DIR ${CMAKE_Fortran_MODULE_DIRECTORY})

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/source_list.cmake)

add_library(lapack95 ${lapack95_sources})

target_include_directories(lapack95 PUBLIC ${LAPACK95_INCLUDE_DIR})

install(TARGETS lapack95
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
        DESTINATION include)
