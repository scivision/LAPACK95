cmake_minimum_required (VERSION 3.12)
project(lapack95demo Fortran)
enable_testing()

option(USEMKL "use MKL libraries")

cmake_policy(SET CMP0074 NEW)

if(USEMKL OR CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
    set(USEMKL 1)
    set(BLA_VENDOR Intel10_64lp_seq)
    set(BLA_F95 ON)
    find_package(LAPACK REQUIRED)
else()
    list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/../cmake/Modules/)

    set(USEMKL 0)
    set(LAPACK95_ROOT ../../LAPACK95)
    find_package(LAPACK95 REQUIRED)
endif()

add_executable(lp95 DemoLapack95.F90 assert.f90)
target_link_libraries(lp95 PRIVATE ${LAPACK95_LIBRARIES})
target_include_directories(lp95 PRIVATE ${LAPACK95_INCLUDE_DIRS})
target_compile_definitions(lp95 PRIVATE USEMKL=${USEMKL})

add_test(NAME Lapack95 COMMAND lp95)

