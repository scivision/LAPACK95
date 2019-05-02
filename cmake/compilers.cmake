
if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  list(APPEND FFLAGS -std=legacy -fno-trapping-math -march=native)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  # https://www.nag.co.uk/nagware/np/r62_doc/manual/compiler_2_4.html#OPTIONS
endif()
