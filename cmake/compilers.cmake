
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  string(APPEND CMAKE_Fortran_FLAGS " -std=legacy -fno-trapping-math -mtune=native")
endif()
