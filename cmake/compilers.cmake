if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  add_compile_options(
  "$<$<COMPILE_LANGUAGE:Fortran>:-fimplicit-none;-std=legacy;-fno-trapping-math>"
  )
  if(NOT CMAKE_CROSSCOMPILING)
    add_compile_options(-mtune=native)
  endif()
endif()
