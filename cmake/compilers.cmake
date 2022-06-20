
add_compile_options(
"$<$<Fortran_COMPILER_ID:GNU>:-mtune=native>"
"$<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-fimplicit-none;-std=legacy;-fno-trapping-math>"
)
