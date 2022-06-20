if("d" IN_LIST arith)
  add_executable(lp95 DemoLapack95.f90 assert.f90)
  target_link_libraries(lp95 PRIVATE lapack95)
  add_test(NAME basic COMMAND lp95)
endif()

if(DEFINED ENV{MKLROOT})
  add_executable(svd demo_gesvd.f90)
  target_link_libraries(svd PRIVATE lapack95)
  add_test(NAME Lapack95SVD COMMAND svd)
endif()

#=== LAPACK MKL example with Intel OR Gfortran compilers ===

add_executable(dsvd intel_dgesvd.f90)
target_link_libraries(dsvd PRIVATE lapack95)
add_test(NAME LapackDsvd COMMAND dsvd)
