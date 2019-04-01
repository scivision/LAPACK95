if(USEMKL)
  message(WARNING "Netlib Examples are only for Netlib LAPACK95 unless you add preprocessing USE statements (simple to do)")
  return()
endif()

include_directories(${LAPACK_INCLUDE_DIRS})
link_libraries(${LAPACK_LIBRARIES})

add_executable(gbsv la_gbsv_example.f90)
add_test(NAME GBSV COMMAND gbsv ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(gesvx la_gesvx_example.f90)
add_test(NAME GESVX COMMAND gesvx ${CMAKE_CURRENT_SOURCE_DIR})


#add_executable(hpgvx la_hpgvx_example.f90)
#add_test(NAME HPGVX COMMAND hpgvx)
#la_spsv_example.f90
#la_gbsvx_example.f90    la_gges_example.f90   la_lamch_example.f90  la_spsvx_example.f90
#la_gees_example.f90     la_ggesx_example.f90  la_pbsv_example.f90   la_stev_example.f90
#la_geesx_example.f90    la_ggev_example.f90   la_pbsvx_example.f90  la_stevr_example.f90
#la_geevab_example.f90   la_ggevx_example.f90  la_posv_example.f90   la_stevx_example.f90
#la_geev_example.f90     la_ggglm_example.f90  la_posvx_example.f90  la_syev_example.f90
#la_geevx_example.f90    la_gglse_example.f90  la_ppsv_example.f90   la_syevr_example.f90
#la_gels_example.f90     la_ggsvd_example.f90  la_ppsvx_example.f90  la_syevx_example.f90
#la_gelss_example.f90    la_gtsv_example.f90   la_ptsv_example.f90   la_sygv_example.f90
#la_gelsy_example.f90    la_gtsvx_example.f90  la_ptsvx_example.f90  la_sygvx_example.f90

add_executable(geqrf la_geqrf_example.f90)
add_test(NAME GEQRF COMMAND geqrf)

#la_hesv_example.f90   la_sbev_example.f90   la_sysv_example.f90
#la_gesvd_example.f90    la_hpev_example.f90   la_sbevx_example.f90  la_sysvx_example.f90
#la_gesv_example.f90     la_hpevx_example.f90  la_sbgv_example.f90
#la_gesvxab_example.f90  la_hpgv_example.f90   la_sbgvx_example.f90

