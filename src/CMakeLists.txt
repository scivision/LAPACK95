# Copied from LAPACK95/SRC/makefile

target_sources(lapack95 PRIVATE
la_auxmod.f90 la_erinfo.f90 la_ws_gels.f90 la_ws_gelss.f90
)

set(s_sources
f77_lapack_single.f90 f95_lapack_single.f90
la_sgesv.f90 la_sgesv1.f90 la_sgesvx.f90 la_sgesvx1.f90
la_sgetrf.f90 la_slagge.f90 la_slange.f90 la_slange1.f90
la_sgetri.f90 la_sgetrs.f90 la_sgetrs1.f90 la_sgeequ.f90
la_sgerfs.f90 la_sgerfs1.f90 la_ssytrd.f90 la_ssygst.f90
la_sorgtr.f90 la_spotrf.f90 la_sgbtrf.f90 la_slamch.f90
la_sgbsv.f90 la_sgbsv1.f90 la_sgbsvx.f90 la_sgbsvx1.f90
la_sgtsv.f90 la_sgtsv1.f90 la_sgtsvx.f90 la_sgtsvx1.f90
la_sposv.f90 la_sposv1.f90 la_sposvx.f90 la_sposvx1.f90
la_sppsv.f90 la_sppsv1.f90 la_sppsvx.f90 la_sppsvx1.f90
la_spbsv.f90 la_spbsv1.f90 la_spbsvx.f90 la_spbsvx1.f90
la_sptsv.f90 la_sptsv1.f90 la_sptsvx.f90 la_sptsvx1.f90
la_ssysv.f90 la_ssysv1.f90 la_ssysvx.f90 la_ssysvx1.f90
la_sspsv.f90 la_sspsv1.f90 la_sspsvx.f90 la_sspsvx1.f90
la_sgels.f90 la_sgels1.f90 la_sgelsx.f90 la_sgelsx1.f90
la_sgelss.f90 la_sgelss1.f90 la_sgglse.f90 la_sggglm.f90
la_ssyev.f90 la_ssyevd.f90 la_ssyevx.f90 la_sspev.f90
la_sspevd.f90 la_sspevx.f90 la_ssbev.f90 la_ssbevd.f90
la_ssbevx.f90 la_sstev.f90 la_sstevd.f90 la_sstevx.f90
la_sgees.f90 la_sgeesx.f90 la_sgeev.f90 la_sgeevx.f90
la_sgesvd.f90 la_ssygv.f90 la_sspgv.f90 la_ssbgv.f90
la_sgegs.f90 la_sgegv.f90 la_sggsvd.f90 la_sgelsy.f90
la_sgelsy1.f90 la_sspgvd.f90 la_ssbgvd.f90 la_ssygvd.f90
la_ssbgvx.f90 la_sspgvx.f90 la_sggev.f90 la_sgges.f90
la_sstevr.f90 la_sgelsd.f90 la_sgelsd1.f90 la_ssyevr.f90
la_sggevx.f90 la_ssygvx.f90 la_sggesx.f90 la_sgesdd.f90
)

set(d_sources
f77_lapack_double.f90 f95_lapack_double.f90
la_dgesv.f90 la_dgesv1.f90 la_dgesvx.f90 la_dgesvx1.f90
la_dgetrf.f90 la_dlagge.f90 la_dlange.f90 la_dlange1.f90
la_dgetri.f90 la_dgetrs.f90 la_dgetrs1.f90 la_dgeequ.f90
la_dgerfs.f90 la_dgerfs1.f90 la_dsytrd.f90 la_dsygst.f90
la_dorgtr.f90 la_dpotrf.f90 la_dgbtrf.f90 la_dlamch.f90
la_dgbsv.f90 la_dgbsv1.f90 la_dgbsvx.f90 la_dgbsvx1.f90
la_dgtsv.f90 la_dgtsv1.f90 la_dgtsvx.f90 la_dgtsvx1.f90
la_dposv.f90 la_dposv1.f90 la_dposvx.f90 la_dposvx1.f90
la_dppsv.f90 la_dppsv1.f90 la_dppsvx.f90 la_dppsvx1.f90
la_dpbsv.f90 la_dpbsv1.f90 la_dpbsvx.f90 la_dpbsvx1.f90
la_dptsv.f90 la_dptsv1.f90 la_dptsvx.f90 la_dptsvx1.f90
la_dsysv.f90 la_dsysv1.f90 la_dsysvx.f90 la_dsysvx1.f90
la_dspsv.f90 la_dspsv1.f90 la_dspsvx.f90 la_dspsvx1.f90
la_dgels.f90 la_dgels1.f90 la_dgelsx.f90 la_dgelsx1.f90
la_dgelss.f90 la_dgelss1.f90 la_dgglse.f90 la_dggglm.f90
la_dsyev.f90 la_dsyevd.f90 la_dsyevx.f90 la_dspev.f90
la_dspevd.f90 la_dspevx.f90 la_dsbev.f90 la_dsbevd.f90
la_dsbevx.f90 la_dstev.f90 la_dstevd.f90 la_dstevx.f90
la_dgees.f90 la_dgeesx.f90 la_dgeev.f90 la_dgeevx.f90
la_dgesvd.f90 la_dsygv.f90 la_dspgv.f90 la_dsbgv.f90
la_dgegs.f90 la_dgegv.f90 la_dggsvd.f90 la_dgelsy.f90
la_dgelsy1.f90 la_dspgvd.f90 la_dsbgvd.f90 la_dsygvd.f90
la_dsbgvx.f90 la_dspgvx.f90 la_dggev.f90 la_dgges.f90
la_dstevr.f90 la_dgelsd.f90 la_dgelsd1.f90 la_dsyevr.f90
la_dggevx.f90 la_dsygvx.f90 la_dggesx.f90 la_dgesdd.f90
)

set(c_sources
f77_lapack_single_complex.f90 f95_lapack_single_complex.f90
la_cgesv.f90 la_cgesv1.f90 la_cgesvx.f90 la_cgesvx1.f90
la_cgetrf.f90 la_clagge.f90 la_clange.f90 la_clange1.f90
la_cgetri.f90 la_cgetrs.f90 la_cgetrs1.f90 la_cgeequ.f90
la_cgerfs.f90 la_cgerfs1.f90 la_chetrd.f90 la_chegst.f90
la_cungtr.f90 la_cpotrf.f90 la_cgbtrf.f90
la_cgbsv.f90 la_cgbsv1.f90 la_cgbsvx.f90 la_cgbsvx1.f90
la_cgtsv.f90 la_cgtsv1.f90 la_cgtsvx.f90 la_cgtsvx1.f90
la_cposv.f90 la_cposv1.f90 la_cposvx.f90 la_cposvx1.f90
la_cppsv.f90 la_cppsv1.f90 la_cppsvx.f90 la_cppsvx1.f90
la_cpbsv.f90 la_cpbsv1.f90 la_cpbsvx.f90 la_cpbsvx1.f90
la_cptsv.f90 la_cptsv1.f90 la_cptsvx.f90 la_cptsvx1.f90
la_csysv.f90 la_csysv1.f90 la_csysvx.f90 la_csysvx1.f90
la_cspsv.f90 la_cspsv1.f90 la_cspsvx.f90 la_cspsvx1.f90
la_chesv.f90 la_chesv1.f90 la_chesvx.f90 la_chesvx1.f90
la_chpsv.f90 la_chpsv1.f90 la_chpsvx.f90 la_chpsvx1.f90
la_cgels.f90 la_cgels1.f90 la_cgelsx.f90 la_cgelsx1.f90
la_cgelss.f90 la_cgelss1.f90 la_cgglse.f90 la_cggglm.f90
la_cheev.f90 la_cheevd.f90 la_cheevx.f90 la_chpev.f90
la_chpevd.f90 la_chpevx.f90 la_chbev.f90 la_chbevd.f90
la_chbevx.f90 la_chbgvx.f90 la_chpgvx.f90
la_cgees.f90 la_cgeesx.f90 la_cgeev.f90 la_cgeevx.f90
la_cgesvd.f90 la_chegv.f90 la_chpgv.f90 la_chbgv.f90
la_cgegs.f90 la_cgegv.f90 la_cggsvd.f90 la_cgelsy.f90
la_cgelsy1.f90 la_chpgvd.f90 la_chbgvd.f90 la_chegvd.f90
la_cggev.f90 la_cgges.f90 la_cgelsd.f90 la_cgelsd1.f90
la_cheevr.f90 la_cggevx.f90 la_chegvx.f90 la_cggesx.f90
la_cgesdd.f90
)

set(z_sources
f77_lapack_double_dcomplex.f90 f95_lapack_double_dcomplex.f90
la_zgesv.f90 la_zgesv1.f90 la_zgesvx.f90 la_zgesvx1.f90
la_zgetrf.f90 la_zlagge.f90 la_zlange.f90 la_zlange1.f90
la_zgetri.f90 la_zgetrs.f90 la_zgetrs1.f90 la_zgeequ.f90
la_zgerfs.f90 la_zgerfs1.f90 la_zhetrd.f90 la_zhegst.f90
la_zungtr.f90 la_zpotrf.f90 la_zgbtrf.f90
la_zgbsv.f90 la_zgbsv1.f90 la_zgbsvx.f90 la_zgbsvx1.f90
la_zgtsv.f90 la_zgtsv1.f90 la_zgtsvx.f90 la_zgtsvx1.f90
la_zposv.f90 la_zposv1.f90 la_zposvx.f90 la_zposvx1.f90
la_zppsv.f90 la_zppsv1.f90 la_zppsvx.f90 la_zppsvx1.f90
la_zpbsv.f90 la_zpbsv1.f90 la_zpbsvx.f90 la_zpbsvx1.f90
la_zptsv.f90 la_zptsv1.f90 la_zptsvx.f90 la_zptsvx1.f90
la_zsysv.f90 la_zsysv1.f90 la_zsysvx.f90 la_zsysvx1.f90
la_zspsv.f90 la_zspsv1.f90 la_zspsvx.f90 la_zspsvx1.f90
la_zhesv.f90 la_zhesv1.f90 la_zhesvx.f90 la_zhesvx1.f90
la_zhpsv.f90 la_zhpsv1.f90 la_zhpsvx.f90 la_zhpsvx1.f90
la_zgels.f90 la_zgels1.f90 la_zgelsx.f90 la_zgelsx1.f90
la_zgelss.f90 la_zgelss1.f90 la_zgglse.f90 la_zggglm.f90
la_zheev.f90 la_zheevd.f90 la_zheevx.f90 la_zhpev.f90
la_zhpevd.f90 la_zhpevx.f90 la_zhbev.f90 la_zhbevd.f90
la_zhbevx.f90 la_zhbgvx.f90 la_zhpgvx.f90
la_zgees.f90 la_zgeesx.f90 la_zgeev.f90 la_zgeevx.f90
la_zgesvd.f90 la_zhegv.f90 la_zhpgv.f90 la_zhbgv.f90
la_zgegs.f90 la_zgegv.f90 la_zggsvd.f90 la_zgelsy.f90
la_zgelsy1.f90 la_zhpgvd.f90 la_zhbgvd.f90 la_zhegvd.f90
la_zggev.f90 la_zgges.f90 la_zgelsd.f90 la_zgelsd1.f90
la_zheevr.f90 la_zggevx.f90 la_zhegvx.f90 la_zggesx.f90
la_zgesdd.f90
)

# as per original README, only one kind can be compiled
target_sources(lapack95 PRIVATE ${${arith}_sources})
