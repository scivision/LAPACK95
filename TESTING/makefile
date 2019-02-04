#
#  -- LAPACK95 interface driver routine (version 2.0) --
#     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
#     August,  2000
#
include ../make.inc

# Note: LAPACK  v3 is required, LAPACK_PATH points to it

TESTLIB  = LIB/testlib.a
LIBS1    = $(TESTLIB) $(LIBS)

$(TESTLIB):
	(mkdir tmp; cd tmp; $(FC1) -c $(OPTS0) $(LAPACK_PATH)/TESTING/LIN/*.f &&\
	$(FC1) -c $(OPTS0) $(LAPACK_PATH)/TESTING/EIG/*.f &&\
	rm -f ../$(TESTLIB) &&\
	ar cr ../$(TESTLIB) *.o && cd ../ ; rm -rf tmp )

OBJO =  la_xerbla.o la_wprint.o la_uestop.o la_etime.o la_erinfo.o \
	la_aux_aa01.o la_alaesm.o la_alasum.o la_alasvm.o

OBJS =  la_schkaa.o la_sdrvge.o la_schkge.o la_serrvx.o la_serrge.o \
        la_sdrvgb.o la_sdrvpo.o la_sdrvgt.o la_sdrvpp.o la_sdrvpb.o \
	la_sdrvpt.o la_sdrvsy.o la_sdrvsp.o la_sdrvls.o la_serrls.o \
	la_test_sgesv.o  la_test_sgetrf.o la_test_sgecon.o la_test_sgetrs.o \
	la_test_sgetri.o la_test_sgerfs.o la_test_sgeequ.o la_test_sgesvx.o \
	la_test_sgbsv.o  la_test_sgtsv.o  la_test_sposv.o  la_test_sppsv.o  \
        la_test_spbsv.o  la_test_sptsv.o  la_test_ssysv.o  la_test_sspsv.o  \
	la_test_sgels.o  la_test_sgelss.o la_test_sgelsx.o \
	la_ws_gelss.o \
	la_test_sgelsy.o  la_test_sgelsd.o la_test_ssysvx.o la_test_sspsvx.o \
	la_test_sptsvx.o  la_test_spbsvx.o la_test_sposvx.o la_test_sppsvx.o \
	la_test_sgtsvx.o  la_test_sgbsvx.o 

OBJD =  la_dchkaa.o la_ddrvge.o la_dchkge.o la_derrvx.o la_derrge.o \
	la_ddrvgb.o la_ddrvgt.o la_ddrvpo.o la_ddrvpp.o la_ddrvpb.o \
	la_ddrvpt.o la_ddrvsy.o la_ddrvsp.o la_ddrvls.o la_derrls.o \
	la_test_dgesv.o  la_test_dgetrf.o la_test_dgecon.o la_test_dgetrs.o \
	la_test_dgetri.o la_test_dgerfs.o la_test_dgeequ.o la_test_dgesvx.o \
	la_test_dgbsv.o  la_test_dgtsv.o  la_test_dposv.o  la_test_dppsv.o  \
	la_test_dpbsv.o  la_test_dptsv.o  la_test_dsysv.o  la_test_dspsv.o  \
	la_test_dgels.o  la_test_dgelss.o la_test_dgelsx.o la_test_dgelsy.o \
        la_test_dgelsd.o la_test_dsysvx.o la_test_dspsvx.o la_test_dptsvx.o \
	la_test_dpbsvx.o la_test_dposvx.o la_test_dppsvx.o la_test_dgtsvx.o \
	la_test_dgbsvx.o  \
        la_ws_gelss.o

OBJC =  la_cchkaa.o la_cdrvge.o la_cchkge.o la_cerrvx.o la_cerrge.o \
	la_cdrvgb.o la_cdrvgt.o la_cdrvpo.o la_cdrvpp.o la_cdrvpb.o \
	la_cdrvpt.o la_cdrvsy.o la_cdrvsp.o la_cdrvhe.o la_cdrvhp.o \
	la_cdrvls.o la_cerrls.o la_test_cgelsx.o \
        la_test_cgesv.o  la_test_cgetrf.o la_test_cgecon.o la_test_cgetrs.o \
        la_test_cgetri.o la_test_cgerfs.o la_test_cgeequ.o la_test_cgesvx.o \
        la_test_cgbsv.o  la_test_cgtsv.o  la_test_cposv.o  la_test_cppsv.o  \
	la_test_cpbsv.o  la_test_cptsv.o  la_test_csysv.o  la_test_cspsv.o  \
	la_test_chesv.o  la_test_chpsv.o  la_test_cgels.o  la_test_cgelss.o \
	la_test_cgelsd.o la_test_cgbsvx.o la_test_cgtsvx.o \
	la_test_cposvx.o la_test_cppsvx.o la_test_cpbsvx.o la_test_cptsvx.o \
	la_test_csysvx.o la_test_chesvx.o la_test_cspsvx.o la_test_chpsvx.o \
	la_test_cgelsy.o \
	la_ws_gelss.o 

OBJZ =  la_zchkaa.o la_zdrvge.o la_zchkge.o la_zerrvx.o la_zerrge.o \
	la_zdrvgb.o la_zdrvgt.o la_zdrvpo.o la_zdrvpp.o la_zdrvpt.o \
	la_zdrvpb.o la_zdrvsy.o la_zdrvsp.o la_zdrvhe.o la_zdrvhp.o \
	la_zdrvls.o la_zerrls.o \
	la_test_zgesv.o  la_test_zgetrf.o la_test_zgecon.o la_test_zgetrs.o \
	la_test_zgetri.o la_test_zgerfs.o la_test_zgeequ.o la_test_zgesvx.o \
	la_test_zgbsv.o  la_test_zgtsv.o  la_test_zposv.o  la_test_zppsv.o  \
	la_test_zptsv.o  la_test_zpbsv.o  la_test_zsysv.o  la_test_zspsv.o  \
        la_test_zhesv.o  la_test_zhpsv.o  la_test_zgels.o  la_test_zgelss.o \
	la_test_zgelsx.o la_test_zgelsd.o la_test_zgbsvx.o la_test_zgtsvx.o \
	la_test_zposvx.o la_test_zppsvx.o la_test_zpbsvx.o la_test_zptsvx.o \
	la_test_zsysvx.o la_test_zhesvx.o la_test_zspsvx.o la_test_zhpsvx.o \
	la_test_zgelsy.o \
        la_ws_gelss.o

OBJED = la_dchkee.o la_derrst.o la_ddrvst.o la_ddrvsg.o la_ddrvgg.o \
	la_ddrves.o la_derred.o la_ddrvev.o la_dgsvts.o la_derrgg.o \
	la_ddrvbd.o la_dlsets.o la_dglmts.o la_ddrgev.o la_ddrges.o \
	la_ddrgsx.o la_ddrgvx.o la_ddrvvx.o la_dget23.o la_ddrvsx.o \
	la_dget24.o \
	la_test_dsyev.o  la_test_dspev.o  la_test_dsbev.o la_test_dsygv.o   \
	la_test_dspgv.o  la_test_dsbgv.o  la_test_dstev.o la_test_dsyevd.o  \
	la_test_dgegv.o  la_test_dgegs.o  la_test_dgees.o la_test_dspevd.o  \
	la_test_dsbevd.o la_test_dstevd.o la_test_dgeev.o la_test_dggsvd.o  \
        la_test_dgesvd.o la_test_dgglse.o la_test_dggglm.o la_test_dsbgvd.o \
	la_test_dsygvd.o la_test_dspgvd.o la_test_dsbgvx.o la_test_dspgvx.o \
	la_test_dsygvx.o la_test_dggev.o  la_test_dgges.o la_test_dgesdd.o  \
        la_test_dstevr.o la_test_dsyevr.o la_test_dggesx.o la_test_dggevx.o \
        la_test_dgeevx.o la_test_dsbevx.o la_test_dspevx.o la_test_dstevx.o \
	la_test_dsyevx.o la_test_dgeesx.o \
	la_dcklse.o 
	
	
OBJES = la_schkee.o la_serrst.o la_sdrvst.o la_sdrvsg.o la_sdrvgg.o \
	la_sdrvev.o la_serred.o la_serrgg.o la_sgsvts.o la_sdrves.o \
	la_slsets.o la_sglmts.o la_sdrvbd.o la_sdrgev.o la_sdrges.o \
	la_sdrgsx.o la_sdrgvx.o la_sdrvvx.o la_sget23.o la_sdrvsx.o \
	la_sget24.o \
	la_test_ssyev.o  la_test_sspev.o  la_test_ssbev.o  la_test_ssygv.o \
	la_test_sspgv.o  la_test_ssbgv.o  la_test_sgegv.o  la_test_sgegs.o \
	la_test_ssyevd.o la_test_sspevd.o la_test_ssbevd.o la_test_sstev.o \
	la_test_sstevd.o la_test_sgeev.o  la_test_sggsvd.o la_test_sgees.o \
	la_test_sgglse.o la_test_sggglm.o la_test_sgesvd.o \
	la_test_ssbgvd.o la_test_ssygvd.o la_test_sspgvd.o la_test_ssbgvx.o \
	la_test_sspgvx.o la_test_sggev.o  la_test_sgges.o\
	la_test_ssyevx.o la_test_sstevr.o la_test_ssyevr.o \
	la_test_sggevx.o la_test_sgeevx.o la_test_ssbevx.o la_test_sspevx.o\
	la_test_sstevx.o la_test_sgeesx.o la_test_ssygvx.o \
	la_test_sggesx.o la_test_sgesdd.o 

	
OBJEC = la_cchkee.o la_cerrst.o la_cdrvst.o la_cdrvgg.o la_cdrvsg.o \
	la_cerred.o la_cdrvev.o la_cerrgg.o la_cgsvts.o la_cdrves.o \
	la_clsets.o la_cglmts.o la_cdrvbd.o la_cdrges.o la_cdrgev.o \
	la_cdrgvx.o la_cget24.o la_cdrvsx.o la_cget23.o la_cdrvvx.o \
	la_cdrgsx.o\
	la_test_cgegv.o  la_test_cgegs.o  la_test_cheev.o  la_test_chegv.o  \
	la_test_chpgv.o  la_test_chbgv.o  la_test_chpev.o  la_test_chbev.o  \
	la_test_cheevd.o la_test_chbevd.o la_test_chpevd.o la_test_cgeev.o  \
	la_test_cggsvd.o la_test_cgees.o  la_test_cgglse.o la_test_cggglm.o \
	la_test_cgesvd.o la_test_chbgvd.o la_test_cgges.o  la_test_chbevx.o \
        la_test_chegvd.o la_test_chpgvd.o la_test_cggev.o  \
        la_test_cheevx.o la_test_cheevr.o la_test_cggevx.o la_test_chpgvx.o \
	la_test_chbgvx.o la_test_cgeesx.o la_test_cgeevx.o  \
        la_test_chpevx.o la_test_cggesx.o la_test_chegvx.o \
        la_test_cgesdd.o

	
OBJEZ = la_zchkee.o la_zdrvgg.o la_zdrvsg.o la_zdrvst.o la_zerrst.o \
	la_zdrvev.o la_zerred.o la_zerrgg.o la_zgsvts.o la_zlsets.o \
	la_zdrves.o la_zdrvbd.o la_zdrges.o la_zdrgev.o la_zdrgvx.o \
	la_zget24.o la_zdrvsx.o la_zget23.o la_zdrvvx.o la_zdrgsx.o \
	la_zglmts.o \
	la_test_zgegv.o  la_test_zgegs.o  la_test_zhegv.o  la_test_zhpgv.o  \
	la_test_zhbgv.o  la_test_zheev.o  la_test_zhpev.o  la_test_zhbev.o  \
	la_test_zheevd.o la_test_zhpevd.o la_test_zhbevd.o la_test_zgeev.o  \
	la_test_zggsvd.o la_test_zgees.o  la_test_zgglse.o la_test_zggglm.o \
	la_test_zgesvd.o la_test_zhbgvd.o la_test_zgges.o  la_test_zhbevx.o \
	la_test_zgesdd.o la_test_zhegvd.o la_test_zhpgvd.o la_test_zggev.o  \
	la_test_zheevr.o la_test_zggevx.o la_test_zhpgvx.o la_test_zhegvx.o \
	la_test_zhbgvx.o la_test_zgeesx.o la_test_zgeevx.o la_test_zggesx.o \
	la_test_zhpevx.o la_test_zheevx.o


all:    linsingle lindouble lincomplex lindcomplex eigsingle eigdouble eigcomplex eigdcomplex

OBJSS = $(OBJO) $(OBJS)
linsingle:  $(OBJSS) $(TESTLIB)
	$(FC) $(OPTL) $@ $(OBJSS) $(OPTLIB) $(LIBS1)


OBJSSE = $(OBJO) $(OBJES)
eigsingle:  $(OBJSSE) $(TESTLIB)
	$(FC) $(OPTL) $@ $(OBJSSE) $(OPTLIB) $(LIBS1) 

OBJCC = $(OBJO) $(OBJC)
lincomplex:  $(OBJCC) $(TESTLIB)
	$(FC) $(OPTL) $@ $(OBJCC) $(OPTLIB) $(LIBS1)

OBJCCE =	$(OBJO) $(OBJEC)
eigcomplex:	 $(OBJCCE) $(TESTLIB)
	$(FC) $(OPTL) $@ $(OBJCCE) $(OPTLIB) $(LIBS1)
	 
OBJDD = $(OBJO) $(OBJD)
lindouble: 	 $(OBJDD) $(TESTLIB)
	$(FC) $(OPTL) $@ $(OBJDD) $(OPTLIB) $(LIBS1)

OBJDDE =	$(OBJO) $(OBJED)
eigdouble:	 $(OBJDDE) $(TESTLIB)
	$(FC) $(OPTL) $@ $(OBJDDE) $(OPTLIB) $(LIBS1)


OBJZZ = $(OBJO) $(OBJZ)
lindcomplex:  $(OBJZZ) $(TESTLIB)
	$(FC) $(OPTL) $@ $(OBJZZ) $(OPTLIB) $(LIBS1)

OBJZZE = 	$(OBJO) $(OBJEZ)
eigdcomplex: 	 $(OBJZZE) $(TESTLIB)
	$(FC) $(OPTL) $@ $(OBJZZE) $(OPTLIB) $(LIBS1)

clean: 
	'rm' -f *.o *.l *.mod core
	'rm' -f linsingle lindouble lincomplex lindcomplex
	'rm' -f eigdouble eigsingle eigcomplex eigdcomplex

clean1: 
	'rm' -f *.o *.l *.mod core
	'rm' -f linsingle lindouble lincomplex lindcomplex
	'rm' -f eigdouble eigsingle eigcomplex eigdcomplex
	'rm' -f $(TESTLIB)
