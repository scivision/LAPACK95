#
#  -- LAPACK95 TIMING Makefile (version 2.0) --
#     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
#     August,  2000
#

include ../make.inc

#
# ALPHA
#FIX	= -fixed
#CCOPTS	=
#===================================================
# Pentium
FIX	= -fixed
CCOPTS	= 
#===================================================
# SUN
#FIX	= -fixed
#CCOPTS	= -DSUN
#===================================================
# IBM Power2
#FIX	= -qfixed
#CCOPTS	= 
#===================================================

IL	= ilaenv.o

EXECS	= xtimings xtimingd xtimingc xtimingz

all: xtimingd xtimings xtimingc xtimingz

xtimingd:	xtimingd.o $(IL) rtc.o
	$(FC) $(OPTLIB) xtimingd.o rtc.o $(IL) $(LIBS) -o xtimingd

xtimingd.o:	xtimingd.f90 
	$(FC) -c $(OPTS0) -o $@ $(MODLIB) xtimingd.f90

xtimings:	xtimings.o $(IL) rtc.o
	$(FC) $(OPTLIB) xtimings.o rtc.o $(IL) $(LIBS) -o xtimings

xtimings.o:	xtimings.f90 
	$(FC) -c $(OPTS0) -o $@ $(MODLIB) xtimings.f90

xtimingc:	xtimingc.o $(IL) rtc.o
	$(FC) $(OPTLIB) xtimingc.o rtc.o $(IL) $(LIBS) -o xtimingc
	
xtimingc.o:	xtimingc.f90
	$(FC) -c $(OPTS0) -o $@ $(MODLIB) xtimingc.f90

xtimingz:	xtimingz.o $(IL)  rtc.o
	$(FC) $(OPTLIB) xtimingz.o rtc.o $(IL) $(LIBS) -o xtimingz
	
xtimingz.o:	xtimingz.f90
	$(FC) -c $(OPTS0) -o $@ $(MODLIB) xtimingz.f90

rtc.o:  rtc.c
	$(FC) $(CCOPTS) -O3 -c -o rtc.o rtc.c
	
ilaenv.o:	ilaenv.f
	$(FC) $(FIX) -c $(OPTS0) -o $@ $< 
	

clean:	
	rm -f *.o *.l core $(EXECS)
