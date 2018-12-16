PROGRAM LA_DSTEVR_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_STEVR
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
   CHARACTER(LEN=*), PARAMETER :: FMTR = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
      INTEGER :: I, INFO, N, M, IL, IU, NNN
      REAL(WP) :: VL, VU
!  .. LOCAL ARRAYS ..
      INTEGER,ALLOCATABLE :: ISUPPZ(:) 
   REAL(WP), ALLOCATABLE :: D(:), DD(:), E(:), EE(:), W(:), Z(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DSTEVR ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
      PRINT *, 'N = ', N
      NNN=2*MAX(1,N)
      ALLOCATE ( D(N), DD(N), E(N), EE(N), W(N), Z(N,N), ISUPPZ(NNN) )
      INFO = 0; M=N
!
      READ (NIN, *) DD
      READ (NIN, *) EE
   WRITE(NOUT,*) 'The matrix A:'
   WRITE (NOUT,*) 'D'; WRITE (NOUT,FMTR) DD
   WRITE (NOUT,*) 'E'; WRITE (NOUT,FMTR) EE
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_DSTEVR LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, INFO=INFO )'
      D=DD; E=EE
      CALL LA_STEVR( D, E, W, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
      WRITE(NOUT,FMTR) W
!      
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z=Z, INFO=INFO )'
      D=DD; E=EE
      CALL LA_STEVR( D, E, W, Z=Z, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
      WRITE(NOUT,FMTR) W
      WRITE(NOUT,*) 'EIGENVECTORS:'
      DO I = 1, M; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMTR) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, VL=3.0_WP, M=M, INFO=INFO )'
   D=DD; E=EE
      CALL LA_STEVR( D, E, W, VL=3.0_WP, M=M, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,*) 'The total number of eigenvalues found is ', M
   WRITE(NOUT,FMTR) W(1:M)
!
      VL= -HUGE(1.0_WP); VU=HUGE(1.0_WP)
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z, -HUGE(1.0_WP), HUGE(1.0_WP), M=M, ISUPPZ=ISUPPZ, INFO=INFO)'
      D=DD; E=EE; M=N; Z = HUGE(1.0_WP); W = HUGE(1.0_WP)
      CALL LA_STEVR( D, E, W, Z, -HUGE(1.0_WP), HUGE(1.0_WP), M=M, ISUPPZ=ISUPPZ, INFO=INFO)
      WRITE(NOUT,*) 'M, INFO, EIGENVALUES:', M, INFO
      WRITE(NOUT,FMTR) W
      WRITE(NOUT,*) 'EIGENVECTORS:'
      DO I = 1, M; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMTR) Z(:,I); END DO
        WRITE(NOUT,*) 'ISUPPZ:'; WRITE (NOUT,*) ISUPPZ
!
      IL=2; IU=N
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z, IL=2, IU=N, M=M, ISUPPZ=ISUPPZ, INFO=INFO)'
      D=DD; E=EE; M=N; Z = HUGE(1.0_WP); W = HUGE(1.0_WP)
      CALL LA_STEVR( D, E, W, Z, IL=IL, IU=IU, M=M, ISUPPZ=ISUPPZ, INFO=INFO)
      WRITE(NOUT,*) 'M, INFO, EIGENVALUES:', M, INFO
      WRITE(NOUT,FMTR) W
      WRITE(NOUT,*) 'EIGENVECTORS:'
      DO I = 1, M; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMTR) Z(:,I); END DO
        WRITE(NOUT,*) 'ISUPPZ:'; WRITE (NOUT,*) ISUPPZ     
! ERROR 2
      VL = 1; VU = 10;
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E(1:N+3), W, Z, VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E(1:N+3), W, Z, VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 3
      VL = 1; VU = 10;
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W(1:N-1), Z, VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W(1:N-1), Z, VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 4
      VL = 1; VU = 10;
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z(:,1:N-1), VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, Z(:,1:N-1), VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 4
      VL = 1; VU = 10;
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z(1:N-1,:), VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, Z(1:N-1,:), VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 5
      VL = 1; VU = 10;
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, VL=VL, VU=VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, VL=VL, VU=VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO      
! ERROR 5
      VL = 1; VU = 10;
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z, VL, VU, M=M, ISUPPZ=ISUPPZ(1:MAX(1,N)-3), INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, Z, VL, VU, M=M, ISUPPZ=ISUPPZ(1:MAX(1,N)-3), INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 6
      VL = 2; VU = VL-1;
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z, VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, Z, VL, VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 7
      IL = 1; IU = N; VL = 1; VU =10;
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z, VL, VU, IL, IU, M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, Z, VL, VU, IL, IU, M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 8
      IL=1; IU=IL-1
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z, IL=IL, IU=IU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, Z, IL=IL, IU=IU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 8
      IL=-1; IU=N
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z, ISUPPZ, IL=IL, IU=IU, M=M, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, Z, IL=IL, IU=IU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 9
      IL=1; IU=N+1
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_STEVR( D, E, W, Z, IL=IL, IU=IU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )'
      D=DD; E=EE; Z = HUGE(1.0_WP)
      CALL LA_STEVR(  D, E, W, Z, IL=IL, IU=IU, M=M, ISUPPZ=ISUPPZ, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
END!PROGRAM LA_DSTEVR_ET_EXAMPLE
