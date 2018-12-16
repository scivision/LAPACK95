PROGRAM LA_DPTSV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_PTSV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, IFAIL, N, NRHS
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: E(:), B(:,:)
   REAL(WP), ALLOCATABLE :: D(:), DD(:), EE(:), BB(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DPTSV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( D(N), DD(N), E(N-1), EE(N-1), B(N,NRHS), BB(N,NRHS) )
!
        READ (NIN, *) DD(:), EE(:)
        
      BB(1,:) = DD(1) + EE(1)
!     BB(2:N-1,:) = EE(1:N-2) + DD(2:N-1) + EE(2:N-1)
      DO I = 2, N-1
         BB(I,:) = EE(I-1) + DD(I) + EE(I)
      ENDDO
      BB(N,:) = EE(N-1) + DD(N)
      DO I = 1, NRHS
         BB(:,I) = BB(:,I)*I
      ENDDO
   D = DD; E = EE; B = BB
      WRITE(NOUT,*) 'The matrix A:'
      WRITE (NOUT,*) 'D  '; WRITE (NOUT,FMT) D
      WRITE (NOUT,*) 'EE '; WRITE (NOUT,FMT) E
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_DPTSV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSV( D, E, B )'
   D = DD; E = EE; B = BB
   IF (NRHS .GT. 1) THEN
      CALL LA_PTSV( D, E, B )
   ELSE
      CALL LA_PTSV( D, E, B(1:N,1) )
   END IF
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PTSV:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) B(:,J)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSV( D, E, B, INFO=IFAIL)'
   D = DD; E = EE; B = BB
   CALL LA_PTSV( D, E, B, INFO=IFAIL )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PTSV, INFO = ', IFAIL
   DO J = 1, NRHS
      WRITE (NOUT,FMT) B(:,J)
   END DO
   D = DD; E = EE; B = BB
   CALL LA_PTSV( D, E, B(1:N,1), IFAIL )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PTSV, INFO = ', IFAIL
   WRITE (NOUT,FMT) B(1:N,1)
!
END PROGRAM LA_DPTSV_ET_EXAMPLE
