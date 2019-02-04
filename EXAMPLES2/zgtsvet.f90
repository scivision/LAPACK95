PROGRAM LA_ZGTSV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GTSV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT =                              &
     &                   '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, IFAIL, N, NRHS
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: DDL(:), DD(:), DDU(:), BB(:,:)
   COMPLEX(WP), ALLOCATABLE :: DL(:), D(:), DU(:), B(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'ZGTSV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( DL(N-1), DDL(N-1), D(N), DD(N), DU(N-1), DDU(N-1), B(N,NRHS), BB(N,NRHS) )
!
        READ (NIN, *) DDU(:), DD(:), DDL(:)
        
      BB(1,:) = DD(1) + DDU(1)
!     BB(2:N-1,:) = DDL(1:N-2) + DD(2:N-1) + DDU(2:N-1)
      DO I = 2, N-1
         BB(I,:) = DDL(I-1) + DD(I) + DDU(I)
      ENDDO
      BB(N,:) = DDL(N-1) + DD(N-1)
      DO I = 1, NRHS
         BB(:,I) = BB(:,I)*I
      ENDDO
!
   DL = DDL; D = DD; DU = DDU; B = BB
      WRITE(NOUT,*) 'The matrix A:'
      WRITE (NOUT,*) 'DU '; WRITE (NOUT,FMT) DU
      WRITE (NOUT,*) 'D  '; WRITE (NOUT,FMT) D
      WRITE (NOUT,*) 'DL '; WRITE (NOUT,FMT) DL
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_ZGTSV LAPACK Subroutine Results.'
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) ' CALL LA_GTSV(DL, D, DU, B )'
   DL = DDL; D = DD; DU = DDU; B = BB
   CALL LA_GTSV(DL, D, DU, B )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GTSV:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GTSV(DL, D, DU, B(1:N,1)'
   DL = DDL; D = DD; DU = DDU; B = BB
   CALL LA_GTSV(DL, D, DU, B(1:N,1) )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GTSV:'
   WRITE (NOUT,FMT) B(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GTSV(DL, D, DU, B, INFO=IFAIL )'
   DL = DDL; D = DD; DU = DDU; B = BB
   CALL LA_GTSV(DL, D, DU, B, INFO=IFAIL )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GTSV:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) ' CALL LA_GTSV(DL, D, DU, B(1:N,1), IFAIL )'
   DL = DDL; D = DD; DU = DDU; B = BB
   CALL LA_GTSV(DL, D, DU, B(1:N,1), IFAIL )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GTSV:'
   WRITE (NOUT,FMT) B(1:N,1)
!
END PROGRAM LA_ZGTSV_ET_EXAMPLE
