!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
PROGRAM LA_ZGTSVX_ET_EXAMPLE    
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GTSVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, IFAIL, N, NRHS
   REAL(WP) :: RCOND
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IPIV(:)
   COMPLEX(WP), ALLOCATABLE :: DL(:), D(:), DU(:), B(:,:), &
             X(:,:), DLF(:), DF(:), DUF(:), DU2(:)
   REAL(WP), ALLOCATABLE :: DDL(:), DD(:), DDU(:), BB(:,:), FERR(:), BERR(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'ZGTSVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( DL(N-1), DDL(N-1), D(N), DD(N), DU(N-1), DDU(N-1), B(N,NRHS), BB(N,NRHS), &
         X(N,NRHS), IPIV(N), DLF(N-1), DF(N), DUF(N-1), DU2(N-2), FERR(NRHS), BERR(NRHS) )
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
   WRITE ( NOUT, * )'Details of LA_ZZGTSVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GTSVX(DL, D, DU, B, X )'
   DL = DDL; D = DD; DU = DDU; B = BB
   CALL LA_GTSVX(DL, D, DU, B, X )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GTSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GTSVX(DL, D, DU, B(1:N,1), X(1:N,1) )'
   DL = DDL; D = DD; DU = DDU; B = BB
   CALL LA_GTSVX(DL, D, DU, B(1:N,1), X(1:N,1) )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GTSVX:'
   WRITE (NOUT,FMT) X(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GTSVX(DL, D, DU, B, X, IPIV=IPIV)'
   DL = DDL; D = DD; DU = DDU; B = BB
   CALL LA_GTSVX(DL, D, DU, B, X, IPIV=IPIV )
   WRITE(NOUT,*) 'Pivots ', IPIV
   WRITE(NOUT,*)
   WRITE(NOUT,*) ' CALL LA_GTSVX(DL, D, DU, B(1:N,1), X(1:N,1), INFO=IFAIL )'
   DL = DDL; D = DD; DU = DDU; B = BB
   CALL LA_GTSVX(DL, D, DU, B(1:N,1), X(1:N,1), INFO=IFAIL )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GTSVX, INFO = ', IFAIL
   WRITE (NOUT,FMT) X(1:N,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GTSVX(DL, D, DU, B, X, DLF, DF, DUF, DU2, ', &
                 'IPIV, ''N'', ''N'', FERR, BERR, RCOND, IFAIL )'
   DL = DDL; D = DD; DU = DDU; B = BB; X = HUGE(1.0_WP)
   CALL LA_GTSVX(DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV, 'N', 'N', &
                   FERR, BERR, RCOND, IFAIL )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GTSVX, INFO = ', &
                 IFAIL, ' RCOND = ', RCOND
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
!
END PROGRAM LA_ZGTSVX_ET_EXAMPLE
