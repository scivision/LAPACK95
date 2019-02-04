PROGRAM LA_CGESVX_ET_EXAMPLE
!
!  -- LAPACK95 INTERFACE DRIVER ROUTINE (VERSION 3.0) --
!     UNI-C, DENMARK; UNIV. OF TENNESSEE, USA; NAG LTD., UK
!     SEPTEMBER, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GESVX, LA_GETRF
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
   CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F9.3,1H,,F9.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   CHARACTER(LEN=1) :: EQUED
   INTEGER :: I, J, INFO, N, NRHS
   REAL(WP) :: RPVGRW, RCOND, BERR1, FERR1
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IPIV(:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), FERR(:), BERR(:), R(:), C(:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:), AF(:,:), X(:,:), B1(:), X1(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'GESVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   ALLOCATE ( IPIV(N), A(N,N), B(N,NRHS), AA(N,N), BB(N,NRHS), &
               FERR(NRHS), BERR(NRHS), R(N), C(N), AF(N,N), &
               X(N,NRHS), B1(N), X1(N) )
!
      DO I = 1, N
        READ (NIN, *) (AA(I, J), J = 1, N)
      ENDDO
      DO J = 1, NRHS
         BB(:,J) = SUM( AA, DIM=2)*J
      ENDDO
      A = AA; B = BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE (NOUT,FMT) A(I,:)
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CGESVX LAPACK SUBROUTINE Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVX( A, B, X )'
   WRITE(NOUT,*) 'ON ENTRY: A, B'
   WRITE(NOUT,*) '   A - the original matrix'
   WRITE(NOUT,*) '   B - the right hand side matrix'
   WRITE(NOUT,*) 'ON EXIT: X'
   A=AA; B=BB
   CALL LA_GESVX( A, B, X )
   WRITE(NOUT,*)'   The solution vectors X computed by LA_GESVX:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) X(:,J)
   END DO
   A=AA; B=BB
   B1(1:N)=B(1:N,1)
   CALL LA_GESVX( A, B1, X1 )
   WRITE(NOUT,*)'   The solution vector X computed by LA_GESVX:'
   WRITE (NOUT,FMT) X1(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_GESVX(A, B, X, AF, IPIV, FACT='F', TRANS='N')"
   WRITE(NOUT,*) 'ON ENTRY: A, B, AF, IPIV'
   WRITE(NOUT,*) '   A - the original matrix'
   WRITE(NOUT,*) '   B - the right hand side matrix'
   WRITE(NOUT,*) '   AF - the factors L U of matrix A'
   WRITE(NOUT,*) '   IPIV - the pivot'
   WRITE(NOUT,*) 'ON EXIT: X'
   A=AA; B=BB; AF=A
   CALL LA_GETRF(AF, IPIV)
   CALL LA_GESVX(A,B,X,AF,IPIV,FACT='F',TRANS='N')
   WRITE(NOUT,*)'   The solution vectors X computed by LA_GESVX:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) X(:,J)
   END DO
   A=AA; B=BB; AF=A
   B1(1:N)=B(1:N,1)
   CALL LA_GETRF(AF, IPIV)
   CALL LA_GESVX(A,B(:,1),X(:,1),AF,IPIV,FACT='F',TRANS='N')
   WRITE(NOUT,*)'   The solution vector X computed by LA_GESVX:'
   WRITE (NOUT,FMT) X(:,1)
   A=AA; B=BB; AF=A
   CALL LA_GETRF(AF, IPIV)
   CALL LA_GESVX(A, B, X, AF, IPIV, 'N', 'N', EQUED, &
                    R, C, FERR, BERR, RCOND, RPVGRW, INFO)
   WRITE(NOUT,*)'   The solution vectors X computed by LA_GESVX:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) X(:,J)
   END DO
   A=AA; B=BB; AF=A
   B1(1:N)=B(1:N,1)
   CALL LA_GETRF(AF, IPIV)
   CALL LA_GESVX( A, B1, X1, AF, IPIV, 'N', 'N', EQUED, &
                     R, C, FERR1, BERR1, RCOND, RPVGRW, INFO )
   WRITE(NOUT,*)'   The solution vector X computed by LA_GESVX:'
   WRITE (NOUT,FMT) X1(:)
!
END PROGRAM LA_CGESVX_ET_EXAMPLE
