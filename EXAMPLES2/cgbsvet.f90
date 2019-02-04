PROGRAM LA_CGBSV_ET_EXAMPLE
!
!  -- LAPACK95 INTERFACE DRIVER ROUTINE (VERSION 3.0) --
!     UNI-C, DENMARK; UNIV. OF TENNESSEE, USA; NAG LTD., UK
!     SEPTEMBER, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GBSV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
   CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F9.3,1H,,F9.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: K, KL, KU, I, J, INFO, N, NRHS
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IPIV(:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CGBSV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, KL, KU, NRHS
   PRINT *, 'N = ', N, ' KL = ', KL, ' KU = ', KU, ' NRHS = ', NRHS
   ALLOCATE ( A(2*KL+KU+1,N), AA(2*KL+KU+1,N), B(N,NRHS), BB(N,NRHS), IPIV(N) )
!
      DO I = KL+1, KL+1+KU
        READ (NIN, *) (AA(I, J), J = KU-I+KL+2, N)
      ENDDO
      DO I = KL+2+KU, 2*KL+KU+1
        READ (NIN, *) (AA(I, J), J = 1, N-I+KL+1+KU)
      ENDDO
!     ENDDO
      B = 0.0_WP
      DO I = 1, NRHS
         DO J = 1, N
            DO K = MAX(1,J-KL), MIN(J+KU,N)
               BB(J,I) = AA(KL+KU+1+J-K,K) + BB(J,I)
            ENDDO
         ENDDO
         BB(:,I) = BB(:,I)*I
      ENDDO
      A = AA; B = BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = KL+1, 2*KL+KU+1
        WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,MAX(1,KL+KU+2-I):MIN(N,KL+KU+N+1-I))
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
      WRITE ( NOUT, * )'Details of LA_CGBSV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSV( A, B, KL, IPIV, INFO )'
   A=AA; B=BB
   CALL LA_GBSV( A, B, KL, IPIV, INFO )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GBSV, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
   WRITE(NOUT,*) 'Pivots: ', IPIV
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSV( A, B(1:N,1), KL, IPIV, INFO )'
   A=AA; B=BB
   CALL LA_GBSV( A, B(1:N,1), KL, IPIV, INFO )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GBSV, INFO = ', INFO
   WRITE (NOUT,FMT) B(:,1)
   WRITE(NOUT,*) 'Pivots: ', IPIV
! 
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GBSV:'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSV(A, B, KL)'
   A=AA; B=BB
   CALL LA_GBSV(A,B,KL)
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GBSV:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSV(A, B(1:N,1), KL)'
   A=AA; B=BB
      CALL LA_GBSV(A,B(1:N,1),KL)
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GBSV:'
   WRITE (NOUT,FMT) B(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSV(A, B, IPIV=IPIV)'
   A=AA; B=BB
   CALL LA_GBSV(A,B,IPIV=IPIV)
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GBSV:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) B(:,J)
   END DO
   WRITE(NOUT,*) 'Pivots: ', IPIV
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSV(A,B(1:N,1),KL,IPIV)'
   A=AA; B=BB
   CALL LA_GBSV(A,B(1:N,1),KL,IPIV)
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_GBSV:'
   WRITE (NOUT,FMT) B(1:N,1)
   WRITE(NOUT,*) 'Pivots: ', IPIV
!
END PROGRAM LA_CGBSV_ET_EXAMPLE
