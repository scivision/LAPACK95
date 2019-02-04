PROGRAM LA_SPBSV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_PBSV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: KD, I, J, K, INFO, N, NRHS
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: A(:,:), B(:,:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'SPBSV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, KD, NRHS
   PRINT *, 'N = ', N, ' KD = ', KD, ' NRHS = ', NRHS
   ALLOCATE ( A(KD+1,N), AA(KD+1,N), B(N,NRHS), BB(N,NRHS) )
!
      AA = HUGE(1.0_WP)
      DO I = 1, KD+1
        READ (NIN, *) (AA(I, J), J = KD-I+2, N)
      ENDDO
!     DO J = 1, NRHS
!        BB(:,J) = SUM( AA, DIM=2)*J
!     ENDDO
      B = 0.0_WP
      DO K = 1, NRHS
         DO I = 1, N
            DO J = MAX(1,-N+I+KD+1), KD
!              PRINT *, K, I, J, I-J+KD+1, AA(J,I-J+KD+1)
               BB(I,K) = AA(J,I-J+KD+1) + BB(I,K)
            ENDDO
            DO J = MAX(1,KD+2-I), KD+1
               BB(I,K) = AA(J,I) + BB(I,K)
!              PRINT *, K, I, J, AA(J,I)
            ENDDO
         ENDDO
         BB(:,K) = BB(:,K)*K
      ENDDO
   A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, KD+1
        WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,1:N)
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_SPBSV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSV( A, B )'
   A=AA; B=BB
   IF (NRHS .GT. 1) THEN
      CALL LA_PBSV( A, B )
   ELSE
      CALL LA_PBSV( A, B(1:N,1) )
   END IF
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PBSV:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) B(:,J)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSV( A, B, UPLO=''L'')'
   B=BB; A = HUGE(1.0_WP)
   DO I = 1, KD+1
      A(I,1:N-I+1) = AA(KD+2-I,I:N)
      WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,1:N)
   ENDDO
   CALL LA_PBSV( A, B, UPLO='L')
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PBSV:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) B(:,J)
   END DO
   A=AA; B=BB
   CALL LA_PBSV( A, B(1:N,1), INFO=INFO )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PBSV, INFO = ', INFO
   WRITE (NOUT,FMT) B(1:N,1)
!
END PROGRAM LA_SPBSV_ET_EXAMPLE
