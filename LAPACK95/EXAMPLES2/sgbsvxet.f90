PROGRAM LA_SGBSVX_ET_EXAMPLE
!
!  -- LAPACK95 INTERFACE DRIVER ROUTINE (VERSION 3.0) --
!     UNI-C, DENMARK; UNIV. OF TENNESSEE, USA; NAG LTD., UK
!     SEPTEMBER, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GBSVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
   CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: K, KL, KU, I, J, INFO, N, NRHS
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IPIV(:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
   REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), X(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'SGBSVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, KL, KU, NRHS
   PRINT *, 'N = ', N, ' KL = ', KL, ' KU = ', KU, ' NRHS = ', NRHS
   ALLOCATE ( A(KL+KU+1,N), AA(KL+KU+1,N), B(N,NRHS), BB(N,NRHS), X(N,NRHS), IPIV(N) )
!
      AA = HUGE(1.0_WP)
      DO I = 1, 1+KU
        READ (NIN, *) (AA(I, J), J = KU-I+2,N)
      ENDDO
      DO I = 2+KU, KL+KU+1
        READ (NIN, *) (AA(I, J), J = 1, N-I+KU+1)
      ENDDO
      B = 0.0_WP
      DO I = 1, NRHS
         DO J = 1, N
            DO K = MAX(1,J-KL), MIN(J+KU,N)
               BB(J,I) = AA(KU+1+J-K,K) + BB(J,I)
            ENDDO
         ENDDO
         BB(:,I) = BB(:,I)*I
      ENDDO
      A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, KL+KU+1
        WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:)
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_SGBSVX LAPACK SUBROUTINE Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX( A, B, X, KL, IPIV=IPIV, INFO=INFO )'
   A=AA; B=BB
   CALL LA_GBSVX( A, B, X, KL, IPIV=IPIV, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'Pivots: ', IPIV
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX( A, B(1:N,1), X(1:N,1), KL, IPIV=IPIV, INFO=INFO )'
   A=AA; B=BB
   CALL LA_GBSVX( A, B(1:N,1), X(1:N,1), KL, IPIV=IPIV, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(:,1)
   WRITE(NOUT,*) 'Pivots: ', IPIV
! 
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX:'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX( A, B, X, KL )'
   A=AA; B=BB
   CALL LA_GBSVX( A, B, X, KL )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX(A, B(1:N,1), X(1:N,1), KL)'
   A=AA; B=BB
   CALL LA_GBSVX(A, B(1:N,1), X(1:N,1), KL)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX:'
   WRITE (NOUT,FMT) X(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX(A, B, X, IPIV=IPIV)'
   A=AA; B=BB
   CALL LA_GBSVX( A, B, X, IPIV=IPIV)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'Pivots: ', IPIV
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX( A, B(1:N,1), X(1:N,1), IPIV=IPIV )'
   A=AA; B=BB
   CALL LA_GBSVX( A, B(1:N,1), X(1:N,1), IPIV=IPIV )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX:'
   WRITE (NOUT,FMT) X(1:N,1)
   WRITE(NOUT,*) 'Pivots: ', IPIV
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX(A(4:4,:), B, X, INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_GBSVX( A(4:4,:), B, X, INFO=INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX( A(4:4,:), B(1:N,1), X(1:N,1), INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_GBSVX( A(4:4,:), B(1:N,1), X(1:N,1), INFO=INFO )
   WRITE(NOUT,*)'   B - the RHS vector.'
   WRITE (NOUT,FMT) B(1:N,1)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_GBSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(1:N,1)
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX(A(4:4,:), B, X, 1, INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_GBSVX( A(4:4,:), B, X, 1, INFO=INFO)
   WRITE(NOUT,*)' INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX(A(4:4,:), B, X, 6, INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_GBSVX( A(4:4,:), B, X, 1, INFO=INFO)
   WRITE(NOUT,*)' INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX( A, B(1:N,1), X(1:N,1), 1, INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_GBSVX( A, B(1:N,1), X(1:N,1), 6, INFO=INFO )
   WRITE(NOUT,*)'    INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GBSVX( A, B(1:N,1), X(1:N,1), 6 )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_GBSVX( A, B(1:N,1), X(1:N,1), 6 )
   WRITE(NOUT,*)'   INFO = ', INFO
!
END PROGRAM LA_SGBSVX_ET_EXAMPLE
