PROGRAM LA_ZPBSVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_PBSVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   CHARACTER(LEN=1) :: EQU
   INTEGER :: I, J, K, INFO, N, NRHS, KD
   REAL(WP) :: RCOND
!  .. LOCAL ARRAYS ..
   COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:), X(:,:), AF(:,:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), S(:), FERR(:), BERR(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'ZPBSVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, KD, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( A(KD+1,N), AA(KD+1,N), B(N,NRHS), BB(N,NRHS), X(N,NRHS), AF(KD+1,N), &
              S(N), FERR(NRHS), BERR(NRHS) )
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
        WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) AA(I,1:N)
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) BB(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_ZPBSVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX( A, B, X, INFO=INFO )'
   A=AA; B=BB
   CALL LA_PBSVX( A, B, X, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PBSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX( A, B(1:N,1), X(1:N,1), RCOND=RCOND, INFO=INFO )'
   A=AA; B=BB
   CALL LA_PBSVX( A, B(1:N,1), X(1:N,1), RCOND=RCOND, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PBSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(:,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX( A, B, X )'
   A=AA; B=BB
   CALL LA_PBSVX( A, B, X )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PBSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX(A, B(1:N,1), X(1:N,1) )'
   A=AA; B=BB
   CALL LA_PBSVX(A, B(1:N,1), X(1:N,1) )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PBSVX:'
   WRITE (NOUT,FMT) X(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX(A, B, X, ''L'', RCOND=RCOND)'
   B=BB; A = HUGE(1.0_WP)
   DO I = 1, KD+1
      A(I,1:N-I+1) = AA(KD+2-I,I:N)
      WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,1:N)
   ENDDO
   CALL LA_PBSVX( A, B, X, 'L', RCOND=RCOND)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PBSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'RCOND = ', RCOND
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX( A, B(1:N,1), X(1:N,1), ''L'', RCOND=RCOND )'
   B=BB; A = HUGE(1.0_WP)
   DO I = 1, KD+1; A(I,1:N-I+1) = AA(KD+2-I,I:N); ENDDO
   CALL LA_PBSVX( A, B(1:N,1), X(1:N,1), 'L', RCOND=RCOND )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PBSVX:'
   WRITE (NOUT,FMT) X(1:N,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX(A(:,:), B(1:N-1,:), X, INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_PBSVX( A(:,:), B(1:N-1,:), X, INFO=INFO)
   WRITE(NOUT,*)' INFO = ', INFO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX( A(4:4), B(1:N,1), X(1:N,1), INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_PBSVX( A(4:4,:), B(1:N,1), X(1:N,1), INFO=INFO )
   WRITE(NOUT,*)'   B - the RHS vector.'
   WRITE (NOUT,FMT) B(1:N,1)
   WRITE(NOUT,*)' INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX(A, B, X, FACT=''F'', INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_PBSVX( A, B, X, FACT='F', INFO=INFO)
   WRITE(NOUT,*)'   INFO = ', INFO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PBSVX( A, B(1:N,1), X(1:N,1), FACT=''4'', INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_PBSVX( A, B(1:N,1), X(1:N,1), FACT='4', INFO=INFO )
   WRITE(NOUT,*)'   INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(A, B, X, ''U'', AF, ''N'', EQU, S, FERR, BERR, RCOND, INFO)'
   A=AA; B=BB; X=HUGE(1.0_WP); EQU = 'N'
   CALL LA_PBSVX(A, B, X, 'U', AF, 'N', EQU, S, FERR, BERR, RCOND, INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(A, B(1:N,1), X(1:N,1), ''U'', AF, ''N'', EQU, S, ', &
                 'FERR, BERR, RCOND, INFO) '
   A=AA; B=BB; X=HUGE(1.0_WP); EQU = 'N'
   CALL LA_PBSVX(A, B(1:N,1), X(1:N,1), 'U', AF, 'N', EQU, S, FERR(1), BERR(1), RCOND, INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(1:N,1)
!
END PROGRAM LA_ZPBSVX_ET_EXAMPLE
