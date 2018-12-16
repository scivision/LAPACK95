PROGRAM LA_ZHPGVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
  USE LA_PRECISION, ONLY: WP => DP
  USE F95_LAPACK, ONLY: LA_HPGVX
!  .. IMPLICIT STATEMENT ..
  IMPLICIT NONE
!  .. PARAMETERS ..
  CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
  INTEGER, PARAMETER :: NIN=5, NOUT=6
  CHARACTER(LEN=1) :: UPLO
!  .. LOCAL SCALARS ..
  INTEGER :: I, J, INFO, N, NS
  REAL(WP) :: VL, VU
  INTEGER ::  M, ITYPE, IL, IU 
!  .. LOCAL ARRAYS ..
  REAL(WP), ALLOCATABLE :: AA(:), BB(:), W(:)
  COMPLEX(WP), ALLOCATABLE :: A(:), B(:), Z(:,:)
  INTEGER, ALLOCATABLE :: IFAIL(:)   
!  .. EXECUTABLE STATEMENTS ..
  WRITE (NOUT,*) 'ZHPGVX ET_Example Program Results.'
  READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
  READ ( NIN, * ) N
  PRINT *, 'N = ', N
  NS = N*(N+1)/2
  ALLOCATE ( A(NS), AA(NS), B(NS), BB(NS), W(N), Z(N,N), IFAIL(N) )
!
  READ (NIN, *) AA, BB
  A=AA; B=BB
  WRITE(NOUT,*) 'The matrix A:'
  DO I = 1, N
    WRITE (NOUT,*) 'J = ', I; WRITE (NOUT,FMT) (A(J+(I-1)*I/2),J=1,I)
  ENDDO
  WRITE(NOUT,*) 'The matrix B:'
  DO I = 1, N
    WRITE (NOUT,*) 'J = ', I; WRITE (NOUT,FMT) (B(J+(I-1)*I/2),J=1,I)
  ENDDO
  ITYPE = 1; UPLO = 'L'; IL=1 ; IU=N; VL=-10; VU=10
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_ZHPGVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   UPLO = 'U'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, 1, ''U'', Z, INFO=INFO )'
   A=AA; B=BB
   CALL LA_HPGVX( A, B, W, 1, UPLO, Z, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
   WRITE(NOUT,*) 'INFO = ', INFO
   WRITE(NOUT,*) 'The orthonormal eigenvectors of the matrix A stored in Z'
   DO I = 1, N
     WRITE(NOUT,FMT) Z(I,:)
   END DO    
!
   UPLO = 'U'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, 2, ''U'', Z , INFO = INFO)'
   A=AA; B=BB
   CALL LA_HPGVX( A, B, W, 2, UPLO, Z, INFO = INFO )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
     WRITE(NOUT,*) 'INFO = ', INFO
!
        WRITE(NOUT,*)
        WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, 3, Z=Z, INFO=INFO )'
        A=AA; B=BB
        CALL LA_HPGVX( A, B, W, 3, Z=Z, INFO=INFO )
        WRITE(NOUT,*) ' EIGENVALUES:'
        WRITE(NOUT,FMT) W
        WRITE(NOUT,*) 'EIGENVECTORS:'
        DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
        WRITE(NOUT,*) 'INFO = ', INFO
!
        UPLO= 'L'
        WRITE(NOUT,*)
        WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, 1, Z, VL, VU, INFO=INFO )'
        A=AA; B=BB; W = HUGE(1.0_WP); Z = HUGE(1.0_WP)
        CALL LA_HPGVX( A, B, W, 1, Z=Z, VL=VL, VU=VU, INFO=INFO )
        WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
        WRITE(NOUT,FMT) W
        WRITE(NOUT,*) 'EIGENVECTORS:'
        DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
        WRITE(NOUT, *) 'The lower bound of the interval to be searched for eigenvalues VL= ', VL
        WRITE(NOUT, *) 'The upper bound of the interval to be searched for eigenvalues VU= ', VU  
!
        UPLO= 'L'
        WRITE(NOUT,*)
        WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, 1, Z, IL=IL, IU=IU, INFO=INFO )'
        A=AA; B=BB; W = HUGE(1.0_WP); Z = HUGE(1.0_WP)
        CALL LA_HPGVX( A, B, W, 1, Z=Z, IL=IL, IU=IU, INFO=INFO )
        WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
        WRITE(NOUT,FMT) W
        WRITE(NOUT,*) 'EIGENVECTORS:'
        DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
        WRITE(NOUT, *) 'The indices of the smallest eigenvalue to be returned: IL= ', IL
        WRITE(NOUT, *) 'The indices of the largest eigenvalue to be returned: IU= ', IU
!
        UPLO= 'U'
        WRITE(NOUT,*)
        WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, 1, UPLO=''U'', Z, IL=IL, IU=IU, M=M, &
     &    IFAIL=IFAIL, INFO=INFO )'
        A=AA; B=BB; W = HUGE(1.0_WP); Z = HUGE(1.0_WP)
        CALL LA_HPGVX( A, B, W, 1, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL,INFO=INFO )
        WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
        WRITE(NOUT,FMT) W
        WRITE(NOUT,*) 'EIGENVECTORS:'
        DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
        WRITE(NOUT, *) 'The indices of the smallest eigenvalue to be returned: IL= ', IL
        WRITE(NOUT, *) 'The indices of the largest eigenvalue to be returned: IU= ', IU
        WRITE(NOUT,*) ' IFAIL  computed by LA_HPGVX:'
        WRITE(NOUT, *) IFAIL(:)
        WRITE(NOUT, *) 'The total number of eigenvalues found ', M 
! STARTING THE ERROR TESTS
!ERROR 1        
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPGVX( A(1:5), B, W, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HPGVX( A(1:5), B, W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!ERROR 2
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPGVX( A, B(1:5), W, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HPGVX( A, B(1:5), W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!ERROR 3
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W(1:N-1), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_HPGVX( A, B, W(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!ERROR 4
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, 5, INFO=INFO )'
      A=AA; B=BB; Z = HUGE(1.0_WP)
      CALL LA_HPGVX( A, B, W, 5, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
!ERROR 5 
      UPLO = '9'
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, UPLO=''9'', INFO=INFO )'
      A=AA; B=BB; Z = HUGE(1.0_WP)
      CALL LA_HPGVX( A, B, W, UPLO=UPLO, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
      UPLO = 'U'
!ERROR 6 
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, Z=Z(1:N-1,:), INFO=INFO )'
      A=AA; B=BB; Z = HUGE(1.0_WP)
      CALL LA_HPGVX( A, B, W, Z=Z(1:N-1,:), INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
!ERROR 6 
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, Z=Z(:,1:N-1), INFO=INFO )'
      A=AA; B=BB; Z = HUGE(1.0_WP)
      CALL LA_HPGVX( A, B, W, Z=Z(:,1:N-1), INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO      
!ERROR 7
      VL = 10; VU = -10
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL, INFO=INFO )'
      A=AA; B=BB; Z = HUGE(1.0_WP)
      CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO 
!ERROR 8
      VL = 10; VU=-10; IL = 1; IU = N
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, VL, VU, IL, IU, M, IFAIL, INFO=INFO )'
      A=AA; B=BB; Z = HUGE(1.0_WP)
      CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, VL, VU, IL, IU, M, IFAIL, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO 
!ERROR 9
      IL=1; IU=N+1
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL, INFO=INFO )'
      A=AA; B=BB; Z = HUGE(1.0_WP)
      CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO 
!ERROR 10
      IL=1; IU=N+1
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL, INFO=INFO )'
      A=AA; B=BB; Z = HUGE(1.0_WP)
      CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, IL=IL, IU=IU, M=M, IFAIL=IFAIL, INFO=INFO  )
      WRITE(NOUT,*) 'INFO = ', INFO 
! ERROR 12
      VL=-10; VU=10
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL(1:N-1), INFO=INFO )'
      A=AA; B=BB;
      CALL LA_HPGVX( A, B, W, ITYPE, UPLO, Z, VL, VU, M=M, IFAIL=IFAIL(1:N-1), INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO 
END PROGRAM LA_ZHPGVX_ET_EXAMPLE

