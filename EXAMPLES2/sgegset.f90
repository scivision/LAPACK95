PROGRAM LA_SGEGS_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GEGS
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, N
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
   REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), VSL(:,:), VSR(:,:), &
                ALPHAR(:), ALPHAI(:), BETA(:), DUMMY(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'SGEGS ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   ALLOCATE ( A(N,N), AA(N,N), ALPHAR(N), ALPHAI(N), BETA(N), &
              VSL(N,N), VSR(N,N), B(N,N), BB(N,N) )
!
   READ (NIN, *) AA, BB
   A=AA; B=BB
   WRITE(NOUT,*) 'The matrix A:'
   DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
   WRITE(NOUT,*) 'The matrix B:'
   DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) B(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_SGEGS LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR, INFO )'
   A=AA
   CALL LA_GEGS( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
   WRITE(NOUT,*) ' Alphar:'
   WRITE(NOUT,FMT) ALPHAR
   WRITE(NOUT,*) ' Alphai:'
   WRITE(NOUT,FMT) ALPHAI
   WRITE(NOUT,*) ' Beta:'
   WRITE(NOUT,FMT) BETA
   WRITE(NOUT,*) 'VSL:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSL(:,I); END DO
   WRITE(NOUT,*) 'VSR:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, B, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
   WRITE(NOUT,*) 'A:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(:,I); END DO
   WRITE(NOUT,*) 'B:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) B(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( DUMMY, B, INFO=INFO )'
   A=AA
   CALL LA_GEGS( DUMMY, B, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, DUMMY, INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, DUMMY, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, ALPHAR(1:N-1), INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, B, ALPHAR(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, ALPHAI=ALPHAI(1:N-1), INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, B, ALPHAI=ALPHAI(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, BETA=ALPHAI(1:N-1), INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, B, BETA=ALPHAI(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, VSL=VSL(:,1:N-1), INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, B, VSL=VSL(:,1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, VSL=VSL(1:N-1,:), INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, B, VSL=VSL(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, VSR=VSR(1:N-1,:), INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, B, VSR=VSR(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEGS( A, B, VSR=VSR(:,1:N-1), INFO=INFO )'
   A=AA
   CALL LA_GEGS( A, B, VSR=VSR(:,1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
END PROGRAM LA_SGEGS_ET_EXAMPLE
