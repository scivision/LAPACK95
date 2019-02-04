PROGRAM LA_DGGESX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_GGESX
!  .. IMPLICIT STATEMENT ..
      IMPLICIT NONE
      INTERFACE
      LOGICAL FUNCTION SELECT(ALPHAR, ALPHAI, BETA)
      USE LA_PRECISION, ONLY: WP => DP
      REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
      END FUNCTION SELECT
      END INTERFACE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
      INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
      INTEGER :: I, INFO, N, SDIM
!  .. LOCAL ARRAYS ..
      REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), VSL(:,:), VSR(:,:), DUMMY(:,:)
      REAL(WP), ALLOCATABLE ::  ALPHAR(:), ALPHAI(:), BETA(:)
      REAL(WP) RCONDE(2), RCONDV(2)
!  .. EXECUTABLE STATEMENTS ..
      WRITE (NOUT,*) 'GGESX ET_Example Program Results.'
      READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
      READ ( NIN, * ) N
      PRINT *, 'N = ', N
      ALLOCATE ( A(N,N), B(N,N), AA(N,N), BB(N,N), VSL(N,N), VSR(N,N) )
      ALLOCATE (ALPHAI(N), ALPHAR(N), BETA(N))
!
      READ (NIN, *) AA
      A=AA
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
!
        WRITE ( NOUT, * )'---------------------------------------------------------'
        WRITE ( NOUT, * )
        WRITE ( NOUT, * )'Details of LA_DGGESX LAPACK Subroutine Results.'
        WRITE ( NOUT, * )
!
        WRITE(NOUT,*)
        WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR, SELECT, SDIM, RCONDE, RCONDV, INFO )'
        A=AA
        CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR, SELECT, SDIM, RCONDE, RCONDV, INFO )
        WRITE(NOUT,*) 'INFO = ', INFO, ' SDIM = ', SDIM, ' Eigenvalues:'
        WRITE(NOUT,FMT) ALPHAR
        WRITE(NOUT,FMT) ALPHAI
        WRITE(NOUT,*) 'Left Schur vectors:'
        DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSL(:,I); END DO
          WRITE(NOUT,*) 'Right Schur vectors:'
          DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSR(:,I); END DO
          WRITE(NOUT,*) 'RCONDE = :'; WRITE(NOUT,FMT) RCONDE
          WRITE(NOUT,*) 'RCONDV = :'; WRITE(NOUT,FMT) RCONDV
!
            WRITE(NOUT,*)
            WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR )'
            A=AA; VSL = HUGE(1.0_WP); VSR = HUGE(1.0_WP)
            CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR)
            WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
            WRITE(NOUT,FMT)  ALPHAR 
            WRITE(NOUT,FMT)  ALPHAI
            WRITE(NOUT,*) 'Left Schur vectors:'
            DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSL(:,I); END DO
              WRITE(NOUT,*) 'Right Schur vectors:'
              DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSR(:,I); END DO        
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, INFO=INFO )
                WRITE(NOUT,*) 'INFO = ', INFO, ' SDIM = ', SDIM, ' Eigenvalues:'
                WRITE(NOUT,FMT) ALPHAR
                WRITE(NOUT,FMT) ALPHAI
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B,  ALPHAR, ALPHAI, BETA )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA)
                WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
                WRITE(NOUT,FMT) ALPHAR
                WRITE(NOUT,FMT) ALPHAI
! START THE ERROR TESTS 
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( DUMMY, B, ALPHAR, ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV,  INFO=INFO )'
                A=AA
                CALL LA_GGESX( DUMMY, B, ALPHAR, ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM,  RCONDE=RCONDE, &
&                 RCONDV=RCONDV, INFO=INFO )
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A,B(1:N-1,:), ALPHAR, ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B(1:N-1,:), ALPHAR, ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A,B(:,1:N-1), ALPHAR, ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B(:,1:N-1), ALPHAR, ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )
                WRITE(NOUT,*) 'INFO = ', INFO                
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR(1:N-1), ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR(1 :N-1), ALPHAI, BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO)
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI(1:N-1), BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV,INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR, ALPHAI(1:N-1), BETA, SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO)
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA(1:N-1), SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA(1:N-1), SELECT=SELECT, SDIM=SDIM, &
&                 RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO)
                WRITE(NOUT,*) 'INFO = ', INFO                
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL(1:N-1,:), VSR, SELECT, &
&                 SDIM, RCONDE, RCONDV, INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL(1:N-1,:), VSR, SELECT, &
&                 SDIM, RCONDE, RCONDV, INFO)
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL(:,1:N-1), VSR, SELECT, SDIM, &
&                 RCONDE, RCONDV,  INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL(:,1:N-1), VSR, SELECT, SDIM, &
&                 RCONDE, RCONDV, INFO)
                WRITE(NOUT,*) 'INFO = ', INFO                
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR(1:N-1,:), SELECT, SDIM, &
&                 RCONDE, RCONDV, INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR(1:N-1,:), SELECT, SDIM, &
&                 RCONDE, RCONDV,  INFO)
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR(:,1:N-1), SELECT, SDIM, &
&                 RCONDE, RCONDV,  INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, VSL, VSR(:,1:N-1), SELECT, SDIM, &
&                 RCONDE, RCONDV, INFO)
                WRITE(NOUT,*) 'INFO = ', INFO                
END PROGRAM LA_DGGESX_ET_EXAMPLE
                LOGICAL FUNCTION SELECT(ALPHAR, ALPHAI, BETA)
                USE LA_PRECISION, ONLY: WP => DP
                REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
                SELECT = .TRUE.     
              END FUNCTION SELECT
            

            
