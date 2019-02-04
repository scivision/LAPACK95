PROGRAM LA_ZGGESX_ET_EXAMPLE
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
      LOGICAL FUNCTION SELECT(ALPHA, BETA)
      USE LA_PRECISION, ONLY: WP => DP
      COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
      END FUNCTION SELECT
      END INTERFACE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
      INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
      INTEGER :: I, INFO, N, SDIM
!  .. LOCAL ARRAYS ..
      REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
      COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:), VSL(:,:), VSR(:,:), DUMMY(:,:)
      COMPLEX(WP), ALLOCATABLE :: ALPHA(:), BETA(:)
      REAL(WP), ALLOCATABLE :: RCONDE(:), RCONDV(:)
!  .. EXECUTABLE STATEMENTS ..
      WRITE (NOUT,*) 'GGESX ET_Example Program Results.'
      READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
      READ ( NIN, * ) N
      PRINT *, 'N = ', N
      ALLOCATE ( A(N,N), B(N,N), AA(N,N), BB(N,N), VSL(N,N), VSR(N,N) )
      ALLOCATE ( ALPHA(N), BETA(N))
      ALLOCATE(RCONDE(2), RCONDV(2))
!
      READ (NIN, *) AA, BB
      A=AA
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
!
        WRITE ( NOUT, * )'---------------------------------------------------------'
        WRITE ( NOUT, * )
        WRITE ( NOUT, * )'Details of LA_ZGGESX LAPACK Subroutine Results.'
        WRITE ( NOUT, * )
!
        WRITE(NOUT,*)
        WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA, BETA, VSL, VSR, SELECT, SDIM, RCONDE, RCONDV, INFO )'
        A=AA
        CALL LA_GGESX( A, B, ALPHA, BETA, VSL, VSR, SELECT, SDIM, RCONDE, RCONDV, INFO )
        WRITE(NOUT,*) 'INFO = ', INFO, ' SDIM = ', SDIM, ' Eigenvalues:'
        WRITE(NOUT,FMT) ALPHA
        WRITE(NOUT,*) 'Left Schur vectors:'
        DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSL(:,I); END DO
          WRITE(NOUT,*) 'Right Schur vectors:'
          DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSR(:,I); END DO
            WRITE(NOUT,*) 'RCONDE = :'; WRITE(NOUT,FMT) RCONDE
            WRITE(NOUT,*) 'RCONDV = :'; WRITE(NOUT,FMT) RCONDV            
!
            WRITE(NOUT,*)
            WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA, BETA, VSL, VSR )'
            A=AA; VSL = HUGE(1.0_WP); VSR = HUGE(1.0_WP)
            CALL LA_GGESX( A, B, ALPHA, BETA, VSL, VSR)
            WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
            WRITE(NOUT,FMT)  ALPHA 
            WRITE(NOUT,*) 'Left Schur vectors:'
            DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSL(:,I); END DO
              WRITE(NOUT,*) 'Right Schur vectors:'
              DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VSR(:,I); END DO        
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA, BETA, SELECT=SELECT, SDIM=SDIM, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHA, BETA, SELECT=SELECT, SDIM=SDIM, INFO=INFO )
                WRITE(NOUT,*) 'INFO = ', INFO, ' SDIM = ', SDIM, ' Eigenvalues:'
                WRITE(NOUT,FMT) ALPHA
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B,  ALPHA, BETA )'
                A=AA
                CALL LA_GGESX( A, B, ALPHA, BETA)
                WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
                WRITE(NOUT,FMT) ALPHA
! START THE ERROR TESTS 
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( DUMMY, B, ALPHA, BETA, &
     &            SELECT=SELECT, SDIM=SDIM, RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( DUMMY, B, ALPHA, BETA, SELECT=SELECT, SDIM=SDIM, RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A,B(1:N-1,:), ALPHA, BETA, SELECT=SELECT, SDIM=SDIM, &
     &            RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B(1:N-1,:), ALPHA, BETA, SELECT=SELECT, SDIM=SDIM, &
     &            RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A,B(:,1:N-1), ALPHA, BETA, SELECT=SELECT, SDIM=SDIM, &
     &            RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B(:,1:N-1), ALPHA, BETA, SELECT=SELECT, SDIM=SDIM, &
     &            RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )  
                WRITE(NOUT,*) 'INFO = ', INFO                
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA(1:N-1), BETA, SELECT=SELECT, SDIM=SDIM, &
     &            RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHA(1 :N-1), BETA, SELECT=SELECT, SDIM=SDIM, RCONDE=RCONDE, &
     &            RCONDV=RCONDV, INFO=INFO)
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA, BETA(1:N-1), SELECT=SELECT, SDIM=SDIM,&
     &            RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHA,BETA(1:N-1), SELECT=SELECT, SDIM=SDIM, RCONDE=RCONDE,&
     &            RCONDV=RCONDV, INFO=INFO)
                WRITE(NOUT,*) 'INFO = ', INFO                
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA, BETA, VSL(1:N-1,:), VSR, SELECT, &
     &            RCONDE, RCONDV, SDIM, INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHA, BETA, VSL(1:N-1,:), VSR, SELECT, SDIM, RCONDE, RCONDV, INFO)
                WRITE(NOUT,*) 'INFO = ', INFO
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA, BETA, VSL(:,1:N-1), VSR, SELECT, SDIM, &
     &            RCONDE, RCONDV, INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHA, BETA, VSL(:,1:N-1), VSR, SELECT, SDIM, RCONDE, RCONDV, INFO)
                WRITE(NOUT,*) 'INFO = ', INFO                
!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA, BETA, VSL, VSR(1:N-1,:), SELECT, SDIM, &
     &            RCONDE, RCONDV, INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHA, BETA, VSL, VSR(1:N-1,:), SELECT, SDIM, RCONDE, RCONDV, INFO)
                WRITE(NOUT,*) 'INFO = ', INFO

!
                WRITE(NOUT,*)
                WRITE(NOUT,*) 'CALL LA_GGESX( A, B, ALPHA, BETA, VSL, VSR(:,1:N-1), SELECT, SDIM,&
     &            RCONDE, RCONDV, INFO )'
                A=AA
                CALL LA_GGESX( A, B, ALPHA, BETA, VSL, VSR(:,1:N-1), SELECT, SDIM, RCONDE, RCONDV, INFO)
                WRITE(NOUT,*) 'INFO = ', INFO                
END PROGRAM LA_ZGGESX_ET_EXAMPLE
                LOGICAL FUNCTION SELECT(ALPHA, BETA)
                USE LA_PRECISION, ONLY: WP => DP
                COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
                SELECT = .TRUE.     
              END FUNCTION SELECT
            

            
