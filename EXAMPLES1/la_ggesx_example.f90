      PROGRAM LA_SGGESX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GGESX
!  .. "Local Scalars" ..
      INTEGER :: I, J, N
      INTERFACE
         LOGICAL FUNCTION SELECT(X, Y, Z)
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT(IN) :: X, Y, Z 
         END FUNCTION SELECT
      END INTERFACE
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), RCONDE(:), RCONDV(:), VSL(:,:)
      REAL(WP), ALLOCATABLE :: ALPHAR(:), ALPHAI(:), BETA(:), VSR(:,:)  
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GGESX Example Program Results'
      N = 5
      ALLOCATE( A(N,N),  B(N,N), ALPHAR(N), ALPHAI(N), BETA(N), &
                VSL(N,N), VSR(N,N), RCONDE(2), RCONDV(2) )

      OPEN(UNIT=21,FILE='gges.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A : '
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(A(I,:)); 
      ENDDO

      OPEN(UNIT=21,FILE='gges.mb',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix B : '
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(B(I,:)); 
      ENDDO
   
      WRITE(*,*)
      WRITE(*,*) 'CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, SELECT=SELECT,', &
                               ' RCONDE=RCONDE, RCONDV=RCONDV )'
      CALL LA_GGESX( A, B, ALPHAR, ALPHAI, BETA, SELECT=SELECT, &
                     RCONDE=RCONDE, RCONDV=RCONDV )
      
      WRITE(*,*)
      WRITE(*,*)'RCONDE = ', RCONDE
      WRITE(*,*)'RCONDV = ', RCONDV

      END PROGRAM LA_SGGESX_EXAMPLE
! CONTAINS
    LOGICAL FUNCTION SELECT( ALPHAR, ALPHAI, BETA )
      USE LA_PRECISION, ONLY: WP => SP
      REAL(WP), INTENT(IN) :: ALPHAR, ALPHAI, BETA
      INTRINSIC EPSILON, ABS
!
      IF ( ABS(BETA) > EPSILON(1.0_WP) ) THEN
         IF ( ABS(ALPHAI/BETA) < 3.0_WP) THEN
            SELECT = .TRUE.
         ELSE
            SELECT = .FALSE.
         ENDIF
      ELSE
         SELECT = .FALSE.
      ENDIF

    END FUNCTION SELECT
