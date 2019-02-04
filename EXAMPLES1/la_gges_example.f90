      PROGRAM LA_SGGES_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GGES
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, SDIM
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), VSL(:,:)
      REAL(WP), ALLOCATABLE :: ALPHAR(:), ALPHAI(:), BETA(:), VSR(:,:)
!  .. "Intrinsic Functions" ..
      INTERFACE
         LOGICAL FUNCTION SELECT( X, Y, Z )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT(IN) :: X, Y, Z
         END FUNCTION SELECT
      END INTERFACE
!  .. "Executable Statements" ..
      WRITE(*,*)'LA_GGES Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), ALPHAR(N), ALPHAI(N), BETA(N), VSL(N,N), VSR(N,N) )

      OPEN(UNIT=21,FILE='gges.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      AA=A

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

      BB=B
    
      WRITE(*,*)'Matrix B : '
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(B(I,:)); 
      ENDDO
    
      WRITE(*,*)
      WRITE(*,*) 'CALL LA_GGES( A, B, ALPHAR, ALPHAI, BETA )'
      CALL LA_GGES( A, B, ALPHAR, ALPHAI, BETA )
   
      WRITE(*,*)
      WRITE(*,*)'Matrix A on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F9.5,1X))") A(I,:); 
      ENDDO
      
      WRITE(*,*)
      WRITE(*,*)'Matrix B on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F9.5,1X))") B(I,:); 
      ENDDO
      
      WRITE(*,*)
      WRITE(*,*) 'ALPHAR on exit : '
      DO I=1,N
         WRITE(*,"(F9.5,1X)") ALPHAR(I); 
      ENDDO
      WRITE(*,*)'ALPHAI on exit : '
      DO I=1,N
         WRITE(*,"(F9.5,1X)") ALPHAI(I); 
      ENDDO
      WRITE(*,*)'BETA on exit : '
      DO I=1,N
         WRITE(*,"(F9.5,1X)") BETA(I); 
      ENDDO
      WRITE(*,*)
      WRITE(*,*)' Generalazed eigenvalues : '
      DO I=1,N
         WRITE(*,*) '(',ALPHAR(I)/BETA(I),',',ALPHAI(I)/BETA(I),')'
      ENDDO

      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '
      WRITE(*,*)
      WRITE(*,*) 'CALL LA_GGES( A, B, ALPHAR, ALPHAI, BETA, VSL, ', &
                               'VSR, SELECT, SDIM, INFO )'
      CALL LA_GGES( AA, BB, ALPHAR, ALPHAI, BETA, VSL, VSR, SELECT, &
                    SDIM, INFO )
   
      WRITE(*,*)
      WRITE(*,*)'Matrix A on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F9.6,1X))") AA(I,:); 
      ENDDO
      
      WRITE(*,*)
      WRITE(*,*)'Matrix B on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F9.6,1X))") BB(I,:); 
      ENDDO
      
      WRITE(*,*)
      WRITE(*,*) 'ALPHAR on exit : '
      WRITE(*,"(8(F9.6,1X))") ALPHAR(:); 
      WRITE(*,*)'ALPHAI on exit : '
      WRITE(*,"(8(F9.6,1X))") ALPHAI(:); 
      WRITE(*,*)'BETA on exit : '
      WRITE(*,"(8(F9.6,1X))") BETA(:); 
      WRITE(*,*)
      WRITE(*,*)' Generalazed eigenvalues : '
      DO I=1,N
         WRITE(*,*) '(',ALPHAR(I)/BETA(I),',',ALPHAI(I)/BETA(I),')'
      ENDDO

      WRITE(*,*)
      WRITE(*,*) 'VSL on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F12.8,1X))") VSL(I,:); 
      ENDDO
   
      WRITE(*,*)
      WRITE(*,*) 'VSR on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F12.8,1X))") VSR(I,:); 
      ENDDO
   
      WRITE(*,*)
      WRITE(*,*)'SDIM = ', SDIM

      WRITE(*,*)'INFO = ', INFO
      
      END PROGRAM LA_SGGES_EXAMPLE

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
