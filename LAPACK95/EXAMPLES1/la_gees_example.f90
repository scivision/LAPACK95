      PROGRAM LA_SGEES_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GEES
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, SDIM
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), WR(:), WI(:), VS(:,:)
!  .. "Intrinsic Functions" ..
      INTERFACE
         LOGICAL FUNCTION SELECT( X, Y )
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT(IN) :: X, Y
         END FUNCTION SELECT
      END INTERFACE
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GEES Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), VS(N,N), WR(N), WI(N) )

      OPEN(UNIT=21,FILE='gees.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(A(I,:)); 
      ENDDO
         
      WRITE(*,*) 'CALL LA_GEES( A, WR, WI )'
      CALL LA_GEES( A, WR, WI )

      WRITE(*,*)'Matrix A on exit :' 
      DO I=1,N; 
         WRITE(*,"(5(F12.5,1X))") A(I,:); 
      ENDDO
    
      WRITE(*,*) 'WR on exit :'
      DO I=1,N; 
         WRITE(*,"(5(F12.5,1X))") WR(I)
      ENDDO
    
      WRITE(*,*) 'WI on exit :'
      DO I=1,N; 
      WRITE(*,"(5(F12.5,1X))") WI(I)
      ENDDO

      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '

      WRITE(*,*) "CALL LA_GEES( A, WR, WI, VS, SELECT, SDIM, INFO )"
      CALL LA_GEES( AA, WR, WI, VS, SELECT, SDIM, INFO )
       
      WRITE(*,*)'Matrix A on exit :' 
      DO I=1,N; 
         WRITE(*,"(5(F12.5,1X))") AA(I,:); 
      ENDDO
   
      WRITE(*,*) 'WR on exit :'
      DO I=1,N; 
         WRITE(*,"(5(F12.5,1X))") WR(I)
      ENDDO
    
      WRITE(*,*) 'WI on exit :'
      DO I=1,N; 
      WRITE(*,"(5(F12.5,1X))") WI(I)
      ENDDO

      WRITE(*,*) 'VS on exit :'
      DO I=1,N; 
         WRITE(*,"(5(F12.7,1X))") VS(I,:); 
      ENDDO
    
      WRITE(*,*) ' SDIM = ', SDIM
      WRITE(*,*) ' INFO = ', INFO

    END PROGRAM LA_SGEES_EXAMPLE

! CONTAINS
    LOGICAL FUNCTION SELECT(X, Y )
    USE LA_PRECISION, ONLY: WP => SP
    INTRINSIC  EPSILON
      REAL(WP), INTENT(IN) :: X,Y
!      IF (ABS(Y) <= 1.1921E-7) THEN
       IF (ABS(Y) <= EPSILON(1.0_WP)) THEN
          SELECT = .TRUE.
      ELSE
         SELECT = .FALSE.
      ENDIF
   
    END FUNCTION SELECT
