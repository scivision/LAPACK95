      PROGRAM LA_CGEEV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GEEV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
      REAL(WP), ALLOCATABLE ::  WR(:), WI(:)
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:)
      COMPLEX(WP), ALLOCATABLE :: AA(:,:), W(:), VL(:,:), VR(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC  REAL, AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GEEV Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), W(N), WR(N), WI(N), VL(N,N), VR(N,N) )

      OPEN(UNIT=21,FILE='geev1.ma',STATUS='UNKNOWN')
      DO I=1,N
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,N
         WRITE(*,"(5(I3,1X))") INT(A(I,:))
      ENDDO

      WRITE(*,*) "CALL LA_GEEV( A, WR, WI ) "
      CALL LA_GEEV( A, WR, WI )
     
      WRITE(*,*) 'WR on exit : '
      DO I=1,N
         WRITE(*,"((F9.6,1X))") REAL(WR(I))
      ENDDO
   
      WRITE(*,*) 'WI on exit : '
      DO I=1,N
         WRITE(*,"((F9.5,1X))") REAL(WI(I))
      ENDDO
   
      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

      OPEN(UNIT=21,FILE='geev.ma',STATUS='UNKNOWN')
      DO I=1,N
         DO J=1,N
            READ(21,*) AA(I,J)
         ENDDO
      ENDDO
      CLOSE(21)
  
      WRITE(*,*)'Matrix A : '
      DO J=1,N      
         WRITE(*,"(5(I3,1X,'+',I3,'i',1X,1X,1X))") INT(AA(J,1)),INT(AIMAG(AA(J,1))), &
              INT(AA(J,2)),INT(AIMAG(AA(J,2))), &  
              INT(AA(J,3)),INT(AIMAG(AA(J,3))), & 
              INT(AA(J,4)),INT(AIMAG(AA(J,4))), & 
              INT(AA(J,5)),INT(AIMAG(AA(J,5))) 
      ENDDO

      WRITE(*,*) "CALL LA_GEEV( A, W, VL, VR, INFO )"
      CALL LA_GEEV( AA, W, VL, VR, INFO )
      
      WRITE(*,*) ' W on exit : '
      DO I=1,N;
         WRITE(*,"('('E14.7,1X,',',1X,E14.7,')',1X)")  W(I) 
      ENDDO
 
      WRITE(*,*)' VL on exit : '
      DO I=1,N;
         WRITE(*,"(5('('E14.7,1X,',',1X,E14.7,')',1X))")  VL(I,:)
      ENDDO;
      
      WRITE(*,*) ' VR on exit : '
      DO I=1,N;
         WRITE(*,"(5('('E14.7,1X,',',1X,E14.7,')',1X))")  VR(I,:)
      ENDDO;

      WRITE(*,*) ' INFO = ', INFO
      DEALLOCATE( A, AA, VL, VR )

      END PROGRAM LA_CGEEV_EXAMPLE






