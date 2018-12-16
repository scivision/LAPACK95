      PROGRAM LA_CGEEV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     March, 2000
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GEEV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, INFO, N
      REAL(WP), ALLOCATABLE ::  WR(:), WI(:), VL(:,:), VR(:,:)
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:)
      COMPLEX(WP), ALLOCATABLE :: AA(:,:), W(:), VLC(:,:), VRC(:,:)
!  .. "Intrinsic Functions" ..
!     INTRINSIC  REAL, AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GEEV V.A. Barker Program Results'
      N = 3
      ALLOCATE( A(N,N), AA(N,N), W(N), WR(N), WI(N), VL(N,N), VR(N,N), &
                VLC(N,N), VRC(N,N) )

      A(1,1) =  0.0; A(1,2) = 1.0; A(1,3) = 0.0
      A(2,1) = -1.0; A(2,2) = 0.0; A(2,3) = 0.0
      A(3,1) =  0.0; A(3,2) = 0.0; A(3,3) = 1.0

      WRITE(*,*)'Matrix A:'
      DO I=1,N
         WRITE(*,*) A(I,:)
      ENDDO

      WRITE(*,*) "CALL LA_GEEV( A, WR, WI, VL, VR, INFO ) "
      CALL LA_GEEV( A, WR, WI, VL, VR, INFO )
     
      WRITE(*,*) ' INFO = ', INFO

      WRITE(*,*) 'WR on exit : '
      WRITE(*,*) WR(1:N)
   
      WRITE(*,*) 'WI on exit : '
      WRITE(*,*) WI(1:N)

      WRITE(*,*)'Matrix VR:'
      DO I=1,N
         WRITE(*,*) VR(I,:)
      ENDDO

      WRITE(*,*)'Matrix VL:'
      DO I=1,N
         WRITE(*,*) VL(I,:)
      ENDDO
   
      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

      AA(1,1) =  0.0; AA(1,2) = 1.0; AA(1,3) = 0.0
      AA(2,1) = -1.0; AA(2,2) = 0.0; AA(2,3) = 0.0
      AA(3,1) =  0.0; AA(3,2) = 0.0; AA(3,3) = (0.0,1.0)

      WRITE(*,*)'Matrix AA:'
      DO I=1,N
         WRITE(*,*) AA(I,:)
      ENDDO

      WRITE(*,*) "CALL LA_GEEV( AA, W, VLC, VRC, INFO ) "
      CALL LA_GEEV( AA, W, VLC, VRC, INFO )

      WRITE(*,*) ' INFO = ', INFO

      WRITE(*,*) 'W on exit : '
      WRITE(*,*) W(1:N)

      WRITE(*,*)'Matrix VRC:'
      DO I=1,N
         WRITE(*,*) VRC(I,:)
      ENDDO

      WRITE(*,*)'Matrix VLC:'
      DO I=1,N
         WRITE(*,*) VLC(I,:)
      ENDDO

      END PROGRAM LA_CGEEV_EXAMPLE
