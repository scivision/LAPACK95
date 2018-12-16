      PROGRAM LA_SSPSVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements"
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SPSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, INFO, N, NN, NRHS
!  .. Local Arrays ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: B(:,:), AP(:), X(:,:), AFP(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SSPSVX Example Program Results.'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      NN = N*(N+1)/2
      ALLOCATE ( AP(NN), B(N,NRHS), X(N,NRHS), AFP(NN), IPIV(N) )
   
      OPEN(UNIT=21,FILE='spsv.ma',STATUS='UNKNOWN')
      DO I=1,NN 
            READ(21,'(F3.0)') AP(I)
         ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix AP :'
      DO I=1,NN; WRITE(*,"(15(I3,1X,1X),I3,1X))") INT(AP(I));
      ENDDO

      OPEN(UNIT=21,FILE='spsv.mb',STATUS='UNKNOWN')
      DO I=1,N 
         READ(21,'(F3.0)') B(I,1)
      ENDDO
      CLOSE(21)

      WRITE(*,*)"CALL LA_SPSVX( AP, B, X, 'L', AFP, IPIV, INFO=INFO)"
      CALL LA_SPSVX( AP, B, X, 'L', AFP, IPIV, INFO=INFO)

      WRITE(*,*)'AFP = '
      DO I=1,NN; WRITE(*,"(F8.5)") AFP(I);
      ENDDO
      WRITE(*,*)'IPIV = ', IPIV


      END PROGRAM LA_SSPSVX_EXAMPLE
