      PROGRAM LA_SSPSV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements"
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SPSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, N, NN, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: B(:,:), AP(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SSPSV Example Program Results.'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      NN = N*(N+1)/2
      ALLOCATE ( AP(NN), B(N,NRHS), IPIV(N) )
   
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

      WRITE(*,*)'Matrix B :'
      DO I=1,N; WRITE(*,"(10(I3,1X,1X),I3,1X)')") INT(B(I,1));
      ENDDO

      WRITE(*,*)" CALL LA_SPSV( AP, B, 'L', IPIV )"

      CALL LA_SPSV( AP, B, 'L', IPIV )

      WRITE(*,*)'AP on exit: '
      DO I=1,NN; WRITE(*,"(15(E13.5))") AP(I); 
      ENDDO
      
      WRITE(*,*)'Matrix B on exit :'
      DO I=1,N; WRITE(*,"(F9.5)") B(I,1);
      ENDDO
      WRITE(*,*)'IPIV = ', IPIV
          
      END PROGRAM LA_SSPSV_EXAMPLE


