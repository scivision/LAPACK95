      PROGRAM LA_SSYGVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYGVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IFAIL(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), W(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SYEVX Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), W(N), IFAIL(N), Z(N,N) )

      OPEN(UNIT=21,FILE='sygv.ma',STATUS='UNKNOWN')
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
    
      OPEN(UNIT=21,FILE='sygv.mb',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      BB=B

      WRITE(*,*)'Matrix B:'
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(B(I,:)); 
      ENDDO
  
      WRITE(*,*) 'CALL LA_SYGVX( A, B, W, 3, VL=-10.0_WP, VU=10.0_WP, M=M, IFAIL=IFAIL )'
      CALL LA_SYGVX(  A, B, W, 3, VL=-10.0_WP, VU=10.0_WP, M=M, IFAIL=IFAIL )
    
      WRITE(*,*)'Matrix B on exit:'
      DO I=1,N; 
         WRITE(*,"(5(F12.7,1X))") B(I,:); 
      ENDDO
        
      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(F12.7,1X))") W(I)
      ENDDO

      WRITE(*,*) 'M=',M

      WRITE(*,*)'IFAIL = ', IFAIL
        
      WRITE(*,*) 'Z on exit : '
      DO J=1,M
      WRITE(*,"(5(F12.6,1X))") (A(I,J), I = 1,N)
      ENDDO

      END PROGRAM LA_SSYGVX_EXAMPLE


