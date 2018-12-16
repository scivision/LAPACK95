      PROGRAM LA_SSYEVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), W(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SYEVX Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), W(N) )

      OPEN(UNIT=21,FILE='syev.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,N; 
         WRITE(*,"(5(F9.5))") A(I,:); 
      ENDDO
   
      WRITE(*,*) 'CALL LA_SYEVX( A, W, VL=-7.0_WP, VU=7.0_WP, M=M, ABSTOL=1.0E-4_WP )'
      CALL LA_SYEVX( A, W, VL=-7.0_WP, VU=7.0_WP, M=M, ABSTOL=1.0E-4_WP )
      WRITE(*,*) 'W on exit'
      DO I=1,N
      WRITE(*,"(5(F9.5))") W(I)
      ENDDO
      WRITE(*,*)' M =',M

      END PROGRAM LA_SSYEVX_EXAMPLE
