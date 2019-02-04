      PROGRAM LA_SSYEV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYEV, LA_SYEVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), W(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SYEV Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), W(N) )

      OPEN(UNIT=21,FILE='syev.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO    
      CLOSE(21)
    
      AA=TRANSPOSE(A)

      WRITE(*,*)'Matrix A:'
      DO I=1,N; 
         WRITE(*,"(5(F9.5))") A(I,:); 
      ENDDO
    
      WRITE(*,*) 'CALL LA_SYEV( A, W )'
      CALL LA_SYEV(  A, W)
      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(F10.5))") W(I)
      ENDDO

      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '

      WRITE(*,*)'Matrix A:'
      DO I=1,N; 
         WRITE(*,"(5(F9.5))") AA(I,:); 
      ENDDO

      WRITE(*,*) "CALL LA_SYEVD( A, W, 'V', 'L', INFO )"
      CALL LA_SYEVD( AA, W, 'V', 'L', INFO )

      WRITE(*,*) 'A on exit : '
      DO I=1,N; 
         WRITE(*,"(5(E14.6))") AA(I,:); 
      ENDDO

      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(F10.5))") W(I)
      ENDDO

      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_SSYEV_EXAMPLE
