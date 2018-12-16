      PROGRAM LA_SGESVD_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GESVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, M, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), S(:), U(:,:), V(:,:), VT(:,:), WW(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GESVD Example Program Results'
      N = 5; M=3
      ALLOCATE( A(M,N), AA(M,N), S(MIN(M,N)), U(M,M), V(N,N), VT(N,N), WW(MIN(M,N)-1)) 

      OPEN(UNIT=21,FILE='gesvd.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,M 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,M; WRITE(*,"(5(I3,1X))") INT(A(I,:)); ENDDO
 
      WRITE(*,*)
      WRITE(*,*) 'CALL LA_GESVD( A, S )'
!      CALL LA_GESVD( A, S )
     
      WRITE(*,*) 'S on exit : '
      WRITE(*,"(3(F10.5,1X))") S(:)
     
      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '
      WRITE(*,*) "CALL LA_GESVD( A, S, VT=VT, WW=WW, JOB='U', INFO=INFO )"
      CALL LA_GESVD( AA, S, VT=VT, WW=WW, JOB='U', INFO=INFO )
      
      WRITE(*,*) 'A on exit : '
      DO I=1,M; 
         WRITE(*,"(5(E14.6,1X))") AA(I,:); 
      ENDDO
 
      WRITE(*,*) 'VT on exit : '
      DO I=1,N; 
         WRITE(*,"(5(E14.6,1X))") VT(I,:); 
      ENDDO
    
      WRITE(*,*)'WW on exit : '
      WRITE(*,"(5(E14.6,1X))") WW(:)

      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_SGESVD_EXAMPLE
