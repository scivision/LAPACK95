      PROGRAM LA_SSYEVR_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYEVR
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N, IL, IU
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE ::  ISUPPZ(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), W(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SYEVR Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), W(N), Z(N,N), ISUPPZ(2*N)  )

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

      WRITE(*,*) 'CALL LA_SYEVR( A, W, Z=Z, ISUPPZ=ISUPPZ, IL=1, IU=2, M=M )'
      IL=1; IU=2

      CALL LA_SYEVR( A, W, IL=1, IU=2, M=M )

      WRITE(*,*)'Matrix A on exit :'
      DO I=1,N; 
         WRITE(*,"(5(F9.5))") A(I,:); 
      ENDDO


      WRITE(*,*)'M = ', M
      WRITE(*,*) 'W on exit :'
      DO I=1,M
         WRITE(*,"(5(F9.5))") W(I)
      ENDDO
      
      WRITE(*,*)'Z on exit :'
      DO I=1,N
         WRITE(*,"(5(E14.6))") Z(I,1:M)
      ENDDO
      WRITE(*,*)'ISUPPZ on exit:'
      WRITE(*,*) ISUPPZ(1:2*max(1,m))

      END PROGRAM LA_SSYEVR_EXAMPLE
