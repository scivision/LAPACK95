      PROGRAM LA_CGGEVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GGEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N
      REAL :: ABNRM, BBNRM
!  .. "Local Arrays" ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
      CHARACTER(LEN=1) :: BALANC
      COMPLEX(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), ALPHA(:)
      COMPLEX(WP), ALLOCATABLE :: BETA(:), LAMBDA(:)
      REAL(WP), ALLOCATABLE :: LSCALE(:), RSCALE(:), RCONDE(:), RCONDV(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GGEVX Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), ALPHA(N), BETA(N), LSCALE(N), &
&       RSCALE(N), RCONDE(N), RCONDV(N), LAMBDA(N) )

      OPEN(UNIT=21,FILE='ggev.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      AA=A
    
      WRITE(*,*)'Matrix A : '
      DO I=1,N 
         WRITE(*,"(5('('(I3,1X,',',I3)')',1X,1X))") INT(A(I,:)), INT(AIMAG(A(I,:)))
      ENDDO
    
      OPEN(UNIT=21,FILE='ggev.mb',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      BB=B
      WRITE(*,*)'Matrix B : '
      DO I=1,N 
         WRITE(*,"(5('('(I3,1X,',',I3)')',1X,1X))") INT(B(I,:)), INT(AIMAG(B(I,:)))
      ENDDO
      
      WRITE(*,*)
      WRITE(*,*) "CALL LA_GGEVX( A, B, BALANC='B', LSCALE=LSCALE, RSCALE=RSCALE,"
      WRITE(*,*)"ABNRM=ABNRM, BBNRM=BBNRM, RCONDE=RCONDE, RCONDV=RCONDV )"
      BALANC = 'B'
      CALL LA_GGEVX( A, B, ALPHA, BETA, BALANC=BALANC, LSCALE=LSCALE, RSCALE=RSCALE, ABNRM=ABNRM, &
           BBNRM=BBNRM, RCONDE=RCONDE, RCONDV=RCONDV )
   
      WRITE(*,*)
      WRITE(*,*)'LSCALE : '
      DO I=1,N
         WRITE(*,'(F9.5)') LSCALE(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'RSCALE : '
      DO I=1,N
         WRITE(*,'(F9.5)') RSCALE(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'ABNRM = ', ABNRM
      WRITE(*,*)
      WRITE(*,*)'BBNRM = ', BBNRM
      WRITE(*,*)
      WRITE(*,*)'RCONDE : '
      DO I=1,N
         WRITE(*,'(F9.5)') RCONDE(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'RCONDV : '
      DO I=1,N
         WRITE(*,'(F9.5)') RCONDV(I)
      ENDDO

      WRITE(*,*)'Matrix ALPHA : '
      WRITE(*,FMT) ALPHA
      
      WRITE(*,*)'Matrix BETA : '
      WRITE(*,FMT) BETA
      lambda = alpha/beta
      print *,'lambda = ', LAMBDA
      WRITE(*,FMT) LAMBDA
      END PROGRAM LA_CGGEVX_EXAMPLE





