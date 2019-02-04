      PROGRAM LA_GGEV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GGEV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), ALPHA(:)
      COMPLEX(WP), ALLOCATABLE :: VL(:,:), VR(:,:), BETA(:)
      REAL(WP), ALLOCATABLE :: AR(:,:), BR(:,:), ALPHAR(:), ALPHAI(:), BETAR(:), &
                               VLR(:,:), VRR(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GGEV Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), ALPHA(N), BETA(N), VL(N,N), VR(N,N) )
      ALLOCATE( AR(N,N), BR(N,N), ALPHAR(N), ALPHAI(N), BETAR(N), VLR(N,N), VRR(N,N) )

      WRITE (*,*) 'Example 1, Real Example'
      OPEN(UNIT=21,FILE='gges.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) AR(I,J)
         ENDDO
      ENDDO
      CLOSE(21)
      WRITE(*,*)'Matrix AR : '
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(AR(I,:));
      ENDDO

      OPEN(UNIT=21,FILE='gges.mb',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) BR(I,J)
         ENDDO
      ENDDO
      CLOSE(21)
      WRITE(*,*)'Matrix BR : '
      DO I=1,N;
         WRITE(*,"(5(I3,1X))") INT(BR(I,:));
      ENDDO
      WRITE(*,*)
      WRITE(*,*) 'CALL LA_GGEV( A, B, ALPHAR, ALPHAI, BETAR, VLR, VRR )'
      CALL LA_GGEV( AR, BR, ALPHAR, ALPHAI, BETAR, VLR, VRR )

      WRITE(*,*); WRITE(*,*)'ALPHAR on exit : '; WRITE(*,*) ALPHAR(1:N)
      WRITE(*,*); WRITE(*,*)'ALPHAI on exit : '; WRITE(*,*) ALPHAI(1:N)
      WRITE(*,*); WRITE(*,*)'BETA on exit : '; WRITE(*,*) BETAR(1:N)
      WRITE(*,*)'Array VL:'; DO I =1,N; WRITE(*,*)I, VLR(I,1:N); ENDDO
      WRITE(*,*)'Array VR:'; DO I =1,N; WRITE(*,*)I, VRR(I,1:N); ENDDO

      WRITE(*,*)
      WRITE(*,*)' Generalized eigenvalues : '
      DO I=1,N
         WRITE(*,*) '(',ALPHAR(I)/BETAR(I),',',ALPHAI(I)/BETAR(I),')'
      ENDDO

      WRITE (*,*) 'Example 2, Complex Example'
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
      WRITE(*,*) 'CALL LA_GGEV( A, B, ALPHA, BETA, INFO=INFO )'
      CALL LA_GGEV( A, B, ALPHA, BETA,  INFO=INFO )
   
      WRITE(*,*)
      WRITE(*,*)'ALPHA on exit : '
      DO I=1,N
         WRITE(*,*) ALPHA(I)
      ENDDO

      WRITE(*,*)
      WRITE(*,*)'BETA on exit : '
      DO I=1,N
         WRITE(*,*) BETA(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*)'INFO = ', INFO

      WRITE(*,*) 
      WRITE(*,*)' Generalized eigenvalues : '
      DO I=1,N 
         WRITE(*,*)  ALPHA(I)/BETA(I)
      ENDDO

      WRITE(*,*)
!     WRITE(*,*)' * EXAMPLE 2 * '
      WRITE(*,*) 'CALL LA_GGEV( A, B, ALPHAR, ALPHAI, BETA, VL, VR )'
      CALL LA_GGEV( A, B, ALPHA, BETA, VL, VR )

      WRITE(*,*)
      WRITE(*,*)'Matrix VL on exit : '
      DO I=1,N;
         WRITE(*,"(5('('(F8.5,1X,',',F8.5)')'))") REAL(VL(I,:)), AIMAG(VL(I,:))
      ENDDO

      WRITE(*,*)
      WRITE(*,*)'Matrix VR on exit : '
      DO I=1,N;
         WRITE(*,"(5('('(F8.5,1X,',',F8.5)')'))") REAL(VR(I,:)), AIMAG(VR(I,:))
      ENDDO

      END PROGRAM LA_GGEV_EXAMPLE
