      PROGRAM LA_CHPGV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_HPGV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
      REAL(WP), ALLOCATABLE :: W(:)
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:), AA(:), B(:), BB(:), Z(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC REAL, AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_HPGV Example Program Results'
      N = 5
      ALLOCATE( A(N*(N+1)/2), AA(N*(N+1)/2), B(N*(N+1)/2), BB(N*(N+1)/2), W(N), Z(N,N) )

      OPEN(UNIT=21,FILE='hpgvu.ma',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
         READ(21,*) A(J)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix AP : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,',',I3)')')") INT(A(I)), INT(AIMAG(A(I)))
      ENDDO

      OPEN(UNIT=21,FILE='hpgvu.mb',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
         READ(21,*) B(J)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix B : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,',',I3)')')") INT(B(I)), INT(AIMAG(B(I)))
      ENDDO
    
      WRITE(*,*) "CALL LA_HPGV( AP, BP, W) "
      CALL LA_HPGV( A, B, W )
   
      WRITE(*,*)'BP on exit : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(E14.6,1X,','E14.6)')')") REAL(B(I)), AIMAG(B(I))
      ENDDO
  
      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(E14.6))") W(I)
      ENDDO   

      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '

      OPEN(UNIT=21,FILE='hpgvl.ma',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
         READ(21,*) AA(J)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix AP : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,',',I3)')')") INT(AA(I)), INT(AIMAG(AA(I)))
      ENDDO
    
      OPEN(UNIT=21,FILE='hpgvl.mb',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
         READ(21,*) BB(J)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix BP : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,',',I3)')')") INT(BB(I)), INT(AIMAG(BB(I)))
      ENDDO
   
      WRITE(*,*)
!      WRITE(*,*) "CALL LA_HPGV( A, B, W, JOBZ, UPLO, INFO )"
!      CALL LA_HPGV( AA, BB, W, 3, 'L', Z, INFO )
       WRITE(*,*) "CALL LA_HPGV( A, B, W, 3, 'L', Z, INFO)"
       CALL LA_HPGV( AA, BB, W, 3, 'L', Z, INFO )
      
      WRITE(*,*)'Z on exit : '
      DO I=1,N
         DO J=1,N-1
            WRITE(*,"('('(E14.6,1X,',',1X,E14.6)')')") REAL(Z(I,J)), AIMAG(Z(I,J))
         ENDDO
         WRITE(*,"('('(E14.6,1X,',',1X,E14.6)')')") REAL(Z(I,N)), AIMAG(Z(I,N))
      ENDDO
      WRITE(*,*)'BP on exit : '
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(E14.6,1X,',',1X,E14.6)')')") REAL(BB(I)), AIMAG(BB(I))
      ENDDO

      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO
      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_CHPGV_EXAMPLE
