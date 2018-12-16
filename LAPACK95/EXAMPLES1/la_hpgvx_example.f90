      PROGRAM LA_HPGVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_HPGVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, ITYPE, IL, IU, M, INFO
      REAL(WP), ALLOCATABLE :: W(:)
      REAL(WP) :: ABSTOL
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IFAIL(:)
      COMPLEX(WP), ALLOCATABLE :: A(:), AA(:), B(:), BB(:), Z(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_HPGVX Example Program Results'
      N = 5
      ALLOCATE( A(N*(N+1)/2), AA(N*(N+1)/2), B(N*(N+1)/2), BB(N*(N+1)/2), &
                W(N), Z(N,N), IFAIL(N) )

      OPEN(UNIT=21,FILE='hpgvu.ma',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
         READ(21,*) A(J)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,1X,',',I3)')')") INT(A(I)), INT(AIMAG(A(I)))
      ENDDO
   

      OPEN(UNIT=21,FILE='hpgvu.mb',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
         READ(21,*) B(J)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix B:'
      DO I=1,N*(N+1)/2
         WRITE(*,"('('(I3,1X,',',I3)')')") INT(B(I)), INT(AIMAG(B(I)))
      ENDDO

      WRITE(*,*) "CALL LA_HPGVX( A, B, W, 2, Z=Z, IL=4, IU=5, M=M, ", &
                 "IFAIL=IFAIL, ABSTOL=1.0E-3_WP, INFO =INFO ) "
      ITYPE=2; IL=4; IU=5; ABSTOL=1e-3
      CALL LA_HPGVX( A, B, W, 2, Z=Z, IL=4, IU=5, M=M, &
                     IFAIL=IFAIL, ABSTOL=1.0E-3_WP, INFO = INFO )
      
      WRITE(*,*) 'W on exit:'
      DO I=1,N
         WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO

      WRITE(*,*) 'IFAIL on exit : ',IFAIL

      WRITE(*,*) 'M and INFO on exit:', M, INFO
      WRITE(*,*) 'Z on exit : '
      DO I=1,N
      WRITE(*,"(5(1H(,2(F12.10,1X),1H),1X))") (Z(I,J), J = 1,M)
      ENDDO

      END PROGRAM LA_HPGVX_EXAMPLE






