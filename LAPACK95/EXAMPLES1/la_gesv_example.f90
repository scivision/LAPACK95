      PROGRAM LA_SGESV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GESV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SGESV Example Program Results.'
      OPEN(UNIT=21,FILE='gesv.ma',STATUS='UNKNOWN')
      READ (21, * ) N
      READ (21, * ) NRHS
      ALLOCATE( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), IPIV(N) )
      DO J=1,N
      DO I=1,N 
         READ(21,'(F2.0)') AA(I,J)
      ENDDO  
      ENDDO
      CLOSE(21)

      DO J = 1, NRHS; BB(:,J) = SUM( AA, DIM=2)*J; ENDDO

      WRITE(*,*) 'The matrix A:'
      DO I=1,N; WRITE(*,"(4(I3,1X),I3,1X)") INT(AA(I,:)); ENDDO

      WRITE(*,*) 'The RHS matrix B:'
      DO I=1,N; WRITE(*,"(2(I3,1X),I3,1X)") INT(BB(I,:)); ENDDO

      WRITE(*,*) 'CALL LA_GESV( A, B )'
      A=AA; B=BB

      CALL LA_GESV(  A, B )
   
      WRITE(*,*) 'B - the solution vectors computed by LA_GESV'
      DO I=1,N; WRITE(*,"(2(E12.6,1X),E12.6,1X)") B(I,:); ENDDO
   
      WRITE(*,*) 'CALL LA_GESV( A, B(:,1), IPIV, INFO )'

      CALL LA_GESV(  AA, BB(:,1), IPIV, INFO )
   
      WRITE(*,*) ' A on exit:'
      DO I=1,N; WRITE(*,"(4(E12.6,1X),E12.6,1X)") AA(I,:); ENDDO
     
      WRITE(*,*) 'B on exit:'
      DO I=1,N; WRITE(*,"(4(E12.6,1X),E12.6,1X)") BB(I,1); ENDDO

      WRITE(*,*)'IPIV on exit:', IPIV

      WRITE(*,*)'INFO on exit:', INFO
   
      END PROGRAM LA_SGESV_EXAMPLE









