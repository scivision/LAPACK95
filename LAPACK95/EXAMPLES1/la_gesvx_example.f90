      PROGRAM LA_SGESVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GESVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP) :: RCOND, RPVGRW
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), X(:,:),BB(:,:), FERR(:), BERR(:)
      REAL(WP), ALLOCATABLE :: RR(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SGESVX Example Program Results.'
      WRITE(*,*)'Size of matrix A, N = ?'
      READ ( *, * ) N
      WRITE(*,*)'Number of right hand sides, NRHS = ?'
      READ ( *, * ) NRHS
      ALLOCATE( A(N,N), AA(N,N), B(N,NRHS), X(N,NRHS),BB(N,NRHS), IPIV(N), RR(N,N), FERR(NRHS), BERR(NRHS) )

      OPEN(UNIT=21,FILE='gesv.ma',STATUS='UNKNOWN')
      DO J=1,N
      DO I=1,N 
         READ(21,'(F2.0)') AA(I,J)
      ENDDO  
      ENDDO
      CLOSE(21)

!      DO I = 1, N; READ (*, *) (RR(I, J), J = 1, N); ENDDO
!      AA=RR
      
      DO J = 1, NRHS; BB(:,J) = SUM( AA, DIM=2)*J; ENDDO
      
      WRITE(*,*) 'The matrix A:'
      DO I=1,N; WRITE(*,"(4(I3,1X),I3,1X)") INT(AA(I,:)); ENDDO
     
      WRITE(*,*) 'The RHS matrix B:'
      DO I=1,N; WRITE(*,"(2(I3,1X),I3,1X)") INT(BB(I,:)); ENDDO
        
      WRITE(*,*) 'CALL LA_GESVX( A, B, X, FERR=FERR, BERR=BERR, RCOND=RCOND, RPVGRW=RPVGRW )'
      A=AA; B=BB
      CALL LA_GESVX( A, B, X, FERR=FERR, BERR=BERR, RCOND=RCOND, RPVGRW=RPVGRW )
      
      WRITE(*,*) 'FERR = ', FERR
      WRITE(*,*) 'BERR = ', BERR
      WRITE(*,*) 'RCOND = ', RCOND
      WRITE(*,*) 'RPVGRW = ', RPVGRW

      WRITE(*,*) '\noindent'
      WRITE(*,*) 'The solution of the system $ A\,X = B $ is:'
      WRITE(*,*) '$$ X = \left( \begin{array}{rrr}'
      DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
      WRITE(*,*) '\end{array} \right). $$'

      END PROGRAM LA_SGESVX_EXAMPLE









