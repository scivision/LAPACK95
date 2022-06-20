PROGRAM EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
use, intrinsic :: iso_fortran_env, only : wp => real64

#if USEMKL
use lapack95, only: gesvx
#else
use f95_lapack, only: gesvx => la_gesvx
#endif
!  .. "Implicit Statement" ..
IMPLICIT NONE (type, external)
!  .. "Local Scalars" ..
INTEGER :: I, J, N, NRHS, u, info
!  .. "Local Arrays" ..
INTEGER, ALLOCATABLE :: IPIV(:)
REAL(WP) :: RCOND, RPVGRW
REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), X(:,:),BB(:,:), FERR(:), BERR(:)
REAL(WP), ALLOCATABLE :: RR(:,:)
character(1024) :: argv

!  .. "Executable Statements" ..
print *, 'SGESVX Example Program Results.'

N = 4
NRHS = 3

ALLOCATE( A(N,N), AA(N,N), B(N,NRHS), X(N,NRHS),BB(N,NRHS), IPIV(N), RR(N,N), FERR(NRHS), BERR(NRHS) )

call get_command_argument(1,argv)

OPEN(newunit=u,FILE=trim(argv)//'/gesv.ma',STATUS='old')
DO J=1,N
DO I=1,N
   READ(u,'(F2.0)') AA(I,J)
ENDDO
ENDDO
CLOSE(u)

!      DO I = 1, N; READ (*, *) (RR(I, J), J = 1, N); ENDDO
!      AA=RR

DO J = 1, NRHS; BB(:,J) = SUM( AA, DIM=2)*J; ENDDO

print *, 'The matrix A:'
DO I=1,N; WRITE(*,"(4(I3,1X),I3,1X)") INT(AA(I,:)); ENDDO

print *, 'The RHS matrix B:'
DO I=1,N; WRITE(*,"(2(I3,1X),I3,1X)") INT(BB(I,:)); ENDDO

A=AA; B=BB
CALL GESVX( A, B, X, FERR=FERR, BERR=BERR, RCOND=RCOND, RPVGRW=RPVGRW, info=info )

print *, 'FERR = ', FERR
print *, 'BERR = ', BERR
print *, 'RCOND = ', RCOND
print *, 'RPVGRW = ', RPVGRW

print *, '\noindent'
print *, 'The solution of the system $ A\,X = B $ is:'
print *, '$$ X = \left( \begin{array}{rrr}'
DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
print *, '\end{array} \right). $$'

stop info

END PROGRAM
