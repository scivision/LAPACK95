PROGRAM EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999

!  .. "Use Statements"
USE LA_PRECISION, ONLY: WP => dp
use f95_lapack, only: gbsv=>la_gbsv
!  .. "Implicit Statement" ..
IMPLICIT NONE
!  .. "Local Scalars" ..
INTEGER :: K, KL, KU, I, J, N, NRHS, INFO, u
!  .. "Local Arrays" ..
INTEGER, ALLOCATABLE :: IPIV(:)
REAL(WP), ALLOCATABLE :: AB(:,:), B(:,:)
character(1024) :: argv
!  .. "Executable Statements" ..
print *, 'SGBSV Example Program Results.'
N = 6; KL = 2; KU = 1; NRHS = 2  
ALLOCATE ( AB(2*KL+KU+1,N), B(N,NRHS), IPIV(N))

call get_command_argument(1, argv)

OPEN(newunit=u,FILE=trim(argv)//'/gbsv.ma',STATUS='old')
DO I=KL+1,2*KL+KU+1 
DO J=1,N
  READ(u,'(F2.0)') AB(I,J)
ENDDO  
ENDDO
CLOSE(u)

print *, 'The matrix AB:'
DO I=1,N; WRITE(*,"(5(I3,1X,1X),I3,1X)") INT(AB(I,:)); 
ENDDO

DO I = 1, NRHS  
DO J = 1, N
DO K = MAX(1,J-KL),MIN(J+KU,N); B(J,I) = AB(KL+KU+1+J-K,K)+B(J,I); 
ENDDO
ENDDO
B(:,I) = B(:,I)*I; 
ENDDO

print *, 'The RHS matrix B:'
DO I=1,N; WRITE(*,"(1(I3,1X),I3,1X)") INT(B(I,:)); ENDDO

CALL GBSV( AB, B, 2, IPIV, INFO )

print *,'AB on exit: '
DO I=1,2*KL+KU+1; WRITE(*,"(5(E12.6,1X),E12.6,1X)") AB(I,:); ENDDO
print *,'B on exit: '
DO I=1,N; WRITE(*,"(1(E12.6,1X),E12.6,1X)") B(I,:); ENDDO
print *,'IPIV on exit: ', IPIV

stop info

END PROGRAM
