PROGRAM LA_DPTSVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_PTSVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N, NRHS
   REAL(WP) :: RCOND
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: E(:), B(:,:), EF(:), X(:,:)
   REAL(WP), ALLOCATABLE :: D(:), DD(:), EE(:), BB(:,:), DF(:), FERR(:), BERR(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DPTSVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( D(N), DD(N), E(N-1), EE(N-1), B(N,NRHS), BB(N,NRHS), &
             DF(N), EF(N-1), X(N,NRHS), FERR(NRHS), BERR(NRHS) )
!
   READ (NIN, *) DD(:), EE(:)
   DO I = 1, NRHS
      BB(1,I) = (DD(1) + EE(1))*I
      BB(2:N-1,I) = (EE(1:N-2) + DD(2:N-1) + EE(2:N-1))*I
      BB(N,I) = (EE(N-1) + DD(N))*I
   ENDDO
   D=DD; E=EE; B=BB
   WRITE(NOUT,*) 'The matrix A:'
   WRITE (NOUT,*) 'D '; WRITE (NOUT,FMT) D
   WRITE (NOUT,*) 'E '; WRITE (NOUT,FMT) E
   WRITE(NOUT,*) 'The RHS matrix B:'
   DO J = 1, NRHS
     WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
   ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_DPTSVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX( D, E, B, X, INFO=INFO )'
   D=DD; E=EE; B=BB
   CALL LA_PTSVX( D, E, B, X, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PTSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX( D, E, B(1:N,1), X(1:N,1), RCOND=RCOND, INFO=INFO )'
   D=DD; E=EE; B=BB
   CALL LA_PTSVX( D, E, B(1:N,1), X(1:N,1), RCOND=RCOND, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PTSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(:,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX( D, E, B, X )'
   D=DD; E=EE; B=BB
   CALL LA_PTSVX( D, E, B, X )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PTSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX(D, E, B(1:N,1), X(1:N,1) )'
   D=DD; E=EE; B=BB
   CALL LA_PTSVX(D, E, B(1:N,1), X(1:N,1) )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PTSVX:'
   WRITE (NOUT,FMT) X(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX( D, E, B, X, RCOND=RCOND)'
   D=DD; E=EE; B=BB
   CALL LA_PTSVX( D, E, B, X, RCOND=RCOND)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PTSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'RCOND = ', RCOND
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX( D, E, B(1:N,1), X(1:N,1), RCOND=RCOND )'
   D=DD; E=EE; B=BB
   CALL LA_PTSVX( D, E, B(1:N,1), X(1:N,1), RCOND=RCOND )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PTSVX:'
   WRITE (NOUT,FMT) X(1:N,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX(D, E, B(1:N-1,:), X, INFO =INFO )'
   D=DD; E=EE; B=BB; X=HUGE(1.0_WP)
   CALL LA_PTSVX( D, E, B(1:N-1,:), X, INFO=INFO)
   WRITE(NOUT,*)' INFO = ', INFO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX( D, E(1:N-2), B(1:N,1), X(1:N,1), INFO=INFO )'
   D=DD; E=EE; B=BB; X=HUGE(1.0_WP)
   CALL LA_PTSVX( D, E(1:N-2), B(1:N,1), X(1:N,1), INFO=INFO )
   WRITE(NOUT,*)'   B - the RHS vector.'
   WRITE (NOUT,FMT) B(1:N,1)
   WRITE(NOUT,*)'   INFO = ', INFO
   WRITE (NOUT,*) INFO
      
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX(D, E, B, X, FACT=''F'', INFO =INFO )'
   D=DD; E=EE; B=BB; X=HUGE(1.0_WP)
   CALL LA_PTSVX( D, E, B, X, FACT='F', INFO=INFO)
   WRITE(NOUT,*)'  INFO = ', INFO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX( D, E, B(1:N,1), X(1:N,1), FACT=''4'', INFO=INFO )'
   D=DD; E=EE; B=BB; X=HUGE(1.0_WP)
   CALL LA_PTSVX( D, E, B(1:N,1), X(1:N,1), FACT='4', INFO=INFO )
   WRITE(NOUT,*)'   INFO = ', INFO
   WRITE (NOUT,*) INFO
      
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX(D, E, B, X, DF, EF, FACT=''F'', INFO =INFO )'
   D=DD; E=EE; B=BB; X=HUGE(1.0_WP)
   CALL LA_PTSVX( D, E, B, X, DF, EF, FACT='N' )
   CALL LA_PTSVX( D, E, B, X, DF, EF, FACT='F', INFO=INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PTSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PTSVX( D, E, B(1:N,1), X(1:N,1), DF, FACT=''N'', INFO=INFO )'
   D=DD; E=EE; B=BB; X=HUGE(1.0_WP)
   CALL LA_PTSVX( D, E, B(1:N,1), X(1:N,1), DF, FACT='N', INFO=INFO )
   WRITE(NOUT,*)'   INFO = ', INFO
   WRITE (NOUT,*) INFO
   
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(D, E, B, X, DF, EF, ''N'', FERR, BERR, RCOND, INFO)'
   D=DD; E=EE; B=BB; X=HUGE(1.0_WP)
   CALL LA_PTSVX(D, E, B, X, DF, EF, 'N', FERR, BERR, RCOND, INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(D, E, B(1:N,1), X(1:N,1), DF, EF, ''N'', ', &
                 'FERR, BERR, RCOND, INFO) '
   D=DD; E=EE; B=BB; X=HUGE(1.0_WP)
   CALL LA_PTSVX(D, E, B(1:N,1), X(1:N,1), DF, EF, 'N', FERR(1), BERR(1), RCOND, INFO)
   WRITE(NOUT,*)'   INFO = ', INFO
   WRITE (NOUT,*) INFO
!
END PROGRAM LA_DPTSVX_ET_EXAMPLE
