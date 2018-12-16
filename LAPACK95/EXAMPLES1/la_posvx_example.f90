      PROGRAM LA_SPOSVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_POSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NRHS
      CHARACTER(LEN=1) :: EQUED
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), X(:,:), S(:), &
                               FERR(:), BERR(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPOSVX Example Program Results'
      N = 5; NRHS = 3
      ALLOCATE ( A(N,N), B(N,NRHS), X(N,NRHS), S(N), FERR(NRHS), &
                 BERR(NRHS) )
    
      OPEN(UNIT=21,FILE='posv.ma',STATUS='UNKNOWN')
      DO I=1,N
         DO J=I,N
         READ(21,'(F3.0)') A(I,J);
         ENDDO
      ENDDO;
      CLOSE(21)

      A(:,1)=1E-6*A(:,1);A(1,2:N)=1E-6*A(1,2:N)

      DO J = 1, NRHS; DO I = 1, N
      B(I,J) = (SUM(A(I,I:N)) + SUM(A(1:I-1,I)))*J
      ENDDO; ENDDO
  
      WRITE(*,*) 'The array B :'
      DO J=1,NRHS; DO I = 1, N
       WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(B(I,J)); 
      ENDDO; ENDDO
                                                        
      WRITE(*,*) 'CALL LA_POSVX( A, B, X, FACT="E", EQUED=EQUED, S=S )'
      CALL LA_POSVX(  A, B, X, FACT='E', EQUED=EQUED, S=S )
      
      WRITE(*,*)'EQUED = ', EQUED
      WRITE(*,*)'S = ', S
      WRITE(*,*)'FERR = ', FERR
      WRITE(*,*)'BERR = ', BERR

      WRITE(*,*) 'The solution of the system $ A\,X = B $ is:'
      WRITE(*,*) '$$ X = \left( \begin{array}{rrr}'
      DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
      WRITE(*,*) '\end{array} \right). $$'
    
      END PROGRAM LA_SPOSVX_EXAMPLE

