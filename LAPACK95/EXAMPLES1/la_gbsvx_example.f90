      PROGRAM LA_SGBSVX_EXAMPLE
 
!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!     
!  .. "Use Statements"
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GBSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: K, KL, KU, I, J, N, NRHS
      CHARACTER(LEN=1) :: EQUED
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: AB(:,:), B(:,:), X(:,:), R(:), C(:), &
                               FERR(:), BERR(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GBSVX Example Program Results.'
      N = 6; KL = 2; KU = 1; NRHS = 2
!     ALLOCATE ( AB(2*KL+KU+1,N), B(N,NRHS), X(N,NRHS), R(N), C(N), &
      ALLOCATE ( AB(  KL+KU+1,N), B(N,NRHS), X(N,NRHS), R(N), C(N), &
                 FERR(NRHS),BERR(NRHS) )
 
      OPEN(UNIT=21,FILE='gbsv.ma',STATUS='UNKNOWN')
!     DO I=KL+1,2*KL+KU+1 
      DO I=   1,  KL+KU+1 
      DO J=1,N
         READ(21,'(F2.0)') AB(I,J)
      ENDDO  
      ENDDO
      CLOSE(21)

      AB(:,1:1)=1e-6*AB(:,1:1)
      DO J = 1, N
        DO K = 1, 1+KU-MIN(J-1,KU)-1; AB(K,J) = HUGE(1.0_WP); END DO
        DO K = 1+ KU+MIN(N-J,KL)+KL+1, 1+KU+KL; AB(K,J) = HUGE(1.0_WP); END DO
      END DO
      WRITE(*,*) 'The array AB:'
      DO I=1,N; WRITE(*,*) AB(I,:); ENDDO
     
      B = 0.0_WP
      DO I = 1, NRHS;
        DO J = 1, N
          DO K = 1+KU-MIN(J-1,KU), 1+ KU+MIN(N-J,KL)
            B(J,I) = AB(K,J)+B(J,I); 
          ENDDO
        ENDDO; 
        B(:,I) = B(:,I)*I; 
      ENDDO
    
      WRITE(*,*) 'The RHS matrix B:'
      DO I=1,N; WRITE(*,"(1(I3,1X),I3,1X)") INT(B(I,:)); ENDDO
     
      WRITE(*,*) "CALL LA_GBSVX( AB, B, X, 2, TRANS='T', EQUED=EQUED,", &
                 " R=R, C=C, FERR=FERR, BERR=BERR )"

      CALL LA_GBSVX( AB, B, X, 2, FACT='E',TRANS='T',EQUED=EQUED, &
                     R=R, C=C, FERR=FERR, BERR=BERR )

      WRITE(*,*)'X on exit: '
      DO I=1,N; WRITE(*,"(1(E12.6,1X),E12.6,1X)") X(I,:); ENDDO
      WRITE(*,*)'EQUED = ', EQUED
      WRITE(*,*)'R = ', R
      WRITE(*,*)'C = ', C
      WRITE(*,*)'FERR = ', FERR
      WRITE(*,*)'BERR = ', BERR

      END PROGRAM LA_SGBSVX_EXAMPLE


