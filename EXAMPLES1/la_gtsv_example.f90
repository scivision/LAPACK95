      PROGRAM LA_SGTSV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999

!  .. "Use Statements"
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GTSV
!  .."Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, INFO, N, NRHS
      INTEGER, ALLOCATABLE :: IPIV(:)
!  .. Local Arrays ..
      REAL(WP), ALLOCATABLE :: DL(:), D(:), DU(:), B(:,:),X(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'GTSV Example Program Results.'
      N = 6; NRHS = 3

      ALLOCATE(DL(N-1),D(N),DU(N-1),B(N,NRHS), X(N,NRHS), IPIV(N) )

      OPEN(UNIT=21,FILE='gtsv.ma',STATUS='UNKNOWN')
          DO I=1,N-1
          READ(21,'(F2.0)') DL(I)
          ENDDO
          DO I=1,N
          READ(21,'(F2.0)') D(I)   
          ENDDO
          DO I=1,N-1
          READ(21,'(F2.0)') DU(I)
          ENDDO
      CLOSE(21)

      WRITE(*,*)'DU :'
      WRITE(*,"(I3,1X)") INT(DU(:));
         
      WRITE(*,*)'D :'
      WRITE(*,"(I3,1X)") INT(D(:)); 
      
      WRITE(*,*)'DL :'
      WRITE(*,"(I3,1X)") INT(DL(:)); 
    
      DO I = 2, N-1; B(I,:) = DL(I-1) + D(I) + DU(I); ENDDO
      B(1,:) = D(1) + DU(1);B(N,:) = DL(N-1) + D(N)
      DO I = 1, NRHS; B(:,I) = B(:,I)*I; ENDDO
   
      WRITE(*,*) 'The RHS matrix B:'
      DO I=1,N; WRITE(*,"(3(I3,1X))") INT(B(I,:)); ENDDO
   
      WRITE(*,*) ' CALL LA_GTSV(DL, D, DU, B, INFO)'
      CALL LA_GTSV(DL, D, DU, B, INFO )

      WRITE(*,*)'DL on exit: '
      WRITE(*,"(F8.5)") DL(:); 
      WRITE(*,*)'D on exit: '
      WRITE(*,"(F8.5)") D(:); 
      WRITE(*,*)'DU on exit: '
      WRITE(*,"(F8.5)") DU(:); 
      WRITE(*,*)'B on exit: '
      DO I=1,N; WRITE(*,"(6(F8.5))") B(I,:); ENDDO

      WRITE(*,*)'INFO = ',INFO

      DEALLOCATE(DL,D,DU,B,X)

      END PROGRAM LA_SGTSV_EXAMPLE




