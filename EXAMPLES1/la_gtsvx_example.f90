      PROGRAM LA_SGTSVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements"
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GTSVX
!  .."Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, N, NRHS
      INTEGER, ALLOCATABLE :: IPIV(:)
      CHARACTER(LEN=1) :: TRANS
!  .. Local Arrays ..
      REAL(WP), ALLOCATABLE :: DL(:), D(:), DU(:), DLF(:), &
                DF(:), DUF(:), DU2(:), B(:,:),X(:,:)

!  .. "Executable Statements" ..
       WRITE (*,*) 'SGTSVX Example Program Results.'
       N = 6; NRHS = 3

       ALLOCATE(DL(N-1), DLF(N-1), D(N), DF(N), DU(N-1), DUF(N-1), &
                DU2(N-2), B(N,NRHS), X(N,NRHS), IPIV(N) )

      
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
       WRITE(*,"(8(I3,1X))") INT(DU(:));
       WRITE(*,*)'D :'
       WRITE(*,"(8(I3,1X))") INT(D(:)); 
       WRITE(*,*)'DL :'
       WRITE(*,"(8(I3,1X))") INT(DL(:))

       B = 0.0_WP
       DO I = 2, N-1; B(I,:) = DU(I-1) + D(I) + DL(I); ENDDO
       B(1,:) = D(1) + DL(1);B(N,:) = DU(N-1) + D(N)
       DO I = 1, NRHS; B(:,I) = B(:,I)*I; ENDDO
       WRITE(*,*)'B = '
       DO I=1,N; WRITE(*,"(8(F8.5,1X))") B(I,:)
       ENDDO

       WRITE(*,*) "CALL LA_GTSVX(DL, D, DU, B, X, DLF, DF, DUF, DU2, TRANS='T' )"
       TRANS='T'
       CALL LA_GTSVX(DL, D, DU, B, X, DLF, DF, DUF, DU2, IPIV, TRANS='T' )

       WRITE(*,*)'X = '
       DO I=1,N;WRITE(*,"(6(F8.5,1X))") X(I,:)
       ENDDO

       WRITE(*,*)'DLF on exit:'; WRITE(*,"(8(F8.5,1X))") DLF(:);
       WRITE(*,*)'DF on exit:';  WRITE(*,"(8(F8.5,1X))") DF(:);
       WRITE(*,*)'DUF on exit:'; WRITE(*,"(8(F8.5,1X))") DUF(:); 
       WRITE(*,*)'DU2 on exit:'; WRITE(*,"(8(F8.5,1X))") DU2(:);
       WRITE(*,*)'IPIV on exit:';WRITE(*,"(8(I6,1X))") IPIV(:);

       DEALLOCATE(DL,DLF,D,DF,DU,DUF,DU2,B,X)

       END PROGRAM LA_SGTSVX_EXAMPLE




