      SUBROUTINE UESTOP(SRNAME)
      CHARACTER(*) SRNAME
!     .. Array Arguments ..
      WRITE(*,*) ' +++ ', SRNAME, ' +++ Unexpected entry +++'
      STOP
      END SUBROUTINE UESTOP
