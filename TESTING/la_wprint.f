      SUBROUTINE WPRINT(SRNAME, PAR, MSG)
      CHARACTER*6 SRNAME
      CHARACTER*(*) MSG
      LOGICAL PAR
      IF (PAR) THEN
        PRINT *, 'The test of ', SRNAME, ' started. ', MSG
      ELSE
        PRINT *, 'The test of ', SRNAME, ' ended. ', MSG
      END IF
      RETURN
      END
