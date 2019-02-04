PROGRAM LA_LAMCH_EXAMPLE
!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     August, 2000
! .. "Use Statements" ..
     USE LA_PRECISION, ONLY: SP, DP
     USE F95_LAPACK, ONLY: LA_LAMCH
! .. "Implicit Statement" ..
     IMPLICIT NONE
! .. "Parameters" ..
     CHARACTER( LEN=10 ), PARAMETER :: CHU='ESBPNRMULO', &
                                       CHL='esbpnrmulo' 
     CHARACTER( LEN=5 ), PARAMETER :: CNAME(10) = (/'eps  ', &
        'sfmin', 'base ', 'prec ', 't    ', 'rnd  ', 'emin ', &
        'rmin ', 'emax ', 'rmax '/)
! .. "Local Scalars" ..
     INTEGER :: I, INFO
     REAL(SP) :: SR
     REAL(DP) :: DR
! .. "Executable Statements" ..
     WRITE (*,*) 'LA_LAMCH Example Program Results.'
     WRITE(*,*)'Machine constants, sengle and double precision:'
     DO I=1, 10
       SR = LA_LAMCH( 1.0_SP, CHU(I:I) )
       DR = LA_LAMCH( 1.0_DP, CHL(I:I) )
       WRITE(*,*) CNAME(I), SR, '          ', DR
     ENDDO
     PRINT *, CNAME(10), LA_LAMCH( 1.0_SP, 'E', INFO), '  INFO = ', INFO
     PRINT *, CNAME(10), LA_LAMCH( 1.0_DP, 'E', INFO), '  INFO = ', INFO
     PRINT *, CNAME(10), LA_LAMCH( 1.0_DP, '1', INFO), '  INFO = ', INFO
     SR = LA_LAMCH( 1.0_SP, '1')
     PRINT *, CNAME(10), SR
END PROGRAM LA_LAMCH_EXAMPLE
