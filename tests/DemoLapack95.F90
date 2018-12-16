program DemoLapack95

use, intrinsic:: ieee_arithmetic, only: ieee_is_finite
use, intrinsic:: iso_fortran_env, only: wp=>real64, stderr=>error_unit
use assert, only: assert_isclose

#if USEMKL
use lapack95, only: gesvd
#else
use f95_lapack, only: gesvd=>la_gesvd
#endif
!use lapack, only: dlamch, sdisna

implicit none

  integer :: ierr
  real(wp) ::  A2(3,3), S2(3), A3(4,3), S3(3)
  
                
  A2 = reshape([2,0,2,&
                0,1,0,&
                0,0,0],&
                shape(A2), order=[2,1])
                
  A3 = reshape([4,3,5,&
                2,5,8,&
                3,6,10,&
                4,5,11],&
                shape(A3), order=[2,1])
                
! --------- Lapack error bound SVD example http://www.netlib.org/lapack/lug/node96.html

! print *,A3(1,:)

  call gesvd(A3,S3, info=ierr)
  if (ierr/=0) error stop 'convergence error LAPACK95 SVD accuracy example'
  call error_analysis(A3,S3)
  call assert_isclose(S3, [21.0493811_wp,2.3702096_wp,1.1426562_wp], err_msg="LAPACK95 convergence example")

! Observe that LAPACK95 was automatically tuned for far lower error
  call lapack_svd(A3,S3)
  call assert_isclose(S3, [21.0493811_wp,2.3702096_wp,1.1426562_wp], rtol=0.25_wp, err_msg="LAPACK convergence example")


!-------------- Matlab example
  call gesvd(A2,S2, info=ierr)
  if (ierr/=0) error stop 'convergence error LAPACK95 Matlab example'
  call assert_isclose(S2, [2.8284_wp,1._wp,0._wp],  err_msg="LAPACK95 Matlab example")
  call error_analysis(A2,S2)
  
  call lapack_svd(A2,S2)
  call assert_isclose(S2, [2.8284_wp,1._wp,0._wp], rtol=0.1_wp,  err_msg="LAPACK Matlab example")
  

  print *,'OK'

contains

subroutine lapack_svd(A,S)
! this is old-fashioned messy way of doing SVD with plain LAPACK, shows why it's so much nicer to use Lapack95
! http://www.netlib.org/lapack/lapack-3.1.1/html/dgesvd.f.html

  real(wp), intent(in) :: A(:,:)
  real(wp), intent(out) :: S(:)

  integer :: ierr,M,N,Lwork
  real(wp), allocatable :: U(:),VT(:),WORK(:)
  integer, allocatable :: Iwork(:)

  M = size(A,1)
  N = size(A,2)
  
  Lwork = MAX(1,3*MIN(M,N)+MAX(M,N),5*MIN(M,N)) ! dgesvd
  ! Lwork = 3*min(M,N) + max(max(M,N),7*min(M,N))   ! dgesdd

  allocate(U(M), VT(M), work(Lwork), Iwork(8*min(M,N)))

  call dgesvd('N','N',M,N,A,M,S,U,M,VT,M, WORK, Lwork, ierr)
  
  !call dgesdd('N',M,N,A,M,S,U,M,VT,M,WORK,Lwork, Iwork, ierr)
  if (ierr /= 0) then
    write(stderr,*) 'SVD failure, error code ',ierr
    error stop
  endif


endsubroutine lapack_svd


subroutine error_analysis(A,S)

  real(wp), intent(in) :: A(:,:), S(:)

  integer :: M,N,P, ierr
  real(wp) :: errbnd
  real(wp), allocatable :: RcondU(:), RcondV(:), Verrbnd(:), Uerrbnd(:)
  real(wp), parameter :: eps = epsilon(0._wp)
  
  print '(A,ES10.3,A,I3)','machine epsilon',eps,' for real bits ',storage_size(eps)


  M = size(A,1)
  N = size(A,2)
  P = size(S)
  
  allocate(RcondU(P), RcondV(P),Verrbnd(P), Uerrbnd(P))
  
  errbnd = eps * S(1)
  
  call sdisna('l', M,N,S, RcondU,ierr)
  if (ierr/=0) then
    write(stderr,'(A,I2,A)') 'argument # ',-ierr,' was illegal while computing U condition number'
    return
  endif
  
  call sdisna('r', M,N,S, RcondV,ierr)
  if (ierr/=0) then
    write(stderr,'(A,I2,A)') 'argument # ',-ierr,' was illegal while computing V condition number'
    return
  endif
  
  Verrbnd = eps * (S(1)/RcondV)
  Uerrbnd = eps * (S(1)/RcondU)
  
  print '(A,10F10.3)','singular values',S
  print '(A,2ES13.3)','V, U error bound', Verrbnd(1),Uerrbnd(1)

endsubroutine error_analysis


endprogram
