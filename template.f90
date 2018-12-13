
! INTPOL 1.1
! -- General (Three Atoms PES) Interpolation Program
! -- Method: Manybody/Spline/RKHS/Hybrid
! -- Author: Huanchen Zhai
! -- Email: stczhc@gmail.com
! -- v1.0: Nov. 27, 2015
! -- v1.1: Dec. 16, 2015

#include "head.h"

#ifdef PREP
program intpol_p
  implicit none
  interface
    subroutine intpol_main(do_type, x, y, z, v, dv)
      implicit none
      integer :: do_type
      real(8), optional :: x, y, z, v, dv(0:do_type - 1)
    end subroutine intpol_main
  end interface
  call intpol_main(-1)
end program intpol_p
#endif

#ifdef TEST
program intpol_p
  implicit none
  interface
    subroutine intpol_main(do_type, x, y, z, v, dv)
      implicit none
      integer :: do_type
      real(8), optional :: x, y, z, v, dv(0:do_type - 1)
    end subroutine intpol_main
  end interface
  integer :: i
  real(8) :: x, y, z, v
  call intpol_main(0)
  call intpol_main(2)
end program intpol_p
#endif

subroutine intpol_main(do_type, x, y, z, v, dv)
  implicit none

#ifdef MANYBODY
  interface
    function fh(x)
      implicit none
      real(8) :: x(0:2), fh(0:2)
    end function fh
#ifdef DERIVATIVE1
    function fhd(x)
      implicit none
      real(8) :: x(0:2), fhd(0:2, 0:2)
    end function fhd
#endif
#ifdef DERIVATIVE2
    function fhdd(x)
      implicit none
      real(8) :: x(0:2), fhdd(0:5, 0:2)
    end function fhdd
#endif
  end interface
#endif

  integer :: do_type ! -1:prep 0:init 1:E 3:dE 6:ddE
  real(8), optional :: x, y, z, v, dv(0:do_type - 1)

  real(8), parameter :: m_pi = 3.141592653589793d0
  real(8), parameter :: m_e = 2.718281828459045d0
  real(8), parameter :: vcut = VCUT

  type arr
    real(8), pointer :: x(:)
  end type arr
  type arrx
    real(8), pointer :: x(:, :)
  end type arrx

  real(8), save :: r_exp(0:3), r_alpha(0:3)

#if defined(RKHS)
  real(8), allocatable, save :: a(:, :), b(:)
#endif

#ifdef MANYBODY
  real(8), parameter :: e1 = E1
  real(8), allocatable, save :: mbr(:, :), mbv(:, :), mbx(:, :)
  integer, parameter :: umm = UMM
  integer, allocatable, save :: mbn(:)
  real(8), parameter :: mbvcut(0:2) = MBVCUT
#endif
#ifdef RKHS
  real(8), allocatable, save :: r_coef(:, :)
#endif
#ifdef MAIN0R
  type(arr), allocatable, save :: mbsum(:)
#endif

#if defined(MAIN1R) && defined(READMAIN3)
  real(8), allocatable, save :: rsum(:, :, :), rxijk(:, :)
  integer, save :: nrsum(0:5)
#elif defined(MAIN1R) && defined(READMAIN12)
  type(arr), allocatable, save :: rsum(:)
#elif defined(MAIN1R) && defined(READMAIN21)
  type(arrx), allocatable, save :: rsum(:), rxij(:)
  integer, allocatable, save :: nrsum(:, :)
#elif defined(MAIN1R) && defined(READMAIN111)
  type(arr), allocatable, save :: rsum(:, :), rxi(:, :)
#endif

#if defined(READMAIN3)
  real(8), allocatable, save :: rx(:)
#elif defined(READMAIN12) || defined(READMAIN21)
  real(8), allocatable, save :: rx(:, :)
#elif defined(READMAIN111)
  real(8), allocatable, save :: rx(:, :, :)
#endif

#ifdef TEST
#if defined(READMAIN3)
  real(8), allocatable, save :: tsv(:)
#elif defined(READMAIN12) || defined(READMAIN21)
  real(8), allocatable, save :: tsv(:, :)
#elif defined(READMAIN111)
  real(8), allocatable, save :: tsv(:, :, :)
#endif
#endif

#ifdef READMAIN3
  real(8), allocatable, save :: vp(:), vijk(:, :)
  integer, parameter :: uijk = UIJK
#ifdef MAIN1R
  integer, parameter :: uim = UIM, ujm = UJM, ukm = UKM
#endif
#endif
#ifdef READMAIN21
  real(8), allocatable, save :: vk(:), vijp(:, :), vij(:, :, :)
  integer, allocatable, save :: uij(:)
  integer, parameter :: uk = UK, uijm = UIJM
#ifdef MAIN1R
  integer, parameter :: uim(0:uk-1) = UIM, &
    ujm(0:uk-1) = UJM
#endif
#endif
#ifdef READMAIN12
  real(8), allocatable, save :: vjk(:, :), vip(:, :), vi(:, :)
  integer, allocatable, save :: ui(:)
  integer, parameter :: ujk = UJK, uim = UIM
#ifdef MAIN2R
  integer, parameter :: ujm = UJM, ukm = UKM
#endif
#endif
#ifdef READMAIN111
  real(8), allocatable, save :: vk(:), vj(:, :), vip(:, :, :), vi(:, :, :)
  integer, allocatable, save :: uj(:), ui(:, :)
  integer, parameter :: uk = UK
  integer, parameter :: ujm = UJM
  integer, parameter :: uim = UIM
#endif

  character(30), parameter :: parameter_file = PARAMFILE
  character(30), parameter :: main_file = MAINFILE
  character(30), parameter :: prep_file = PREPFILE
#ifdef MANYBODY
  character(30), parameter :: manybody_file = MANYBODYFILE
#endif
#if defined(MANYBODY) && defined(PREP)
  character(30), parameter :: rab_file = RABFILE
  character(30), parameter :: rbc_file = RBCFILE
  character(30), parameter :: rac_file = RACFILE
#endif

  integer i, j, k, r, ip, jp, kp
  real(8) wi, wj, wk, wv, wr(0:2)
  real(8) wn(0:2), wnp(0:2)
#ifdef TEST
  real(8) ts(0:10)
#endif

#ifndef PREP
  if (do_type == 0) then
#endif
  write (*, *) 'ready...'
  call read_parameter()
  call read_main()

#ifdef TEST
#if defined(READMAIN12)
  allocate ( tsv(0:uim-1, 0:ujk-1) ); tsv = vip
#elif defined(READMAIN111)
  allocate ( tsv(0:uim-1, 0:ujm-1, 0:uk-1) ); tsv = vip
#elif defined(READMAIN3)
  allocate ( tsv(0:uijk-1) ); tsv = vp
#elif defined(READMAIN21)
  allocate ( tsv(0:uijm-1, 0:uk-1) ); tsv = vijp
#endif
#endif

#ifdef MANYBODY
  allocate ( mbn(0:2), mbr(0:umm-1, 0:2), mbv(0:umm-1, 0:2), mbx(0:umm-1, 0:2) )
#ifdef PREP
  call read_ori_manybody()
  ! calculate many body coefs and sums
#ifdef MAIN0R
  write (*, *) 'many body rkhs...'
  do r = 0, 2
    allocate ( a(0:mbn(r)-1,0:mbn(r)-1), b(0:mbn(r)-1) )
    wv = mbv(mbn(r) - 1, r)
    do i = 0, mbn(r) - 1
      do j = i, mbn(r) - 1
        a(j, i) = MAIN0RK(mbr(i, r), mbr(j, r))
      end do
      a(i, i) = a(i, i) + r_alpha(0)
      mbv(i, r) = mbv(i, r) - wv
      b(i) = mbv(i, r)
    end do
    call cholesky(a, b, mbx(0, r), mbn(r))
    deallocate ( a, b )
  end do
#endif
#ifdef MAIN0S
  write (*, *) 'many body spline...'
  do r = 0, 2
    mbv(0:mbn(r)-1, r) = mbv(0:mbn(r)-1, r) - mbv(mbn(r) - 1, r)
    call MAIN0SS(mbr(0, r), mbv(0, r), mbn(r), mbx(0, r))
  end do
#endif
  call write_manybody()
#else
  call read_manybody()
#endif
#ifdef MAIN0R
  allocate ( mbsum(0:2) )
  do r = 0, 2
    allocate ( mbsum(r)%x(-1:4*mbn(r)+2*(1+MAIN0R)) )
    call MAIN0RP(mbsum(r)%x, mbn(r), mbr(0, r), mbx(0, r))
  end do
#endif

  ! substract two body and one body parts
#ifdef READMAIN3
  do i = 0, uijk - 1
    wi = vijk(0, i); wj = vijk(1, i); wk = vijk(2, i); wv = vp(i)
#endif
#ifdef READMAIN21
  do k = 0, uk - 1
    do i = 0, uij(k) - 1
      wi = vij(0, i, k); wj = vij(1, i, k); wk = vk(k); wv = vijp(i, k)
#endif
#ifdef READMAIN12
  do j = 0, ujk - 1
    do i = 0, ui(j) - 1
      wi = vi(i, j); wj = vjk(0, j); wk = vjk(1, j); wv = vip(i, j)
#endif
#ifdef READMAIN111
  do k = 0, uk - 1
    do j = 0, uj(k) - 1
      do i = 0, ui(j, k) - 1
        wi = vi(i, j, k); wj = vj(j, k); wk = vk(k); wv = vip(i, j, k)
#endif
        wr(0:2) = fh((/ wi, wj, wk /))
        wv = wv - e1
        do r = 0, 2
#ifdef MAIN0R
          wv = wv - MAIN0RE(mbsum(r)%x, mbn(r), mbr(0, r), wr(r))
#endif
#ifdef MAIN0S
          wv = wv - spline_eval(mbr(0, r), mbv(0, r), mbx(0, r), mbn(r), wr(r))
#endif
        end do
#ifdef READMAIN111
        vip(i, j, k) = wv
      end do
    end do
  end do
#endif
#ifdef READMAIN12
      vip(i, j) = wv
    end do
  end do
#endif
#ifdef READMAIN21
      vijp(i, k) = wv
    end do
  end do
#endif
#ifdef READMAIN3
    vp(i) = wv
  end do
#endif
#endif

#if defined(READMAIN12)
  allocate ( rx(0:uim-1, 0:ujk-1) )
#elif defined(READMAIN111)
  allocate ( rx(0:uim-1, 0:ujm-1, 0:uk-1) )
#elif defined(READMAIN3)
  allocate ( rx(0:uijk-1) )
#elif defined(READMAIN21)
  allocate ( rx(0:uijm-1, 0:uk-1) )
#endif

#ifdef PREP
#ifdef MAIN1R
  write (*, *) 'preparation for first dimension main rkhs...'
#ifdef READMAIN3
  allocate ( a(0:uijk-1, 0:uijk-1), b(0:uijk-1) )
  do i = 0, uijk - 1
    wv = vp(i)
    do ip = i, uijk - 1
      wn(0:2) = vijk(0:2, i); wnp(0:2) = vijk(0:2, ip)
#endif
#ifdef READMAIN21
  do k = 0, uk - 1
    allocate ( a(0:uij(k)-1, 0:uij(k)-1), b(0:uij(k)-1) )
    do i = 0, uij(k) - 1
      wv = vijp(i, k)
      do ip = i, uij(k) - 1
        wn(0:1) = vij(0:1, i, k); wnp(0:1) = vij(0:1, ip, k)
#endif
#ifdef READMAIN12
  do j = 0, ujk - 1
    allocate ( a(0:ui(j)-1, 0:ui(j)-1), b(0:ui(j)-1) )
    do i = 0, ui(j) - 1
      wv = vip(i, j)
      do ip = i, ui(j) - 1
        wn(0) = vi(i, j); wnp(0) = vi(ip, j)
#endif
#ifdef READMAIN111
  do k = 0, uk - 1
    do j = 0, uj(k) - 1
      allocate ( a(0:ui(j, k)-1, 0:ui(j, k)-1), b(0:ui(j, k)-1) )
      do i = 0, ui(j, k) - 1
        wv = vip(i, j, k)
        do ip = i, ui(j, k) - 1
          wn(0) = vi(i, j, k); wnp(0) = vi(ip, j, k)
#endif
          a(ip, i) = MAIN1RK(wn(0), wnp(0))
#if defined(READMAIN21) || defined(READMAIN3)
          a(ip, i) = a(ip, i) * MAIN2RK(wn(1), wnp(1))
#if defined(READMAIN3)
          a(ip, i) = a(ip, i) * MAIN3RK(wn(2), wnp(2))
#endif
#endif
        end do
        a(i, i) = a(i, i) + r_alpha(1)
        b(i) = wv
      end do
#ifdef READMAIN111
      call cholesky(a, b, rx(0, j, k), ui(j, k))
      deallocate ( a, b )
    end do
  end do
#endif
#ifdef READMAIN12
    call cholesky(a, b, rx(0, j), ui(j))
    deallocate ( a, b )
  end do
#endif
#ifdef READMAIN21
    call cholesky(a, b, rx(0, k), uij(k))
    deallocate ( a, b )
  end do
#endif
#ifdef READMAIN3
  call cholesky(a, b, rx, uijk)
  deallocate ( a, b )
#endif
#endif

#ifdef MAIN1S
  write (*, *) 'preparation for first dimension main spline...'
#ifdef READMAIN12
  do j = 0, ujk - 1
    call MAIN1SS(vi(0, j), vip(0, j), ui(j), rx(0, j))
  end do
#endif
#ifdef READMAIN111
  do k = 0, uk - 1
    do j = 0, uj(k) - 1
      call MAIN1SS(vi(0, j, k), vip(0, j, k), ui(j, k), rx(0, j, k))
    end do
  end do
#endif
#endif
  open (37, file = prep_file); write (37, *) rx; close (37)
  write (*, *) 'finished.'
#else
  open (37, file = prep_file); read (37, *) rx; close (37)
#endif

#ifndef PREP
#ifdef MAIN1R
  write (*, *) 'presummation for first dimension main rkhs...'
#ifdef READMAIN3
  nrsum = (/ uim, ujm, ukm, MAIN1R, MAIN2R, MAIN3R /)
  allocate ( rsum(-1:4*(nrsum(2)+nrsum(5))+2, -1:4*(nrsum(1)+nrsum(4))+2, -1:4*(nrsum(0)+nrsum(3))+2) )
  allocate ( rxijk(0:max(nrsum(0),nrsum(1),nrsum(2))-1, 0:2) )
  call rkhs3d_presum(rsum, nrsum, rxijk, vijk, rx, uijk)
#endif
#ifdef READMAIN21
  allocate ( nrsum(0:3, 0:uk-1), rsum(0:uk-1), rxij(0:uk-1) )
  do k = 0, uk - 1
    nrsum(0:3, k) = (/ uim(k), ujm(k), MAIN1R, MAIN2R /)
    allocate ( rsum(k)%x(-1:4*(nrsum(1, k)+nrsum(3, k))+2, -1:4*(nrsum(0, k)+nrsum(2, k))+2), &
      rxij(k)%x(0:max(nrsum(0, k), nrsum(1, k))-1, 0:1) )
    call rkhs2d_presum(rsum(k)%x, nrsum(0, k), rxij(k)%x, vij(0, 0, k), rx(0, k), uij(k), (/ 1, 2 /))
  end do
#endif
#ifdef READMAIN12
  allocate ( rsum(0:ujk-1) )
  do j = 0, ujk - 1
    allocate ( rsum(j)%x(-1:4*ui(j)+2*(1+MAIN1R)) )
    call MAIN1RP(rsum(j)%x, ui(j), vi(0, j), rx(0, j))
  end do
#endif
#ifdef READMAIN111
  allocate ( rsum(0:ujm-1, 0:uk-1) )
  do k = 0, uk - 1
    do j = 0, uj(k) - 1
      allocate ( rsum(j, k)%x(-1:4*ui(j, k)+2*(1+MAIN1R)) )
      call MAIN1RP(rsum(j, k)%x, ui(j, k), vi(0, j, k), rx(0, j, k))
    end do
  end do
#endif
#endif
  else if (do_type == 1) then
    v = main_eval((/ x, y, z /))
  else if (do_type == 3) then
    dv(0:2) = main_evald((/ x, y, z /))
  else if (do_type == 6) then
    dv(0:5) = main_evaldd((/ x, y, z /))
#ifdef TEST
  else if (do_type == 2) then
    write (*, '(x,A16,A25,A25,A25)') '#', 'root mean square error', 'min error', 'max eroor'
    write (*, '(2x,15a1,3(x,24a1))') ('-', i = 1, 15+72 )
    ip = 0; jp = 0; r = 1000
#ifdef READMAIN3
    do i = 0, uijk - 1
      wi = vijk(0, i); wj = vijk(1, i); wk = vijk(2, i); wv = tsv(i)
#endif
#ifdef READMAIN21
    do k = 0, uk - 1
      do i = 0, uij(k) - 1
        wi = vij(0, i, k); wj = vij(1, i, k); wk = vk(k); wv = tsv(i, k)
#endif
#ifdef READMAIN12
    do j = 0, ujk - 1
      do i = 0, ui(j) - 1
        wi = vi(i, j); wj = vjk(0, j); wk = vjk(1, j); wv = tsv(i, j)
#endif
#ifdef READMAIN111
    do k = 0, uk - 1
      do j = 0, uj(k) - 1
        do i = 0, ui(j, k) - 1
          wi = vi(i, j, k); wj = vj(j, k); wk = vk(k); wv = tsv(i, j, k)
#endif
          ts(6) = (main_eval((/ wi, wj, wk /)) - wv) ** 2
          if (ip == 0) then
            ts(0) = ts(6); ts(1) = ts(6); ts(2) = ts(6)
          else
            ts(0) = ts(0) + ts(6); ts(1) = min(ts(1), ts(6)); ts(2) = max(ts(2), ts(6))
          end if
          if (mod(ip, r) == 0) then
            if (ip /= 0) then
              write (*, '(x,I8,I8,F25.10,F25.10,F25.10)') ip - r + 1, ip, sqrt(ts(3)/r), sqrt(ts(4)), sqrt(ts(5))
            end if
            ts(3) = ts(6); ts(4) = ts(6); ts(5) = ts(6)
          else
            ts(3) = ts(3) + ts(6); ts(4) = min(ts(4), ts(6)); ts(5) = max(ts(5), ts(6))
          end if
          ip = ip + 1
#ifdef READMAIN111
        end do
      end do
    end do
#endif
#ifdef READMAIN12
      end do
    end do
#endif
#ifdef READMAIN21
      end do
    end do
#endif
#ifdef READMAIN3
    end do
#endif
    write (*, '(2x,15a1,3(x,24a1))') ('-', i = 1, 15+72 )
    write (*, '(x,a16,f25.10,f25.10,f25.10)') 'total', sqrt(ts(0)/ip), sqrt(ts(1)), sqrt(ts(2))
#endif
  end if
#endif

contains

  subroutine read_parameter()
    implicit none
    integer i
    
    namelist /rkhs/ r_exp, r_alpha
    open (11, file = parameter_file)
    read (11, rkhs)
    close (11)
#ifdef RKHS
    allocate ( r_coef(0:1, 0:3) )
    do i = 0, 3
      r_coef(0, i) = 4 * factorial(r_exp(i)) / factorial(r_exp(i) + 2)
      r_coef(1, i) = 4 * factorial(r_exp(i) + 1) / factorial(r_exp(i) + 3)
    end do
#endif
  end subroutine read_parameter

  subroutine read_main()
    implicit none
    integer ix, ii, iz, sta
    real(8) va1, va2, va3, va4

    open(12, file = main_file)

#if defined(READMAIN3)
    allocate ( vijk(0:2, 0:uijk-1), vp(0:uijk-1) )
#elif defined(READMAIN21)
    allocate ( vk(0:uk-1), uij(0:uk-1), vij(0:1, 0:uijm-1, 0:uk-1) )
    allocate ( vijp(0:uijm-1, 0:uk-1) )
#elif defined(READMAIN12)
    allocate ( vjk(0:1, 0:ujk-1), ui(0:ujk-1), vi(0:uim-1, 0:ujk-1) )
    allocate ( vip(0:uim-1, 0:ujk-1) )
#elif defined(READMAIN111)
    allocate ( vj(0:ujm-1, 0:uk-1), vk(0:uk-1), uj(0:uk-1), ui(0:ujm-1, 0:uk-1) )
    allocate ( vi(0:uim-1, 0:ujm-1, 0:uk-1), vip(0:uim-1, 0:ujm-1, 0:uk-1) )
#endif
    ix = -1
    iz = 0

    do while (.true.)
      read (12, *, iostat = sta) ii, va1, va2, va3, va4
      if (sta /= 0 .or. ii == 0) exit
      va4 = min(va4, vcut)
#if defined(READMAIN3)
      ix = ix + 1
      vijk(0:2, ix) = (/ va1, va2, va3 /); vp(ix) = va4
#elif defined(READMAIN21)
      if (ix == -1) then
        ix = ix + 1
        vk(ix) = va3
        uij(ix) = 0
      else if (vk(ix) /= va3) then
        ix = ix + 1
        vk(ix) = va3
        uij(ix) = 0
      end if
      vij(0:1, uij(ix), ix) = (/ va1, va2 /); vijp(uij(ix), ix) = va4
      uij(ix) = uij(ix) + 1
#elif defined(READMAIN12)
      if (ix == -1) then
        ix = ix + 1
        vjk(0:1, ix) = (/ va2, va3 /)
        ui(ix) = 0
      elseif (vjk(0, ix) /= va2 .or. vjk(1, ix) /= va3) then
        ix = ix + 1
        vjk(0:1, ix) = (/ va2, va3 /)
        ui(ix) = 0
      end if
      vi(ui(ix), ix) = va1; vip(ui(ix), ix) = va4
      ui(ix) = ui(ix) + 1
#elif defined(READMAIN111)
      if (ix == -1) then
        ix = ix + 1
        vk(ix) = va3
        uj(ix) = 0
        vj(uj(ix), ix) = va2
        ui(uj(ix), ix) = 0
      elseif (vk(ix) /= va3) then
        ix = ix + 1
        vk(ix) = va3
        uj(ix) = 0
        vj(uj(ix), ix) = va2
        ui(uj(ix), ix) = 0
      elseif (vj(uj(ix), ix) /= va2) then
        uj(ix) = uj(ix) + 1
        vj(uj(ix), ix) = va2
        ui(uj(ix), ix) = 0
      end if
      vi(ui(uj(ix), ix), uj(ix), ix) = va1
      vip(ui(uj(ix), ix), uj(ix), ix) = va4
      ui(uj(ix), ix) = ui(uj(ix), ix) + 1
#endif
      iz = iz + 1
    end do
    write (*, *) "total number of points: ", iz

    close (12)

  end subroutine read_main
#if defined(MANYBODY) && defined(PREP)
  subroutine read_ori_manybody()
    implicit none
    integer i, ii, ix, sta
    real(8) va1, va2
    character(30) :: r_files(0:2)
    
    r_files(0:2) = (/ rab_file, rbc_file, rac_file /)
    
    do i = 0, 2
      open (13, file = r_files(i))
      mbn(i) = 0
      do while(.true.)
        read (13, *, iostat = sta) ii, va1, va2
        if (sta /= 0 .or. ii == 0) exit
        va2 = min(va2, mbvcut(i))
        mbr(mbn(i), i) = va1
        mbv(mbn(i), i) = va2
        mbn(i) = mbn(i) + 1
      end do
      close (13)
    end do
    
  end subroutine read_ori_manybody
#endif

#ifdef MANYBODY
#ifdef PREP
#define WRNAME write_manybody
#define WRACTION write
#else
#define WRNAME read_manybody
#define WRACTION read
#endif
  subroutine WRNAME()
    implicit none
    integer i
    open (20, file = manybody_file)
    WRACTION (20, *) mbn(0:2)
    do i = 0, 2
      WRACTION (20, *) mbr(0:mbn(i)-1, i)
      WRACTION (20, *) mbv(0:mbn(i)-1, i)
      WRACTION (20, *) mbx(0:mbn(i)-1, i)
    end do
    close (20)
  end subroutine WRNAME
#undef WRNAME
#undef WRACTION
#endif

#ifdef RKHS
  real(8) function factorial(x)
    implicit none
    real(8) x, z, r
    if (x < 0) then
      factorial = 1
      return
    end if
    z = x + 1
    r = sqrt(2 * m_pi / z)
    r = r * (((z + 1 / (12 * z - 1 / (10 * z))) / m_e) ** z)
    factorial = r
    return
  end function factorial

  subroutine cholesky(a, b, x, n)
    implicit none
    integer n
    real(8) a(0:n - 1, 0:n - 1), b(0:n - 1), x(0:n - 1)
    integer i, j, k
    real(8) c
    
    do k = 0, n - 1
      a(k, k) = sqrt(a(k, k))
      do i = k + 1, n - 1
        a(i, k) = a(i, k) / a(k, k)
      end do
      !$omp parallel default(shared) private(j, i)
      !$omp do schedule(static)
      do j = k + 1, n - 1
        do i = j, n - 1
          a(i, j) = a(i, j) - a(i, k) * a(j, k)
        end do
      end do
      !$omp end parallel
    end do
    b(0) = b(0) / a(0, 0)
    do i = 1, n - 1
            c = dot_product(a(i, 0:i - 1), b(0 : i - 1))
            b(i) = (b(i) - c) / a(i, i)
    end do
    x(n - 1) = b(n - 1) / a(n - 1, n - 1)
    do i = n - 2, 0, -1
            c = dot_product(a(i + 1:n - 1, i), x(i + 1:n - 1))
            x(i) = (b(i) - c) / a(i, i)
    end do
  end subroutine cholesky
#endif
#ifdef DISTANCELIKE
  real(8) function distancelike_kernel(x1, x2, r)
    implicit none
    real(8) x1, x2, xb, xs, xt
    integer r
    xb = x1; xs = x2
    if (xb < xs) then
      xt = xb; xb = xs; xs = xt
    end if
    distancelike_kernel = r_coef(0,r)*(xb ** (-(r_exp(r) + 1))) - &
      r_coef(1,r)*(xb ** (-(r_exp(r) + 2))) * xs
  end function distancelike_kernel

  subroutine distancelike_presum(gsum, n, r, x, t)
    implicit none
    integer n, t, i
    real(8) gsum(-1:4*n+2), r(0:n-1), x(0:n-1)
    gsum(-1) = 0; gsum(1) = 0
    do i = 0, n - 1
      gsum(4*i+3+0) = gsum(4*(i-1)+3+0) + x(i) * r_coef(0, t)
      gsum(4*i+3+2) = gsum(4*(i-1)+3+2) + x(i) * (-r_coef(1, t) * r(i))
    end do
    gsum(4*n) = 0; gsum(4*n+2) = 0
    do i = n - 1, 0, -1
      gsum(4*(i-1)+3+1) = gsum(4*i+3+1) + x(i) * r_coef(0, t) * r(i)**(-r_exp(t)-1)
      gsum(4*(i-1)+3+3) = gsum(4*i+3+3) + x(i) * (-r_coef(1, t) * r(i)**(-r_exp(t)-2))
    end do
  end subroutine distancelike_presum

  real(8) function distancelike_eval(gsum, n, r, x, t)
    implicit none
    integer n, t, xi
    real(8) gsum(-1:4*n+2), r(0:n-1), x
    xi = binary_search(x, r, n)
    distancelike_eval = gsum(4*xi+3+0) * x**(-r_exp(t)-1) + gsum(4*xi+3+1) &
      + gsum(4*xi+3+2) * x**(-r_exp(t)-2) + gsum(4*xi+3+3) * x
  end function distancelike_eval
#ifdef DERIVATIVE1
  real(8) function distancelike_evald(gsum, n, r, x, t)
    implicit none
    integer n, t, xi
    real(8) gsum(-1:4*n+2), r(0:n-1), x
    xi = binary_search(x, r, n)
    distancelike_evald = gsum(4*xi+3+0) * (-r_exp(t)-1) * x**(-r_exp(t)-2) &
      + gsum(4*xi+3+2) * (-r_exp(t)-2) * x**(-r_exp(t)-3) + gsum(4*xi+3+3)
  end function distancelike_evald
#endif
#ifdef DERIVATIVE2
  real(8) function distancelike_evaldd(gsum, n, r, x, t)
    implicit none
    integer n, t, xi
    real(8) gsum(-1:4*n+2), r(0:n-1), x
    xi = binary_search(x, r, n)
    distancelike_evaldd = gsum(4*xi+3+0) * (-r_exp(t)-1) * (-r_exp(t)-2) * x**(-r_exp(t)-3) &
      + gsum(4*xi+3+2) * (-r_exp(t)-2) * (-r_exp(t)-3) * x**(-r_exp(t)-4)
  end function distancelike_evaldd
#endif
#endif
#ifdef ANGLELIKE
  real(8) function anglelike_kernel(x1, x2)
    implicit none
    real(8) x1, x2, xb, xs, xt
    xb = (1 - cos(x1/180d0*m_pi))/2
    xs = (1 - cos(x2/180d0*m_pi))/2
    if (xb < xs) then
      xt = xb; xb = xs; xs = xt
    end if
    anglelike_kernel = 1 + xs*xb + 2*xs*xs*xb*(1 - xs/(3*xb))
  end function anglelike_kernel
  
  subroutine anglelike_presum(gsum, n, r, x)
    implicit none
    integer n, i
    real(8) gsum(-1:4*n+4), r(0:n-1), cr(0:n-1), x(0:n-1)
    gsum(-1) = 0; gsum(1) = 0
    cr(0:n-1) = (1-cos(r(0:n-1)/180d0*m_pi)) / 2d0
    do i = 0, n - 1
      gsum(4*i+3+0) = gsum(4*(i-1)+3+0) + x(i) * 2d0 * cr(i) * cr(i)
      gsum(4*i+3+2) = gsum(4*(i-1)+3+2) + x(i) * (-2d0/3d0 * cr(i)**3)
    end do
    gsum(4*n) = 0; gsum(4*n+2) = 0
    do i = n - 1, 0, -1
      gsum(4*(i-1)+3+1) = gsum(4*i+3+1) + x(i) * 2d0 * cr(i)
      gsum(4*(i-1)+3+3) = gsum(4*i+3+3) + x(i) * (-2d0/3d0)
    end do
    gsum(4*n+3) = 0; gsum(4*n+4) = 0
    do i = 0, n - 1
      gsum(4*n+3) = gsum(4*n+3) + x(i)
      gsum(4*n+4) = gsum(4*n+4) + x(i) * cr(i)
    end do
  end subroutine anglelike_presum
  
  real(8) function anglelike_eval(gsum, n, r, x)
    implicit none
    integer n, i, xi
    real(8) gsum(-1:4*n+4), r(0:n-1), x, cx
    xi = binary_search(x, r, n)
    cx = (1-cos(x/180d0*m_pi)) / 2d0
    anglelike_eval = gsum(4*xi+3+0) * cx + gsum(4*xi+3+1) * cx*cx &
      + gsum(4*xi+3+2) + gsum(4*xi+3+3) * cx**3 + gsum(4*n+3) + gsum(4*n+4) * cx 
  end function anglelike_eval
#ifdef DERIVATIVE1
  real(8) function anglelike_evald(gsum, n, r, x)
    implicit none
    integer n, i, xi
    real(8) gsum(-1:4*n+4), r(0:n-1), x, cx, dcx
    xi = binary_search(x, r, n)
    cx = (1-cos(x/180d0*m_pi)) / 2d0
    dcx = (m_pi/360d0)*sin(x/180d0*m_pi)
    anglelike_evald = dcx * (gsum(4*xi+3+0) + gsum(4*xi+3+1) * 2*cx &
      + gsum(4*xi+3+3) * 3*cx*cx + gsum(4*n+4))
  end function anglelike_evald
#endif
#ifdef DERIVATIVE2
  real(8) function anglelike_evaldd(gsum, n, r, x)
    implicit none
    integer n, i, xi
    real(8) gsum(-1:4*n+4), r(0:n-1), x, cx, dcx, ddcx
    xi = binary_search(x, r, n)
    cx = (1-cos(x/180d0*m_pi)) / 2d0
    dcx = (m_pi/360d0)*sin(x/180d0*m_pi)
    ddcx = (m_pi/360d0)*(m_pi/180d0)*cos(x/180d0*m_pi)
    anglelike_evaldd = ddcx * (gsum(4*xi+3+0) + gsum(4*xi+3+1) * 2*cx &
      + gsum(4*xi+3+3) * 3*cx*cx + gsum(4*n+4)) + dcx *  (gsum(4*xi+3+1) * 2 &
      + gsum(4*xi+3+3) * 6*cx)
  end function anglelike_evaldd
#endif
#endif
#ifdef READMAIN3
  subroutine rkhs3d_presum(gsum, ngs, xijk, gr, gx, gn)
    implicit none
    integer :: ngs(0:5), gn
    real(8) :: gsum(-1:4*(ngs(2)+ngs(5))+2, -1:4*(ngs(1)+ngs(4))+2, -1:4*(ngs(0)+ngs(3))+2)
    real(8) :: gr(0:2, 0:gn-1), gx(0:gn-1)
    real(8) :: xijk(0:max(ngs(0),ngs(1),ngs(2))-1, 0:2)
    
    real(8) :: xgx(0:ngs(2)-1, 0:ngs(1)-1, 0:ngs(0)-1)
    integer :: nijk(0:2), dijk(0:2), dlmn(0:2), rlmn(0:2), slmn(0:2), elmn(0:2), clmn(0:2), plmn(0:2)
    integer :: d, i, j, k, l, m, n, lr, mr, nr, dr
    integer :: ls, ms, ns, le, me, ne, ll, mm, nn
    real(8) :: val, al, bl, cx
    logical :: ql
    
    ! set coordinates of grid
    nijk = 0
    xijk = 0d0
    xgx = 0d0
    do i = 0, gn - 1
      do d = 0, 2
        l = binary_search(gr(d, i), xijk(0, d), nijk(d))
        ql = .false.
        if (l == -1) then; ql = .true.
        else if (xijk(l, d) /= gr(d, i)) then; ql = .true.
        end if
        if (ql) then
          do j = nijk(d) - 1, l + 1, -1
            xijk(j + 1, d) = xijk(j, d)
          end do
          xijk(l + 1, d) = gr(d, i)
          nijk(d) = nijk(d) + 1
        end if
      end do
    end do
    
    do i = 0, gn - 1
      xgx(binary_search(gr(2, i), xijk(0, 2), nijk(2)), binary_search(gr(1, i), xijk(0, 1), nijk(1)), &
        binary_search(gr(0, i), xijk(0, 0), nijk(0))) = gx(i)
    end do
    
    ! set initial values for sums
    do m = -1, 4 * (ngs(1) + ngs(4)) + 2
      do n = -1, 4 * (ngs(2) + ngs(5)) + 2
        gsum(n, m, -1) = 0d0
        gsum(n, m, 1) = 0d0
        gsum(n, m, 4 * ngs(0)) = 0d0
        gsum(n, m, 4 * ngs(0) + 2) = 0d0
        if (ngs(3) == 1) then
          gsum(n, m, 4 * ngs(0) + 3) = 0d0
          gsum(n, m, 4 * ngs(0) + 6) = 0d0
        end if
      end do
    end do
    do l = -1, 4 * (ngs(0) + ngs(3)) + 2
      do n = -1, 4 * (ngs(2) + ngs(5)) + 2
        gsum(n, -1, l) = 0d0
        gsum(n, 1, l) = 0d0
        gsum(n, 4 * ngs(1), l) = 0d0
        gsum(n, 4 * ngs(1) + 2, l) = 0d0
        if (ngs(4) == 1) then
          gsum(n, 4 * ngs(1) + 3, l) = 0d0
          gsum(n, 4 * ngs(1) + 6, l) = 0d0
        end if
      end do
    end do
    do l = -1, 4 * (ngs(0) + ngs(3)) + 2
      do m = -1, 4 * (ngs(1) + ngs(4)) + 2
        gsum(-1, m, l) = 0d0
        gsum(1, m, l) = 0d0
        gsum(4 * ngs(2), m, l) = 0d0
        gsum(4 * ngs(2) + 2, m, l) = 0d0
        if (ngs(5) == 1) then
          gsum(4 * ngs(2) + 3, m, l) = 0d0
          gsum(4 * ngs(2) + 6, m, l) = 0d0
        end if
      end do
    end do
  
    ! solve for sums
    do i = 0, 3 + 2*ngs(3)
      ls = mod(i, 2)*(ngs(0) - 1); le = (1-mod(i, 2))*(ngs(0) - 1); ll = 1 - mod(i, 2)*2
      if (i > 3) le = ls
      do l = ls, le, ll
        do j = 0, 3 + 2*ngs(4)
          ms = mod(j, 2)*(ngs(1) - 1); me = (1-mod(j, 2))*(ngs(1) - 1); mm = 1 - mod(j, 2)*2
          if (j > 3) me = ms
          do m = ms, me, mm
            do k = 0, 3 + 2*ngs(5)
              ns = mod(k, 2)*(ngs(2) - 1); ne = (1-mod(k, 2))*(ngs(2) - 1); nn = 1 - mod(k, 2)*2
              if (k > 3) ne = ns
              do n = ns, ne, nn
                val = 1d0
                dijk = (/ i, j, k /); dlmn = (/ l, m, n /)
                slmn = dlmn; elmn = dlmn
                clmn = (/ 4*(l-mod(i,2)) + i + 3, 4*(m-mod(j,2)) + j + 3, 4*(n-mod(k,2)) + k + 3 /)
                plmn = (/ 4*(l-1+mod(i,2)) + i + 3, 4*(m-1+mod(j,2)) + j + 3, 4*(n-1+mod(k,2)) + k + 3 /)
                do d = 0, 2
                  if (dijk(d) <= 3) then
                    if (ngs(d+3) /= 1) then ! distancelike
                      dr = d + 1
                      if (dijk(d) == 0) then
                        val = val * r_coef(0, dr)
                      else if (dijk(d) == 1) then
                        val = val * r_coef(0, dr) * xijk(dlmn(d), d)**(-r_exp(dr) - 1)
                      else if (dijk(d) == 2) then
                        val = val * (-r_coef(1, dr)) * xijk(dlmn(d), d)
                      else if (dijk(d) == 3) then
                        val = val * (-r_coef(1, dr)) * xijk(dlmn(d), d)**(-r_exp(dr) - 2)
                      end if
                    else ! anglelike
                      cx = (1-cos(xijk(dlmn(d), d)/180d0*m_pi)) / 2d0
                      if (dijk(d) == 0) then
                        val = val * 2d0 * cx*cx
                      else if (dijk(d) == 1) then
                        val = val * 2d0 * cx
                      else if (dijk(d) == 2) then
                        val = val * (-2d0/3d0) * cx**3
                      else if (dijk(d) == 3) then
                        val = val * (-2d0/3d0)
                      end if
                    end if
                  else
                    slmn(d) = 0; elmn(d) = ngs(d) - 1
                    clmn(d) = 4*ngs(d) + (dijk(d)*2 - 4) - mod(dijk(d), 2)
                    plmn(d) = 4*ngs(d) + (dijk(d)*2 - 4) - 1 + mod(dijk(d), 2)
                  end if
                end do
                
                al = 0d0
                do lr = slmn(0), elmn(0)
                  do mr = slmn(1), elmn(1)
                    do nr = slmn(2), elmn(2)
                      bl = xgx(nr, mr, lr)
                      rlmn = (/ lr, mr, nr /)
                      do d = 0, 2
                        if (ngs(d+3) == 1) then ! anglelike
                          if (dijk(d) == 5) then
                            cx = (1-cos(xijk(rlmn(d), d)/180d0*m_pi)) / 2d0
                            bl = bl * cx
                          end if
                        end if
                      end do
                      al = al + bl
                    end do
                  end do
                end do
                val = val * al
                
                gsum(clmn(2), clmn(1), clmn(0)) = val + gsum(plmn(2), plmn(1), plmn(0)) &
                  + gsum(plmn(2), clmn(1), clmn(0)) + gsum(clmn(2), plmn(1), clmn(0)) + &
                  gsum(clmn(2), clmn(1), plmn(0)) &
                  - gsum(clmn(2), plmn(1), plmn(0)) - gsum(plmn(2), clmn(1), plmn(0)) - &
                  gsum(plmn(2), plmn(1), clmn(0))
              end do
            end do
          end do
        end do
      end do
    end do
  end subroutine rkhs3d_presum
  
  real(8) function rkhs3d_eval(gsum, ngs, xijk, xi, xj, xk)
    implicit none
    real(8) :: xi, xj, xk, cxi, cxj, cxk
    integer :: ngs(0:5)
    real(8) :: gsum(-1:4*(ngs(2)+ngs(5))+2, -1:4*(ngs(1)+ngs(4))+2, -1:4*(ngs(0)+ngs(3))+2)
    real(8) :: xijk(0:max(ngs(0),ngs(1),ngs(2))-1, 0:2)
    
    integer ii, jj, kk, i, j, gi, gj
    real(8) :: ggamma(0:3+2*ngs(4), 0:3+2*ngs(3)), hgamma(0:3+2*ngs(3))
    
    ii = binary_search(xi, xijk(0, 0), ngs(0))
    jj = binary_search(xj, xijk(0, 1), ngs(1))
    kk = binary_search(xk, xijk(0, 2), ngs(2))
    if (ngs(3) == 1) cxi = (1-cos(xi/180d0*m_pi))/2d0
    if (ngs(4) == 1) cxj = (1-cos(xj/180d0*m_pi))/2d0
    if (ngs(5) == 1) cxk = (1-cos(xk/180d0*m_pi))/2d0
    do i = 0, 3+2*ngs(3)
      gi = 4*ii + 3 + i
      if (i > 3) gi = 4*ngs(0) + i
      do j = 0, 3+2*ngs(4)
        gj = 4*jj + 3 + j
        if (j > 3) gj = 4*ngs(1) + j
        if (ngs(5) == 0) then
          ggamma(j, i) = gsum(4*kk + 3 + 0, gj, gi) * xk**(-r_exp(3) - 1) + gsum(4*kk + 3 + 1, gj, gi) &
            + gsum(4*kk + 3 + 2, gj, gi) * xk**(-r_exp(3) - 2) + gsum(4*kk + 3 + 3, gj, gi) * xk
        else
          ggamma(j, i) = gsum(4*kk + 3 + 0, gj, gi) * cxk + gsum(4*kk + 3 + 1, gj, gi) * cxk**2 &
            + gsum(4*kk + 3 + 2, gj, gi) + gsum(4*kk + 3 + 3, gj, gi) * cxk**3 &
            + gsum(4*ngs(2) + 4, gj, gi) + gsum(4*ngs(2) + 5, gj, gi) * cxk
        end if
      end do
      if (ngs(4) == 0) then
        hgamma(i) = ggamma(0, i) * xj**(-r_exp(2) - 1) + ggamma(1, i) &
          + ggamma(2, i) * xj**(-r_exp(2) - 2) + ggamma(3, i) * xj
      else
        hgamma(i) = ggamma(0, i) * cxj + ggamma(1, i) * cxj**2 + ggamma(2, i) &
          + ggamma(3, i) * cxj**3 + ggamma(4, i) + ggamma(5, i) * cxj
      end if
    end do
    if (ngs(3) == 0) then
      rkhs3d_eval = hgamma(0) * xi**(-r_exp(1) - 1) + hgamma(1) + &
        hgamma(2) * xi**(-r_exp(1) - 2) + hgamma(3) * xi
    else
      rkhs3d_eval = hgamma(0) * cxi + hgamma(1) * cxi**2 + hgamma(2) + &
        hgamma(3) * cxi**3 + hgamma(4) + hgamma(5) * cxi
    end if
  end function rkhs3d_eval
#ifdef DERIVATIVE1
  function rkhs3d_evald(gsum, ngs, xijk, xi, xj, xk)
    implicit none
    real(8) xi, xj, xk, cxi, cxj, cxk, dcxi, dcxj, dcxk
    integer :: ngs(0:5)
    real(8) :: gsum(-1:4*(ngs(2)+ngs(5))+2, -1:4*(ngs(1)+ngs(4))+2, -1:4*(ngs(0)+ngs(3))+2)
    real(8) :: xijk(0:max(ngs(0),ngs(1),ngs(2))-1, 0:2)
    real(8) :: rkhs3d_evald(0:2)
    
    integer ii, jj, kk, i, j, gi, gj
    real(8) :: ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 0:2), hgamma(0:3+2*ngs(3), 0:2)
    
    ii = binary_search(xi, xijk(0, 0), ngs(0))
    jj = binary_search(xj, xijk(0, 1), ngs(1))
    kk = binary_search(xk, xijk(0, 2), ngs(2))
    if (ngs(3) == 1) then
      cxi = (1-cos(xi/180d0*m_pi))/2d0
      dcxi = (m_pi/360d0)*sin(xi/180d0*m_pi)
    end if
    if (ngs(4) == 1) then
      cxj = (1-cos(xj/180d0*m_pi))/2d0
      dcxj = (m_pi/360d0)*sin(xj/180d0*m_pi)
    end if
    if (ngs(5) == 1) then
      cxk = (1-cos(xk/180d0*m_pi))/2d0
      dcxk = (m_pi/360d0)*sin(xk/180d0*m_pi)
    end if
    do i = 0, 3+2*ngs(3)
      gi = 4*ii + 3 + i
      if (i > 3) gi = 4*ngs(0) + i
      do j = 0, 3+2*ngs(4)
        gj = 4*jj + 3 + j
        if (j > 3) gj = 4*ngs(1) + j
        if (ngs(5) == 0) then
          ggamma(j, i, 0) = gsum(4*kk + 3 + 0, gj, gi) * xk**(-r_exp(3) - 1) + gsum(4*kk + 3 + 1, gj, gi) &
            + gsum(4*kk + 3 + 2, gj, gi) * xk**(-r_exp(3) - 2) + gsum(4*kk + 3 + 3, gj, gi) * xk
          ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 1) = ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 0)
          ggamma(j, i, 2) = gsum(4*kk + 3 + 0, gj, gi) * (-r_exp(3)-1)*xk**(-r_exp(3)-2) &
            + gsum(4*kk + 3 + 2, gj, gi) * (-r_exp(3)-2)*xk**(-r_exp(3)-3) + gsum(4*kk + 3 + 3, gj, gi)
        else
          ggamma(j, i, 0) = gsum(4*kk + 3 + 0, gj, gi) * cxk + gsum(4*kk + 3 + 1, gj, gi) * cxk**2 &
            + gsum(4*kk + 3 + 2, gj, gi) + gsum(4*kk + 3 + 3, gj, gi) * cxk**3 &
            + gsum(4*ngs(2) + 4, gj, gi) + gsum(4*ngs(2) + 5, gj, gi) * cxk
          ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 1) = ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 0)
          ggamma(j, i, 2) = dcxk * (gsum(4*kk + 3 + 0, gj, gi) + gsum(4*kk + 3 + 1, gj, gi) * 2*cxk &
            + gsum(4*kk + 3 + 3, gj, gi) * 3*cxk**2 + gsum(4*ngs(2) + 5, gj, gi))
        end if
      end do
      if (ngs(4) == 0) then
        hgamma(i, 0) = ggamma(0, i, 0) * xj**(-r_exp(2) - 1) + ggamma(1, i, 0) &
          + ggamma(2, i, 0) * xj**(-r_exp(2) - 2) + ggamma(3, i, 0) * xj
        hgamma(i, 1) = ggamma(0, i, 1) * (-r_exp(2)-1)*xj**(-r_exp(2)-2) &
          + ggamma(2, i, 1) * (-r_exp(2)-2)*xj**(-r_exp(2)-3) + ggamma(3, i, 1)
        hgamma(i, 2) = ggamma(0, i, 2) * xj**(-r_exp(2) - 1) + ggamma(1, i, 2) &
          + ggamma(2, i, 2) * xj**(-r_exp(2) - 2) + ggamma(3, i, 2) * xj
      else
        hgamma(i, 0) = ggamma(0, i, 0) * cxj + ggamma(1, i, 0) * cxj**2 + ggamma(2, i, 0) &
          + ggamma(3, i, 0) * cxj**3 + ggamma(4, i, 0) + ggamma(5, i, 0) * cxj
        hgamma(i, 1) = dcxj * (ggamma(0, i, 1) + ggamma(1, i, 1) * 2*cxj &
          + ggamma(3, i, 1) * 3*cxj**2 + ggamma(5, i, 1))
        hgamma(i, 2) = ggamma(0, i, 2) * cxj + ggamma(1, i, 2) * cxj**2 + ggamma(2, i, 2) &
          + ggamma(3, i, 2) * cxj**3 + ggamma(4, i, 2) + ggamma(5, i, 2) * cxj
      end if
    end do
    if (ngs(3) == 0) then
      rkhs3d_evald(0) = hgamma(0, 0) * (-r_exp(1)-1)*xi**(-r_exp(1)-2) &
        + hgamma(2, 0) * (-r_exp(1)-2)*xi**(-r_exp(1)-3) + hgamma(3, 0)
      rkhs3d_evald(1) = hgamma(0, 1) * xi**(-r_exp(1) - 1) + hgamma(1, 1) &
        + hgamma(2, 1) * xi**(-r_exp(1) - 2) + hgamma(3, 1) * xi
      rkhs3d_evald(2) = hgamma(0, 2) * xi**(-r_exp(1) - 1) + hgamma(1, 2) &
        + hgamma(2, 2) * xi**(-r_exp(1) - 2) + hgamma(3, 2) * xi
    else
      rkhs3d_evald(0) = dcxi * (hgamma(0, 0) + hgamma(1, 0) * 2*cxi + hgamma(3, 0) * 3*cxi**2 + hgamma(5, 0))
      rkhs3d_evald(1) = hgamma(0, 1) * cxi + hgamma(1, 1) * cxi**2 + hgamma(2, 1) &
        + hgamma(3, 1) * cxi**3 + hgamma(4, 1) + hgamma(5, 1) * cxi
      rkhs3d_evald(2) = hgamma(0, 2) * cxi + hgamma(1, 2) * cxi**2 + hgamma(2, 2) &
        + hgamma(3, 2) * cxi**3 + hgamma(4, 2) + hgamma(5, 2) * cxi
    end if
  end function rkhs3d_evald
#endif
#ifdef DERIVATIVE2
  function rkhs3d_evaldd(gsum, ngs, xijk, xi, xj, xk)
    implicit none
    real(8) xi, xj, xk, cxi, cxj, cxk, dcxi, dcxj, dcxk, ddcxi, ddcxj, ddcxk
    integer :: ngs(0:5)
    real(8) :: gsum(-1:4*(ngs(2)+ngs(5))+2, -1:4*(ngs(1)+ngs(4))+2, -1:4*(ngs(0)+ngs(3))+2)
    real(8) :: xijk(0:max(ngs(0),ngs(1),ngs(2))-1, 0:2)
    real(8) :: rkhs3d_evaldd(0:5) ! 0:ii 1:ij 2:ik 3:jj 4:jk 5:kk
    
    integer ii, jj, kk, i, j, gi, gj
    real(8) :: ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 0:5), hgamma(0:3+2*ngs(3), 0:5)
    
    ii = binary_search(xi, xijk(0, 0), ngs(0))
    jj = binary_search(xj, xijk(0, 1), ngs(1))
    kk = binary_search(xk, xijk(0, 2), ngs(2))
    if (ngs(3) == 1) then
      cxi = (1-cos(xi/180d0*m_pi))/2d0
      dcxi = (m_pi/360d0)*sin(xi/180d0*m_pi)
      ddcxi = (m_pi/360d0)*(m_pi/180d0)*cos(xi/180d0*m_pi)
    end if
    if (ngs(4) == 1) then
      cxj = (1-cos(xj/180d0*m_pi))/2d0
      dcxj = (m_pi/360d0)*sin(xj/180d0*m_pi)
      ddcxj = (m_pi/360d0)*(m_pi/180d0)*cos(xj/180d0*m_pi)
    end if
    if (ngs(5) == 1) then
      cxk = (1-cos(xk/180d0*m_pi))/2d0
      dcxk = (m_pi/360d0)*sin(xk/180d0*m_pi)
      ddcxk = (m_pi/360d0)*(m_pi/180d0)*cos(xk/180d0*m_pi)
    end if
    do i = 0, 3+2*ngs(3)
      gi = 4*ii + 3 + i
      if (i > 3) gi = 4*ngs(0) + i
      do j = 0, 3+2*ngs(4)
        gj = 4*jj + 3 + j
        if (j > 3) gj = 4*ngs(1) + j
        if (ngs(5) == 0) then
          ggamma(j, i, 0) = gsum(4*kk + 3 + 0, gj, gi) * xk**(-r_exp(3) - 1) + gsum(4*kk + 3 + 1, gj, gi) &
            + gsum(4*kk + 3 + 2, gj, gi) * xk**(-r_exp(3) - 2) + gsum(4*kk + 3 + 3, gj, gi) * xk
          ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 1) = ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 0)
          ggamma(j, i, 2) = gsum(4*kk + 3 + 0, gj, gi) * (-r_exp(3)-1)*xk**(-r_exp(3)-2) &
            + gsum(4*kk + 3 + 2, gj, gi) * (-r_exp(3)-2)*xk**(-r_exp(3)-3) + gsum(4*kk + 3 + 3, gj, gi)
          ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 3) = ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 0)
          ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 4) = ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 2)
          ggamma(j, i, 5) = gsum(4*kk + 3 + 0, gj, gi) * (-r_exp(3)-1)*(-r_exp(3)-2)*xk**(-r_exp(3)-3) &
            + gsum(4*kk + 3 + 2, gj, gi) * (-r_exp(3)-2)*(-r_exp(3)-3)*xk**(-r_exp(3)-4)
        else
          ggamma(j, i, 0) = gsum(4*kk + 3 + 0, gj, gi) * cxk + gsum(4*kk + 3 + 1, gj, gi) * cxk**2 &
            + gsum(4*kk + 3 + 2, gj, gi) + gsum(4*kk + 3 + 3, gj, gi) * cxk**3 &
            + gsum(4*ngs(2) + 4, gj, gi) + gsum(4*ngs(2) + 5, gj, gi) * cxk
          ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 1) = ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 0)
          ggamma(j, i, 2) = dcxk * (gsum(4*kk + 3 + 0, gj, gi) + gsum(4*kk + 3 + 1, gj, gi) * 2*cxk &
            + gsum(4*kk + 3 + 3, gj, gi) * 3*cxk**2 + gsum(4*ngs(2) + 5, gj, gi))
          ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 3) = ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 0)
          ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 4) = ggamma(0:3+2*ngs(4), 0:3+2*ngs(3), 2)
          ggamma(j, i, 5) = ddcxk * (gsum(4*kk + 3 + 0, gj, gi) + gsum(4*kk + 3 + 1, gj, gi) * 2*cxk &
            + gsum(4*kk + 3 + 3, gj, gi) * 3*cxk**2 + gsum(4*ngs(2) + 5, gj, gi)) &
            + dcxk * (gsum(4*kk + 3 + 1, gj, gi) * 2 + gsum(4*kk + 3 + 3, gj, gi) * 6*cxk)
        end if
      end do
      if (ngs(4) == 0) then
        hgamma(i, 0) = ggamma(0, i, 0) * xj**(-r_exp(2) - 1) + ggamma(1, i, 0) &
          + ggamma(2, i, 0) * xj**(-r_exp(2) - 2) + ggamma(3, i, 0) * xj
        hgamma(i, 1) = ggamma(0, i, 1) * (-r_exp(2)-1)*xj**(-r_exp(2)-2) &
          + ggamma(2, i, 1) * (-r_exp(2)-2)*xj**(-r_exp(2)-3) + ggamma(3, i, 1)
        hgamma(i, 2) = ggamma(0, i, 2) * xj**(-r_exp(2) - 1) + ggamma(1, i, 2) + &
          ggamma(2, i, 2) * xj**(-r_exp(2) - 2) + ggamma(3, i, 2) * xj
        hgamma(i, 3) = ggamma(0, i, 3) * (-r_exp(2)-1)*(-r_exp(2)-2)*xj**(-r_exp(2)-3) &
          + ggamma(2, i, 3) * (-r_exp(2)-2)*(-r_exp(2)-3)*xj**(-r_exp(2)-4)
        hgamma(i, 4) = ggamma(0, i, 4) * (-r_exp(2)-1)*xj**(-r_exp(2)-2) + &
          ggamma(2, i, 4) * (-r_exp(2)-2)*xj**(-r_exp(2)-3) + ggamma(3, i, 4)
        hgamma(i, 5) = ggamma(0, i, 5) * xj**(-r_exp(2) - 1) + ggamma(1, i, 5) &
          + ggamma(2, i, 5) * xj**(-r_exp(2) - 2) + ggamma(3, i, 5) * xj
      else
        hgamma(i, 0) = ggamma(0, i, 0) * cxj + ggamma(1, i, 0) * cxj**2 + ggamma(2, i, 0) &
          + ggamma(3, i, 0) * cxj**3 + ggamma(4, i, 0) + ggamma(5, i, 0) * cxj
        hgamma(i, 1) = dcxj * (ggamma(0, i, 1) + ggamma(1, i, 1) * 2*cxj &
          + ggamma(3, i, 1) * 3*cxj**2 + ggamma(5, i, 1))
        hgamma(i, 2) = ggamma(0, i, 2) * cxj + ggamma(1, i, 2) * cxj**2 + ggamma(2, i, 2) &
          + ggamma(3, i, 2) * cxj**3 + ggamma(4, i, 2) + ggamma(5, i, 2) * cxj
        hgamma(i, 3) = ddcxj * (ggamma(0, i, 3) + ggamma(1, i, 3) * 2*cxj &
          + ggamma(3, i, 3) * 3*cxj**2 + ggamma(5, i, 3)) &
          + dcxj * (ggamma(1, i, 3) * 2 + ggamma(3, i, 3) * 6*cxj)
        hgamma(i, 4) = dcxj * (ggamma(0, i, 4) + ggamma(1, i, 4) * 2*cxj &
          + ggamma(3, i, 4) * 3*cxj**2 + ggamma(5, i, 4))
        hgamma(i, 5) = ggamma(0, i, 5) * cxj + ggamma(1, i, 5) * cxj**2 + ggamma(2, i, 5) &
          + ggamma(3, i, 5) * cxj**3 + ggamma(4, i, 5) + ggamma(5, i, 5) * cxj
      end if
    end do
    if (ngs(3) == 0) then
      rkhs3d_evaldd(0) = hgamma(0, 0) * (-r_exp(1)-1)*(-r_exp(1)-2)*xi**(-r_exp(1)-3) &
        + hgamma(2, 0) * (-r_exp(1)-2)*(-r_exp(1)-3)*xi**(-r_exp(1)-4)
      rkhs3d_evaldd(1) = hgamma(0, 1) * (-r_exp(1)-1)*xi**(-r_exp(1)-2) + &
        hgamma(2, 1) * (-r_exp(1)-2)*xi**(-r_exp(1)-3) + hgamma(3, 1)
      rkhs3d_evaldd(2) = hgamma(0, 2) * (-r_exp(1)-1)*xi**(-r_exp(1)-2) + &
        hgamma(2, 2) * (-r_exp(1)-2)*xi**(-r_exp(1)-3) + hgamma(3, 2)
      rkhs3d_evaldd(3) = hgamma(0, 3) * xi**(-r_exp(1) - 1) + hgamma(1, 3) &
        + hgamma(2, 3) * xi**(-r_exp(1) - 2) + hgamma(3, 3) * xi
      rkhs3d_evaldd(4) = hgamma(0, 4) * xi**(-r_exp(1) - 1) + hgamma(1, 4) &
        + hgamma(2, 4) * xi**(-r_exp(1) - 2) + hgamma(3, 4) * xi
      rkhs3d_evaldd(5) = hgamma(0, 5) * xi**(-r_exp(1) - 1) + hgamma(1, 5) &
        + hgamma(2, 5) * xi**(-r_exp(1) - 2) + hgamma(3, 5) * xi
    else
      rkhs3d_evaldd(0) = ddcxi * (hgamma(0, 0) + hgamma(1, 0) * 2*cxi + hgamma(3, 0) * 3*cxi**2 &
        + hgamma(5, 0)) + dcxi * (hgamma(1, 0) * 2 + hgamma(3, 0) * 6*cxi)
      rkhs3d_evaldd(1) = dcxi * (hgamma(0, 1) + hgamma(1, 1) * 2*cxi + hgamma(3, 1) * 3*cxi**2 + hgamma(5, 1))
      rkhs3d_evaldd(2) = dcxi * (hgamma(0, 2) + hgamma(1, 2) * 2*cxi + hgamma(3, 2) * 3*cxi**2 + hgamma(5, 2))
      rkhs3d_evaldd(3) = hgamma(0, 3) * cxi + hgamma(1, 3) * cxi**2 &
        + hgamma(2, 3) + hgamma(3, 3) * cxi**3 + hgamma(4, 3) + hgamma(5, 3) * cxi
      rkhs3d_evaldd(4) = hgamma(0, 4) * cxi + hgamma(1, 4) * cxi**2 &
        + hgamma(2, 4) + hgamma(3, 4) * cxi**3 + hgamma(4, 4) + hgamma(5, 4) * cxi
      rkhs3d_evaldd(5) = hgamma(0, 5) * cxi + hgamma(1, 5) * cxi**2 &
        + hgamma(2, 5) + hgamma(3, 5) * cxi**3 + hgamma(4, 5) + hgamma(5, 5) * cxi
    end if
  end function rkhs3d_evaldd
#endif
#endif
#if defined(READMAIN21) || defined(READMAIN12)
  subroutine rkhs2d_presum(gsum, ngs, xij, gr, gx, gn, dr)
    implicit none
    integer :: ngs(0:3), gn
    real(8) :: gsum(-1:4*(ngs(1)+ngs(3))+2, -1:4*(ngs(0)+ngs(2))+2)
    real(8) :: gr(0:1, 0:gn-1), gx(0:gn-1)
    real(8) :: xij(0:max(ngs(0),ngs(1))-1, 0:1)
    
    real(8) :: xgx(0:ngs(1)-1, 0:ngs(0)-1)
    integer :: nij(0:1), dij(0:1), dlm(0:1), rlm(0:1), slm(0:1), elm(0:1), clm(0:1), plm(0:1), dr(0:1)
    integer :: d, i, j, l, m, lr, mr, ls, ms, le, me, ll, mm
    real(8) :: val, al, bl, cx
    logical :: ql
    
    ! set coordinates of grid
    nij = 0
    xij = 0d0
    xgx = 0d0
    do i = 0, gn - 1
      do d = 0, 1
        l = binary_search(gr(d, i), xij(0, d), nij(d))
        ql = .false.
        if (l == -1) then; ql = .true.
        else if (xij(l, d) /= gr(d, i)) then; ql = .true.
        end if
        if (ql) then
          do j = nij(d) - 1, l + 1, -1
            xij(j + 1, d) = xij(j, d)
          end do
          xij(l + 1, d) = gr(d, i)
          nij(d) = nij(d) + 1
        end if
      end do
    end do
    
    do i = 0, gn - 1
      xgx(binary_search(gr(1, i), xij(0, 1), nij(1)), binary_search(gr(0, i), xij(0, 0), nij(0))) = gx(i)
    end do
    
    ! set initial values for sums
    do m = -1, 4 * (ngs(1) + ngs(3)) + 2
      gsum(m, -1) = 0d0
      gsum(m, 1) = 0d0
      gsum(m, 4 * ngs(0)) = 0d0
      gsum(m, 4 * ngs(0) + 2) = 0d0
      if (ngs(2) == 1) then
        gsum(m, 4 * ngs(0) + 3) = 0d0
        gsum(m, 4 * ngs(0) + 6) = 0d0
      end if
    end do
    do l = -1, 4 * (ngs(0) + ngs(2)) + 2
      gsum(-1, l) = 0d0
      gsum(1, l) = 0d0
      gsum(4 * ngs(1), l) = 0d0
      gsum(4 * ngs(1) + 2, l) = 0d0
      if (ngs(3) == 1) then
        gsum(4 * ngs(1) + 3, l) = 0d0
        gsum(4 * ngs(1) + 6, l) = 0d0
      end if
    end do
  
    ! solve for sums
    do i = 0, 3 + 2*ngs(2)
      ls = mod(i, 2)*(ngs(0) - 1); le = (1-mod(i, 2))*(ngs(0) - 1); ll = 1 - mod(i, 2)*2
      if (i > 3) le = ls
      do l = ls, le, ll
        do j = 0, 3 + 2*ngs(3)
          ms = mod(j, 2)*(ngs(1) - 1); me = (1-mod(j, 2))*(ngs(1) - 1); mm = 1 - mod(j, 2)*2
          if (j > 3) me = ms
          do m = ms, me, mm
            val = 1d0
            dij = (/ i, j /); dlm = (/ l, m /)
            slm = dlm; elm = dlm
            clm = (/ 4*(l-mod(i,2)) + i + 3, 4*(m-mod(j,2)) + j + 3 /)
            plm = (/ 4*(l-1+mod(i,2)) + i + 3, 4*(m-1+mod(j,2)) + j + 3 /)
            do d = 0, 1
              if (dij(d) <= 3) then
                if (ngs(d+2) /= 1) then ! distancelike
                  if (dij(d) == 0) then
                    val = val * r_coef(0, dr(d))
                  else if (dij(d) == 1) then
                    val = val * r_coef(0, dr(d)) * xij(dlm(d), d)**(-r_exp(dr(d)) - 1)
                  else if (dij(d) == 2) then
                    val = val * (-r_coef(1, dr(d))) * xij(dlm(d), d)
                  else if (dij(d) == 3) then
                    val = val * (-r_coef(1, dr(d))) * xij(dlm(d), d)**(-r_exp(dr(d)) - 2)
                  end if
                else ! anglelike
                  cx = (1-cos(xij(dlm(d), d)/180d0*m_pi)) / 2d0
                  if (dij(d) == 0) then
                    val = val * 2d0 * cx*cx
                  else if (dij(d) == 1) then
                    val = val * 2d0 * cx
                  else if (dij(d) == 2) then
                    val = val * (-2d0/3d0) * cx**3
                  else if (dij(d) == 3) then
                    val = val * (-2d0/3d0)
                  end if
                end if
              else
                slm(d) = 0; elm(d) = ngs(d) - 1
                clm(d) = 4*ngs(d) + (dij(d)*2 - 4) - mod(dij(d), 2)
                plm(d) = 4*ngs(d) + (dij(d)*2 - 4) - 1 + mod(dij(d), 2)
              end if
            end do
            
            al = 0d0
            do lr = slm(0), elm(0)
              do mr = slm(1), elm(1)
                bl = xgx(mr, lr)
                rlm = (/ lr, mr /)
                do d = 0, 1
                  if (ngs(d+2) == 1) then ! anglelike
                    if (dij(d) == 5) then
                      cx = (1-cos(xij(rlm(d), d)/180d0*m_pi)) / 2d0
                      bl = bl * cx
                    end if
                  end if
                end do
                al = al + bl
              end do
            end do
            val = val * al
            
            gsum(clm(1), clm(0)) = val + gsum(clm(1), plm(0)) + gsum(plm(1), clm(0)) - gsum(plm(1), plm(0))
          end do
        end do
      end do
    end do
  end subroutine rkhs2d_presum
  
  real(8) function rkhs2d_eval(gsum, ngs, xij, dr, xi, xj)
    implicit none
    real(8) :: xi, xj, cxi, cxj
    integer :: ngs(0:3), dr(0:1)
    real(8) :: gsum(-1:4*(ngs(1)+ngs(3))+2, -1:4*(ngs(0)+ngs(2))+2)
    real(8) :: xij(0:max(ngs(0),ngs(1))-1, 0:1)
    
    integer :: ii, jj, i, gi
    real(8) :: ggamma(0:3+2*ngs(2))
    
    ii = binary_search(xi, xij(0, 0), ngs(0))
    jj = binary_search(xj, xij(0, 1), ngs(1))
    if (ngs(2) == 1) cxi = (1-cos(xi/180d0*m_pi))/2d0
    if (ngs(3) == 1) cxj = (1-cos(xj/180d0*m_pi))/2d0
    do i = 0, 3+2*ngs(2)
      gi = 4*ii + 3 + i
      if (i > 3) gi = 4*ngs(0) + i
      if (ngs(3) == 0) then
        ggamma(i) = gsum(4*jj + 3 + 0, gi) * xj**(-r_exp(dr(1)) - 1) + gsum(4*jj + 3 + 1, gi) &
          + gsum(4*jj + 3 + 2, gi) * xj**(-r_exp(dr(1)) - 2) + gsum(4*jj + 3 + 3, gi) * xj
      else
        ggamma(i) = gsum(4*jj + 3 + 0, gi) * cxj + gsum(4*jj + 3 + 1, gi) * cxj**2 + gsum(4*jj + 3 + 2, gi) &
          + gsum(4*jj + 3 + 3, gi) * cxj**3 + gsum(4*ngs(1) + 4, gi) + gsum(4*ngs(1) + 5, gi) * cxj
      end if
    end do
    if (ngs(2) == 0) then
      rkhs2d_eval = ggamma(0) * xi**(-r_exp(dr(0)) - 1) + ggamma(1) &
        + ggamma(2) * xi**(-r_exp(dr(0)) - 2) + ggamma(3) * xi
    else
      rkhs2d_eval = ggamma(0) * cxi + ggamma(1) * cxi**2 + ggamma(2) &
        + ggamma(3) * cxi**3 + ggamma(4) + ggamma(5) * cxi
    end if
  end function rkhs2d_eval
#ifdef DERIVATIVE1
  function rkhs2d_evald(gsum, ngs, xij, dr, xi, xj)
    implicit none
    real(8) :: xi, xj, cxi, cxj, dcxi, dcxj
    integer :: ngs(0:3), dr(0:1)
    real(8) :: gsum(-1:4*(ngs(1)+ngs(3))+2, -1:4*(ngs(0)+ngs(2))+2)
    real(8) :: xij(0:max(ngs(0),ngs(1))-1, 0:1)
    real(8) :: rkhs2d_evald(0:1)
    
    integer :: ii, jj, i, gi
    real(8) :: ggamma(0:3+2*ngs(2), 0:1)
    
    ii = binary_search(xi, xij(0, 0), ngs(0))
    jj = binary_search(xj, xij(0, 1), ngs(1))
    if (ngs(2) == 1) then
      cxi = (1-cos(xi/180d0*m_pi))/2d0
      dcxi = (m_pi/360d0)*sin(xi/180d0*m_pi)
    end if
    if (ngs(3) == 1) then
      cxj = (1-cos(xj/180d0*m_pi))/2d0
      dcxj = (m_pi/360d0)*sin(xj/180d0*m_pi)
    end if
    do i = 0, 3+2*ngs(2)
      gi = 4*ii + 3 + i
      if (i > 3) gi = 4*ngs(0) + i
      if (ngs(3) == 0) then
        ggamma(i, 0) = gsum(4*jj + 3 + 0, gi) * xj**(-r_exp(dr(1)) - 1) + gsum(4*jj + 3 + 1, gi) &
          + gsum(4*jj + 3 + 2, gi) * xj**(-r_exp(dr(1)) - 2) + gsum(4*jj + 3 + 3, gi) * xj
        ggamma(i, 1) = gsum(4*jj + 3 + 0, gi) * (-r_exp(dr(1))-1)*xj**(-r_exp(dr(1))-2) &
          + gsum(4*jj + 3 + 2, gi) * (-r_exp(dr(1))-2)*xj**(-r_exp(dr(1))-3) + gsum(4*jj + 3 + 3, gi)
      else
        ggamma(i, 0) = gsum(4*jj + 3 + 0, gi) * cxj + gsum(4*jj + 3 + 1, gi) * cxj**2 + gsum(4*jj + 3 + 2, gi) &
          + gsum(4*jj + 3 + 3, gi) * cxj**3 + gsum(4*ngs(1) + 4, gi) + gsum(4*ngs(1) + 5, gi) * cxj
        ggamma(i, 1) = dcxj * (gsum(4*jj + 3 + 0, gi) + gsum(4*jj + 3 + 1, gi) * 2*cxj &
          + gsum(4*jj + 3 + 3, gi) * 3*cxj**2 + gsum(4*ngs(1) + 5, gi))
      end if
    end do
    if (ngs(2) == 0) then
      rkhs2d_evald(0) = ggamma(0, 0) * (-r_exp(dr(0))-1)*xi**(-r_exp(dr(0))-2) &
        + ggamma(2, 0) * (-r_exp(dr(0))-2)*xi**(-r_exp(dr(0))-3) + ggamma(3, 0)
      rkhs2d_evald(1) = ggamma(0, 1) * xi**(-r_exp(dr(0)) - 1) + ggamma(1, 1) &
        + ggamma(2, 1) * xi**(-r_exp(dr(0)) - 2) + ggamma(3, 1) * xi
    else
      rkhs2d_evald(0) = dcxi * (ggamma(0, 0) + ggamma(1, 0) * 2*cxi + ggamma(3, 0) * 3*cxi**2 + ggamma(5, 0))
      rkhs2d_evald(1) = ggamma(0, 1) * cxi + ggamma(1, 1) * cxi**2 + ggamma(2, 1) &
        + ggamma(3, 1) * cxi**3 + ggamma(4, 1) + ggamma(5, 1) * cxi
    end if
  end function rkhs2d_evald
#endif
#ifdef DERIVATIVE2
  function rkhs2d_evaldd(gsum, ngs, xij, dr, xi, xj)
    implicit none
    real(8) :: xi, xj, cxi, cxj, dcxi, dcxj, ddcxi, ddcxj
    integer :: ngs(0:3), dr(0:1)
    real(8) :: gsum(-1:4*(ngs(1)+ngs(3))+2, -1:4*(ngs(0)+ngs(2))+2)
    real(8) :: xij(0:max(ngs(0),ngs(1))-1, 0:1)
    real(8) :: rkhs2d_evaldd(0:2) ! 0:ii 1:ij 2:jj
    
    integer :: ii, jj, i, gi
    real(8) :: ggamma(0:3+2*ngs(2), 0:2)
    
    ii = binary_search(xi, xij(0, 0), ngs(0))
    jj = binary_search(xj, xij(0, 1), ngs(1))
    if (ngs(2) == 1) then
      cxi = (1-cos(xi/180d0*m_pi))/2d0
      dcxi = (m_pi/360d0)*sin(xi/180d0*m_pi)
      ddcxi = (m_pi/360d0)*(m_pi/180d0)*cos(xi/180d0*m_pi)
    end if
    if (ngs(3) == 1) then
      cxj = (1-cos(xj/180d0*m_pi))/2d0
      dcxj = (m_pi/360d0)*sin(xj/180d0*m_pi)
      ddcxj = (m_pi/360d0)*(m_pi/180d0)*cos(xj/180d0*m_pi)
    end if
    do i = 0, 3+2*ngs(2)
      gi = 4*ii + 3 + i
      if (i > 3) gi = 4*ngs(0) + i
      if (ngs(3) == 0) then
        ggamma(i, 0) = gsum(4*jj + 3 + 0, gi) * xj**(-r_exp(dr(1)) - 1) + gsum(4*jj + 3 + 1, gi) &
          + gsum(4*jj + 3 + 2, gi) * xj**(-r_exp(dr(1)) - 2) + gsum(4*jj + 3 + 3, gi) * xj
        ggamma(i, 1) = gsum(4*jj + 3 + 0, gi) * (-r_exp(dr(1))-1)*xj**(-r_exp(dr(1))-2) &
          + gsum(4*jj + 3 + 2, gi) * (-r_exp(dr(1))-2)*xj**(-r_exp(dr(1))-3) + gsum(4*jj + 3 + 3, gi)
        ggamma(i, 2) = gsum(4*jj + 3 + 0, gi) * (-r_exp(dr(1))-1)*(-r_exp(dr(1))-2)*xj**(-r_exp(dr(1))-3) &
          + gsum(4*jj + 3 + 2, gi) * (-r_exp(dr(1))-2)*(-r_exp(dr(1))-3)*xj**(-r_exp(dr(1))-4)
      else
        ggamma(i, 0) = gsum(4*jj + 3 + 0, gi) * cxj + gsum(4*jj + 3 + 1, gi) * cxj**2 + gsum(4*jj + 3 + 2, gi) &
          + gsum(4*jj + 3 + 3, gi) * cxj**3 + gsum(4*ngs(1) + 4, gi) + gsum(4*ngs(1) + 5, gi) * cxj
        ggamma(i, 1) = dcxj * (gsum(4*jj + 3 + 0, gi) + gsum(4*jj + 3 + 1, gi) * 2*cxj &
          + gsum(4*jj + 3 + 3, gi) * 3*cxj**2 + gsum(4*ngs(1) + 5, gi))
        ggamma(i, 2) = ddcxj * (gsum(4*jj + 3 + 0, gi) + gsum(4*jj + 3 + 1, gi) * 2*cxj &
          + gsum(4*jj + 3 + 3, gi) * 3*cxj**2 + gsum(4*ngs(1) + 5, gi)) &
          + dcxj * (gsum(4*jj + 3 + 1, gi) * 2 + gsum(4*jj + 3 + 3, gi) * 6*cxj)
      end if
    end do
    if (ngs(2) == 0) then
      rkhs2d_evaldd(0) = ggamma(0, 0) * (-r_exp(dr(0))-1)*(-r_exp(dr(0))-2)*xi**(-r_exp(dr(0))-3) &
        + ggamma(2, 0) * (-r_exp(dr(0))-2)*(-r_exp(dr(0))-3)*xi**(-r_exp(dr(0))-4)
      rkhs2d_evaldd(1) = ggamma(0, 1) * (-r_exp(dr(0))-1)*xi**(-r_exp(dr(0))-2) &
        + ggamma(2, 1) * (-r_exp(dr(0))-2)*xi**(-r_exp(dr(0))-3) + ggamma(3, 1)
      rkhs2d_evaldd(2) = ggamma(0, 2) * xi**(-r_exp(dr(0)) - 1) + ggamma(1, 2) &
        + ggamma(2, 2) * xi**(-r_exp(dr(0)) - 2) + ggamma(3, 2) * xi
    else
      rkhs2d_evaldd(0) = ddcxi * (ggamma(0, 0) + ggamma(1, 0) * 2*cxi &
        + ggamma(3, 0) * 3*cxi**2 + ggamma(5, 0)) + dcxi * (ggamma(1, 0) * 2 + ggamma(3, 0) * 6*cxi)
      rkhs2d_evaldd(1) = dcxi * (ggamma(0, 1) + ggamma(1, 1) * 2*cxi + ggamma(3, 1) * 3*cxi**2 + ggamma(5, 1))
      rkhs2d_evaldd(2) = ggamma(0, 2) * cxi + ggamma(1, 2) * cxi**2 + ggamma(2, 2) &
        + ggamma(3, 2) * cxi**3 + ggamma(4, 2) + ggamma(5, 2) * cxi
    end if
  end function rkhs2d_evaldd
#endif
#endif
#ifdef NATURAL
  subroutine natural_second(x, y, n, y2)
    implicit none
    integer i, n
    real(8) :: x(0:n-1), y(0:n-1), y2(0:n-1), p, sig
    real(8) :: u(0:n-1)
    
    y2(0) = 0d0; y2(n - 1) = 0d0; u(0) = 0
    do i = 1, n - 2
      sig = (x(i) - x(i - 1)) / (x(i + 1) - x(i - 1))
      p = sig * y2(i - 1) + 2
      y2(i) = (sig - 1) / p
      u(i) = (6 * ((y(i + 1) - y(i)) / (x(i + 1) - x(i)) - (y(i) - y(i - 1)) &
        / (x(i) - x(i - 1))) / (x(i + 1) - x(i - 1)) - sig * u(i - 1)) / p
    end do
    do i = n - 2, 0, -1
      y2(i) = y2(i) * y2(i + 1) + u(i)
    end do
  end subroutine natural_second
#endif
#ifdef CLAMP
  subroutine clamp_second(x, y, n, y2)
    implicit none
    integer i, n
    real(8) :: x(0:n-1), y(0:n-1), y2(0:n-1), p, sig, un
    real(8) :: u(0:n-1)

    y2(0) = -0.5d0
    u(0) = 3.0d0 * (y(1) - y(0)) / (x(1) - x(0)) ** 2
    un = -3.0d0 * (y(n - 1) - y(n - 2)) / (x(n - 1) - x(n - 2)) ** 2
    do i = 1, n - 2
      sig = (x(i) - x(i - 1)) / (x(i + 1) - x(i - 1))
      p = sig * y2(i - 1) + 2
      y2(i) = (sig - 1) / p
      u(i) = (6 * ((y(i + 1) - y(i)) / (x(i + 1) - x(i)) - (y(i) - y(i - 1)) &
        / (x(i) - x(i - 1))) / (x(i + 1) - x(i - 1)) - sig * u(i - 1)) / p
    end do
    y2(n - 1) = (un - 0.5d0 * u(n - 2)) / (0.5d0 * y2(n - 2) + 1.0d0)
    do i = n - 2, 0, -1
      y2(i) = y2(i) * y2(i + 1) + u(i)
    end do
  end subroutine clamp_second
#endif
#ifdef SPLINE
  real(8) function spline_eval(xa, ya, y2a, n, x)
    implicit none
    integer n, xi
    real(8) :: x, h, xa(0:n-1), ya(0:n-1), y2a(0:n-1), a, b
    xi = binary_search(x, xa, n)
    if (xi == -1) xi = 0
    if (xi == n - 1) xi = n - 2
    h = xa(xi+1) - xa(xi)
    a = (xa(xi+1) - x) / h
    b = (x - xa(xi)) / h
    spline_eval = a * ya(xi) + b * ya(xi+1) + ((a ** 3 - a) * y2a(xi) + &
      (b ** 3 - b) * y2a(xi+1)) * (h ** 2) / 6
  end function spline_eval
#ifdef DERIVATIVE1
  real(8) function spline_evald(xa, ya, y2a, n, x)
    implicit none
    integer n, xi
    real(8) :: x, h, xa(0:n-1), ya(0:n-1), y2a(0:n-1), a, b
    xi = binary_search(x, xa, n)
    if (xi == -1) xi = 0
    if (xi == n - 1) xi = n - 2
    h = xa(xi+1) - xa(xi)
    a = (xa(xi+1) - x) / h
    b = (x - xa(xi)) / h
    spline_evald = (-1d0/h)*(ya(xi) + (3*a*a - 1d0) * y2a(xi) * (h**2)/6) + &
      (1d0/h)*(ya(xi+1) + (3*b*b - 1d0) * y2a(xi+1) * (h**2)/6)
  end function spline_evald
#endif
#ifdef DERIVATIVE2
  real(8) function spline_evaldd(xa, ya, y2a, n, x)
    implicit none
    integer n, xi
    real(8) x, h, xa(0:n-1), ya(0:n-1), y2a(0:n-1), a, b
    xi = binary_search(x, xa, n)
    if (xi == -1) xi = 0
    if (xi == n - 1) xi = n - 2
    h = xa(xi+1) - xa(xi)
    a = (xa(xi+1) - x) / h
    b = (x - xa(xi)) / h
    spline_evaldd = a * y2a(xi) + b * y2a(xi+1)
  end function spline_evaldd
#endif
#endif
  integer function binary_search(x, r, n)
    implicit none
    integer a, b, m, n
    real(8) x, r(0:n-1)
    a = -1; b = n
    do while (b - a > 1)
      m = (a + b) / 2
      if (r(m) > x) then
        b = m
      else
        a = m
      end if
    end do
    binary_search = a
  end function binary_search
  
#if defined(READMAIN12) || defined(READMAIN21)
  real(8) function step2_eval(x)
    implicit none
    real(8) :: x(0:2)
    integer :: j, k, jp, kp
#if defined(MAIN3R) && defined(READMAIN21)
    real(8) :: ja(0:uk-1, 0:uk-1), jb(0:uk-1), jrsum(-1:4*uk+2*(1+MAIN3R))
#elif defined(MAIN2R) && defined(READMAIN12)
    real(8) :: ja(0:ujk-1, 0:ujk-1), jb(0:ujk-1), jrsum(-1:4*(ukm+MAIN3R)+2, -1:4*(ujm+MAIN2R)+2), jrxjk(0:max(ujm, ukm)-1, 0:1)
#endif
#if defined(READMAIN21)
    real(8) :: jv(0:uk - 1), jrx(0:uk - 1)
#elif defined(READMAIN12)
    real(8) :: jv(0:ujk - 1), jrx(0:ujk - 1)
#endif
    ! first part
#if defined(READMAIN21)
    do k = 0, uk - 1
      jv(k) = rkhs2d_eval(rsum(k)%x, nrsum(0, k), rxij(k)%x, (/ 1, 2 /), x(0), x(1))
    end do
#elif defined(READMAIN12)
    do j = 0, ujk - 1
#if defined(MAIN1R)
      jv(j) = MAIN1RE(rsum(j)%x, ui(j), vi(0, j), x(0))
#elif defined(MAIN1S)
      jv(j) = spline_eval(vi(0, j), vip(0, j), rx(0, j), ui(j), x(0))
#endif
    end do
#endif
    ! second part
#if defined(READMAIN21)
#if defined(MAIN3S)
    call MAIN3SS(vk, jv, uk, jrx)
    step2_eval = spline_eval(vk, jv, jrx, uk, x(2))
#elif defined(MAIN3R)
    do k = 0, uk - 1
      do kp = k, uk - 1
        ja(kp, k) = MAIN3RK(vk(k), vk(kp))
      end do
      ja(k, k) = ja(k, k) + r_alpha(3)
      jb(k) = jv(k)
    end do
    call cholesky(ja, jb, jrx, uk)
    call MAIN3RP(jrsum, uk, vk, jrx)
    step2_eval = MAIN3RE(jrsum, uk, vk, x(2))
#endif
#elif defined(READMAIN12)
    do j = 0, ujk - 1
      do jp = j, ujk - 1
        ja(jp, j) = MAIN2RK(vjk(0, j), vjk(0, jp)) * MAIN3RK(vjk(1, j), vjk(1, jp))
      end do
      ja(j, j) = ja(j, j) + r_alpha(2)
      jb(j) = jv(j)
    end do
    call cholesky(ja, jb, jrx, ujk)
    call rkhs2d_presum(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, vjk, jrx, ujk, (/ 2, 3 /))
    step2_eval = rkhs2d_eval(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, (/ 2, 3 /), x(1), x(2))
#endif
  end function step2_eval
#ifdef DERIVATIVE1
  function step2_evald(x)
    implicit none
    real(8) :: x(0:2)
    real(8) :: step2_evald(0:2)
    
    integer :: j, k, jp, kp, dj
#if defined(MAIN3R) && defined(READMAIN21)
    real(8) :: ja(0:uk-1, 0:uk-1), jb(0:uk-1), jrsum(-1:4*uk+2*(1+MAIN3R))
#elif defined(MAIN2R) && defined(READMAIN12)
    real(8) :: ja(0:ujk-1, 0:ujk-1), jb(0:ujk-1), jrsum(-1:4*(ukm+MAIN3R)+2, -1:4*(ujm+MAIN2R)+2), jrxjk(0:max(ujm, ukm)-1, 0:1)
#endif
#if defined(READMAIN21)
    real(8) :: jv(0:uk - 1, 0:2), jrx(0:uk - 1)
#elif defined(READMAIN12)
    real(8) :: jv(0:ujk - 1, 0:2), jrx(0:ujk - 1)
#endif
    ! first part
#if defined(READMAIN21)
    do k = 0, uk - 1
      jv(k, 0:1) = rkhs2d_evald(rsum(k)%x, nrsum(0, k), rxij(k)%x, (/ 1, 2 /), x(0), x(1))
      jv(k, 2) = rkhs2d_eval(rsum(k)%x, nrsum(0, k), rxij(k)%x, (/ 1, 2 /), x(0), x(1))
    end do
#elif defined(READMAIN12)
    do j = 0, ujk - 1
#if defined(MAIN1R)
      jv(j, 0) = MAIN1RED(rsum(j)%x, ui(j), vi(0, j), x(0))
      jv(j, 1) = MAIN1RE(rsum(j)%x, ui(j), vi(0, j), x(0))
#elif defined(MAIN1S)
      jv(j, 0) = spline_evald(vi(0, j), vip(0, j), rx(0, j), ui(j), x(0))
      jv(j, 1) = spline_eval(vi(0, j), vip(0, j), rx(0, j), ui(j), x(0))
#endif
    end do
#endif
    ! second part
#if defined(READMAIN21)
#if defined(MAIN3S)
    do dj = 0, 2
      call MAIN3SS(vk, jv(0:uk-1, dj), uk, jrx)
      if (dj /= 2) then
        step2_evald(dj) = spline_eval(vk, jv(0:uk-1, dj), jrx, uk, x(2))
      else
        step2_evald(dj) = spline_evald(vk, jv(0:uk-1, dj), jrx, uk, x(2))
      end if
    end do
#elif defined(MAIN3R)
    do dj = 0, 2
      do k = 0, uk - 1
        do kp = k, uk - 1
          ja(kp, k) = MAIN3RK(vk(k), vk(kp))
        end do
        ja(k, k) = ja(k, k) + r_alpha(3)
        jb(k) = jv(k, dj)
      end do
      call cholesky(ja, jb, jrx, uk)
      call MAIN3RP(jrsum, uk, vk, jrx)
      if (dj /= 2) then
        step2_evald(dj) = MAIN3RE(jrsum, uk, vk, x(2))
      else
        step2_evald(dj) = MAIN3RED(jrsum, uk, vk, x(2))
      end if
    end do
#endif
#elif defined(READMAIN12)
    do dj = 0, 1
      do j = 0, ujk - 1
        do jp = j, ujk - 1
          ja(jp, j) = MAIN2RK(vjk(0, j), vjk(0, jp)) * MAIN3RK(vjk(1, j), vjk(1, jp))
        end do
        ja(j, j) = ja(j, j) + r_alpha(2)
        jb(j) = jv(j, dj)
      end do
      call cholesky(ja, jb, jrx, ujk)
      call rkhs2d_presum(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, vjk, jrx, ujk, (/ 2, 3 /))
      if (dj == 0) then
        step2_evald(0) = rkhs2d_eval(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, (/ 2, 3 /), x(1), x(2))
      else
        step2_evald(1:2) = rkhs2d_evald(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, (/ 2, 3 /), x(1), x(2))
      end if
    end do
#endif
  end function step2_evald
#endif
#ifdef DERIVATIVE2
  function step2_evaldd(x)
    implicit none
    real(8) :: x(0:2)
    real(8) :: step2_evaldd(0:5) ! 0:ii 1:ij 2:ik 3:jj 4:jk 5:kk
    
    integer :: j, k, jp, kp, dj
#if defined(MAIN3R) && defined(READMAIN21)
    real(8) :: ja(0:uk-1, 0:uk-1), jb(0:uk-1), jrsum(-1:4*uk+2*(1+MAIN3R))
#elif defined(MAIN2R) && defined(READMAIN12)
    real(8) :: ja(0:ujk-1, 0:ujk-1), jb(0:ujk-1), jrsum(-1:4*(ukm+MAIN3R)+2, -1:4*(ujm+MAIN2R)+2), jrxjk(0:max(ujm, ukm)-1, 0:1)
#endif
#if defined(READMAIN21)
    real(8) :: jv(0:uk - 1, 0:5), jrx(0:uk - 1)
#elif defined(READMAIN12)
    real(8) :: jv(0:ujk - 1, 0:5), jrx(0:ujk - 1)
#endif
    ! first part
#if defined(READMAIN21)
    do k = 0, uk - 1
      jv(k, 0:2) = rkhs2d_evaldd(rsum(k)%x, nrsum(0, k), rxij(k)%x, (/ 1, 2 /), x(0), x(1))
      jv(k, 3:4) = rkhs2d_evald(rsum(k)%x, nrsum(0, k), rxij(k)%x, (/ 1, 2 /), x(0), x(1))
      jv(k, 5) = jv(k, 2)
      jv(k, 2) = jv(k, 3)
      jv(k, 3) = jv(k, 5)
      jv(k, 5) = rkhs2d_eval(rsum(k)%x, nrsum(0, k), rxij(k)%x, (/ 1, 2 /), x(0), x(1))
    end do
#elif defined(READMAIN12)
    do j = 0, ujk - 1
#if defined(MAIN1R)
      jv(j, 0) = MAIN1REDD(rsum(j)%x, ui(j), vi(0, j), x(0))
      jv(j, 1) = MAIN1RED(rsum(j)%x, ui(j), vi(0, j), x(0))
      jv(j, 2) = MAIN1RE(rsum(j)%x, ui(j), vi(0, j), x(0))
#elif defined(MAIN1S)
      jv(j, 0) = spline_evaldd(vi(0, j), vip(0, j), rx(0, j), ui(j), x(0))
      jv(j, 1) = spline_evald(vi(0, j), vip(0, j), rx(0, j), ui(j), x(0))
      jv(j, 2) = spline_eval(vi(0, j), vip(0, j), rx(0, j), ui(j), x(0))
#endif
    end do
#endif
    ! second part
#if defined(READMAIN21)
#if defined(MAIN3S)
    do dj = 0, 5
      call MAIN3SS(vk, jv(0:uk-1, dj), uk, jrx)
      if (dj == 5) then
        step2_evaldd(dj) = spline_evaldd(vk, jv(0:uk-1, dj), jrx, uk, x(2))
      else if (dj == 2 .or. dj == 4) then
        step2_evaldd(dj) = spline_evald(vk, jv(0:uk-1, dj), jrx, uk, x(2))
      else
        step2_evaldd(dj) = spline_eval(vk, jv(0:uk-1, dj), jrx, uk, x(2))
      end if
    end do
#elif defined(MAIN3R)
    do dj = 0, 5
      do k = 0, uk - 1
        do kp = k, uk - 1
          ja(kp, k) = MAIN3RK(vk(k), vk(kp))
        end do
        ja(k, k) = ja(k, k) + r_alpha(3)
        jb(k) = jv(k, dj)
      end do
      call cholesky(ja, jb, jrx, uk)
      call MAIN3RP(jrsum, uk, vk, jrx)
      if (dj == 5) then
        step2_evaldd(dj) = MAIN3REDD(jrsum, uk, vk, x(2))
      else if (dj == 2 .or. dj == 4) then
        step2_evaldd(dj) = MAIN3RED(jrsum, uk, vk, x(2))
      else
        step2_evaldd(dj) = MAIN3RE(jrsum, uk, vk, x(2))
      end if
    end do
#endif
#elif defined(READMAIN12)
    do dj = 0, 2
      do j = 0, ujk - 1
        do jp = j, ujk - 1
          ja(jp, j) = MAIN2RK(vjk(0, j), vjk(0, jp)) * MAIN3RK(vjk(1, j), vjk(1, jp))
        end do
        ja(j, j) = ja(j, j) + r_alpha(2)
        jb(j) = jv(j, dj)
      end do
      call cholesky(ja, jb, jrx, ujk)
      call rkhs2d_presum(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, vjk, jrx, ujk, (/ 2, 3 /))
      if (dj == 0) then
        step2_evaldd(0) = rkhs2d_eval(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, (/ 2, 3 /), x(1), x(2))
      else if (dj == 1) then
        step2_evaldd(1:2) = rkhs2d_evald(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, (/ 2, 3 /), x(1), x(2))
      else
        step2_evaldd(3:5) = rkhs2d_evaldd(jrsum, (/ ujm, ukm, MAIN2R, MAIN3R /), jrxjk, (/ 2, 3 /), x(1), x(2))
      end if
    end do
#endif
  end function step2_evaldd
#endif
#endif

#if defined(READMAIN111)
  real(8) function step3_eval(x)
    implicit none
    real(8) :: x(0:2)
    integer :: j, k, jp, kp
    real(8) :: kv(0:uk-1), krx(0:uk-1)
    real(8), allocatable :: jv(:), jrx(:)
#if defined(MAIN2R) || defined(MAIN3R)
    real(8), allocatable :: ka(:, :), kb(:), krsum(:)
#endif
    do k = 0, uk - 1
      allocate ( jv(0:uj(k)-1), jrx(0:uj(k)-1) )
      do j = 0, uj(k) - 1
#if defined(MAIN1R)
        jv(j) = MAIN1RE(rsum(j, k)%x, ui(j, k), vi(0, j, k), x(0))
#elif defined(MAIN1S)
        jv(j) = spline_eval(vi(0, j, k), vip(0, j, k), rx(0, j, k), ui(j, k), x(0))
#endif
      end do
#if defined(MAIN2R)
      allocate ( ka(0:uj(k)-1, 0:uj(k)-1), kb(0:uj(k)-1), krsum(-1:4*uj(k)+2*(1+MAIN2R)) )
      do j = 0, uj(k) - 1
        do jp = j, uj(k) - 1
          ka(jp, j) = MAIN2RK(vj(j, k), vj(jp, k))
        end do
        ka(j, j) = ka(j, j) + r_alpha(2)
        kb(j) = jv(j)
      end do
      call cholesky(ka, kb, jrx, uj(k))
      call MAIN2RP(krsum, uj(k), vj(0, k), jrx)
      kv(k) = MAIN2RE(krsum, uj(k), vj(0, k), x(1))
      deallocate ( ka, kb, krsum )
#elif defined(MAIN2S)
      call MAIN2SS(vj(0, k), jv, uj(k), jrx)
      kv(k) = spline_eval(vj(0, k), jv, jrx, uj(k), x(1))
#endif
      deallocate ( jv, jrx )
    end do
#if defined(MAIN3R)
    allocate ( ka(0:uk-1, 0:uk-1), kb(0:uk-1), krsum(-1:4*uk+2*(1+MAIN3R)) )
    do k = 0, uk - 1
      do kp = k, uk - 1
        ka(kp, k) = MAIN3RK(vk(k), vk(kp))
      end do
      ka(k, k) = ka(k, k) + r_alpha(3)
      kb(k) = kv(k)
    end do
    call cholesky(ka, kb, krx, uk)
    call MAIN3RP(krsum, uk, vk, krx)
    step3_eval = MAIN3RE(krsum, uk, vk, x(2))
    deallocate ( ka, kb, krsum )
#elif defined(MAIN3S)
    call MAIN3SS(vk, kv, uk, krx)
    step3_eval = spline_eval(vk, kv, krx, uk, x(2))
#endif
  end function step3_eval
#ifdef DERIVATIVE1
  function step3_evald(x)
    implicit none
    real(8) :: x(0:2)
    real(8) :: step3_evald(0:2)
    
    integer :: j, k, jp, kp, dj
    real(8) :: kv(0:uk-1, 0:2), krx(0:uk-1)
    real(8), allocatable :: jv(:, :), jrx(:)
#if defined(MAIN2R) || defined(MAIN3R)
    real(8), allocatable :: ka(:, :), kb(:), krsum(:)
#endif
    do k = 0, uk - 1
      allocate ( jv(0:uj(k)-1, 0:1), jrx(0:uj(k)-1) )
      do j = 0, uj(k) - 1
#if defined(MAIN1R)
        jv(j, 0) = MAIN1RED(rsum(j, k)%x, ui(j, k), vi(0, j, k), x(0))
        jv(j, 1) = MAIN1RE(rsum(j, k)%x, ui(j, k), vi(0, j, k), x(0))
#elif defined(MAIN1S)
        jv(j, 0) = spline_evald(vi(0, j, k), vip(0, j, k), rx(0, j, k), ui(j, k), x(0))
        jv(j, 1) = spline_eval(vi(0, j, k), vip(0, j, k), rx(0, j, k), ui(j, k), x(0))
#endif
      end do
      do dj = 0, 1
#if defined(MAIN2R)
        allocate ( ka(0:uj(k)-1, 0:uj(k)-1), kb(0:uj(k)-1), krsum(-1:4*uj(k)+2*(1+MAIN2R)) )
        do j = 0, uj(k) - 1
          do jp = j, uj(k) - 1
            ka(jp, j) = MAIN2RK(vj(j, k), vj(jp, k))
          end do
          ka(j, j) = ka(j, j) + r_alpha(2)
          kb(j) = jv(j, dj)
        end do
        call cholesky(ka, kb, jrx, uj(k))
        call MAIN2RP(krsum, uj(k), vj(0, k), jrx)
        if (dj == 0) then
          kv(k, 0) = MAIN2RE(krsum, uj(k), vj(0, k), x(1))
        else if (dj == 1) then
          kv(k, 1) = MAIN2RED(krsum, uj(k), vj(0, k), x(1))
          kv(k, 2) = MAIN2RE(krsum, uj(k), vj(0, k), x(1))
        end if
        deallocate ( ka, kb, krsum )
#elif defined(MAIN2S)
        call MAIN2SS(vj(0, k), jv(0, dj), uj(k), jrx)
        if (dj == 0) then
          kv(k, 0) = spline_eval(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
        else if (dj == 1) then
          kv(k, 1) = spline_evald(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
          kv(k, 2) = spline_eval(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
        end if
#endif
      end do
      deallocate ( jv, jrx )
    end do
    do dj = 0, 2
#if defined(MAIN3R)
      allocate ( ka(0:uk-1, 0:uk-1), kb(0:uk-1), krsum(-1:4*uk+2*(1+MAIN3R)) )
      do k = 0, uk - 1
        do kp = k, uk - 1
          ka(kp, k) = MAIN3RK(vk(k), vk(kp))
        end do
        ka(k, k) = ka(k, k) + r_alpha(3)
        kb(k) = kv(k, dj)
      end do
      call cholesky(ka, kb, krx, uk)
      call MAIN3RP(krsum, uk, vk, krx)
      if (dj == 0 .or. dj == 1) then
        step3_evald(dj) = MAIN3RE(krsum, uk, vk, x(2))
      else if (dj == 2) then
        step3_evald(dj) = MAIN3RED(krsum, uk, vk, x(2))
      end if
      deallocate ( ka, kb, krsum )
#elif defined(MAIN3S)
      call MAIN3SS(vk, kv(0, dj), uk, krx)
      if (dj == 0 .or. dj == 1) then
        step3_evald(dj) = spline_eval(vk, kv(0, dj), krx, uk, x(2))
      else if (dj == 2) then
        step3_evald(dj) = spline_evald(vk, kv(0, dj), krx, uk, x(2))
      end if
#endif
    end do
  end function step3_evald
#endif
#ifdef DERIVATIVE2
  function step3_evaldd(x)
    implicit none
    real(8) :: x(0:2)
    real(8) :: step3_evaldd(0:5) ! 0:ii 1:ij 2:ik 3:jj 4:jk 5:kk
    
    integer :: j, k, jp, kp, dj
    real(8) :: kv(0:uk-1, 0:5), krx(0:uk-1)
    real(8), allocatable :: jv(:, :), jrx(:)
#if defined(MAIN2R) || defined(MAIN3R)
    real(8), allocatable :: ka(:, :), kb(:), krsum(:)
#endif
    do k = 0, uk - 1
      allocate ( jv(0:uj(k)-1, 0:2), jrx(0:uj(k)-1) )
      do j = 0, uj(k) - 1
#if defined(MAIN1R)
        jv(j, 0) = MAIN1REDD(rsum(j, k)%x, ui(j, k), vi(0, j, k), x(0))
        jv(j, 1) = MAIN1RED(rsum(j, k)%x, ui(j, k), vi(0, j, k), x(0))
        jv(j, 2) = MAIN1RE(rsum(j, k)%x, ui(j, k), vi(0, j, k), x(0))
#elif defined(MAIN1S)
        jv(j, 0) = spline_evaldd(vi(0, j, k), vip(0, j, k), rx(0, j, k), ui(j, k), x(0))
        jv(j, 1) = spline_evald(vi(0, j, k), vip(0, j, k), rx(0, j, k), ui(j, k), x(0))
        jv(j, 2) = spline_eval(vi(0, j, k), vip(0, j, k), rx(0, j, k), ui(j, k), x(0))
#endif
      end do
      do dj = 0, 2
#if defined(MAIN2R)
        allocate ( ka(0:uj(k)-1, 0:uj(k)-1), kb(0:uj(k)-1), krsum(-1:4*uj(k)+2*(1+MAIN2R)) )
        do j = 0, uj(k) - 1
          do jp = j, uj(k) - 1
            ka(jp, j) = MAIN2RK(vj(j, k), vj(jp, k))
          end do
          ka(j, j) = ka(j, j) + r_alpha(2)
          kb(j) = jv(j, dj)
        end do
        call cholesky(ka, kb, jrx, uj(k))
        call MAIN2RP(krsum, uj(k), vj(0, k), jrx)
        if (dj == 0) then
          kv(k, 0) = MAIN2RE(krsum, uj(k), vj(0, k), x(1))
        else if (dj == 1) then
          kv(k, 1) = MAIN2RED(krsum, uj(k), vj(0, k), x(1))
          kv(k, 2) = MAIN2RE(krsum, uj(k), vj(0, k), x(1))
        else if (dj == 2) then
          kv(k, 3) = MAIN2REDD(krsum, uj(k), vj(0, k), x(1))
          kv(k, 4) = MAIN2RED(krsum, uj(k), vj(0, k), x(1))
          kv(k, 5) = MAIN2RE(krsum, uj(k), vj(0, k), x(1))
        end if
        deallocate ( ka, kb, krsum )
#elif defined(MAIN2S)
        call MAIN2SS(vj(0, k), jv(0, dj), uj(k), jrx)
        if (dj == 0) then
          kv(k, 0) = spline_eval(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
        else if (dj == 1) then
          kv(k, 1) = spline_evald(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
          kv(k, 2) = spline_eval(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
        else if (dj == 2) then
          kv(k, 3) = spline_evaldd(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
          kv(k, 4) = spline_evald(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
          kv(k, 5) = spline_eval(vj(0, k), jv(0, dj), jrx, uj(k), x(1))
        end if
#endif
      end do
      deallocate ( jv, jrx )
    end do
    do dj = 0, 5
#if defined(MAIN3R)
      allocate ( ka(0:uk-1, 0:uk-1), kb(0:uk-1), krsum(-1:4*uk+2*(1+MAIN3R)) )
      do k = 0, uk - 1
        do kp = k, uk - 1
          ka(kp, k) = MAIN3RK(vk(k), vk(kp))
        end do
        ka(k, k) = ka(k, k) + r_alpha(3)
        kb(k) = kv(k, dj)
      end do
      call cholesky(ka, kb, krx, uk)
      call MAIN3RP(krsum, uk, vk, krx)
      if (dj == 5) then
        step3_evaldd(dj) = MAIN3REDD(krsum, uk, vk, x(2))
      else if (dj == 2 .or.  dj == 4) then
        step3_evaldd(dj) = MAIN3RED(krsum, uk, vk, x(2))
      else
        step3_evaldd(dj) = MAIN3RE(krsum, uk, vk, x(2))
      end if
      deallocate ( ka, kb, krsum )
#elif defined(MAIN3S)
      call MAIN3SS(vk, kv(0, dj), uk, krx)
      if (dj == 5) then
        step3_evaldd(dj) = spline_evaldd(vk, kv(0, dj), krx, uk, x(2))
      else if (dj == 2 .or.  dj == 4) then
        step3_evaldd(dj) = spline_evald(vk, kv(0, dj), krx, uk, x(2))
      else
        step3_evaldd(dj) = spline_eval(vk, kv(0, dj), krx, uk, x(2))
      end if
#endif
    end do
  end function step3_evaldd
#endif
#endif
  real(8) function main_eval(x)
    implicit none
    real(8) :: x(0:2)
#ifdef MANYBODY
    real(8) :: r(0:2)
    integer :: d
#endif
    main_eval = 0d0
#ifdef MANYBODY
    main_eval =  main_eval + e1
    r = fh(x)
    do d = 0, 2
#ifdef MAIN0R
      main_eval = main_eval + MAIN0RE(mbsum(d)%x, mbn(d), mbr(0, d), r(d))
#endif
#ifdef MAIN0S
      main_eval = main_eval + spline_eval(mbr(0, d), mbv(0, d), mbx(0, d), mbn(d), r(d))
#endif
    end do
#endif
#if defined(READMAIN111)
    main_eval = main_eval + step3_eval(x)
#elif defined(READMAIN12) || defined(READMAIN21)
    main_eval = main_eval + step2_eval(x)
#elif defined(READMAIN3)
    main_eval = main_eval + rkhs3d_eval(rsum, nrsum, rxijk, x(0), x(1), x(2))
#endif
  end function main_eval
#ifdef DERIVATIVE1
  function main_evald(x)
    implicit none
    real(8) :: x(0:2), main_evald(0:2)
#ifdef MANYBODY
    real(8) :: r(0:2)
    real(8) :: rd(0:2, 0:2)
    integer :: d
#endif
    main_evald = 0d0
#ifdef MANYBODY
    r = fh(x); rd = fhd(x)
    do d = 0, 2
#ifdef MAIN0R
      main_evald = main_evald + MAIN0RED(mbsum(d)%x, mbn(d), mbr(0, d), r(d)) * rd(0:2, d)
#endif
#ifdef MAIN0S
      main_evald = main_evald + spline_evald(mbr(0, d), mbv(0, d), mbx(0, d), mbn(d), r(d)) * rd(0:2, d)
#endif
    end do
#endif
#if defined(READMAIN111)
    main_evald = main_evald + step3_evald(x)
#elif defined(READMAIN12) || defined(READMAIN21)
    main_evald = main_evald + step2_evald(x)
#elif defined(READMAIN3)
    main_evald = main_evald + rkhs3d_evald(rsum, nrsum, rxijk, x(0), x(1), x(2))
#endif
  end function main_evald
#endif
#ifdef DERIVATIVE2
  function main_evaldd(x)
    implicit none
    real(8) :: x(0:2), main_evaldd(0:5)
    integer :: dr
#ifdef MANYBODY
    real(8) :: r(0:2), rdv, rddv
    real(8) :: rd(0:2, 0:2), rdd(0:5, 0:2)
    integer, parameter :: xd(0:1, 0:5) = reshape((/ 0, 0, 0, 1, 0, 2, 1, 1, 1, 2, 2, 2 /), (/ 2, 6 /))
    integer :: d
#endif
    main_evaldd = 0d0
#ifdef MANYBODY
    r = fh(x); rd = fhd(x); rdd = fhdd(x)
    do d = 0, 2
#ifdef MAIN0R
      rdv = MAIN0RED(mbsum(d)%x, mbn(d), mbr(0, d), r(d))
      rddv = MAIN0REDD(mbsum(d)%x, mbn(d), mbr(0, d), r(d))
#endif
#ifdef MAIN0S
      rdv = spline_evald(mbr(0, d), mbv(0, d), mbx(0, d), mbn(d), r(d))
      rddv = spline_evaldd(mbr(0, d), mbv(0, d), mbx(0, d), mbn(d), r(d))
#endif
      do dr = 0, 5
        main_evaldd(dr) = main_evaldd(dr) + rddv * rd(xd(0, dr), d) * rd(xd(1, dr), d) + rdv * rdd(dr, d)
      end do
    end do
#endif
#if defined(READMAIN111)
    main_evaldd = main_evaldd + step3_evaldd(x)
#elif defined(READMAIN12) || defined(READMAIN21)
    main_evaldd = main_evaldd + step2_evaldd(x)
#elif defined(READMAIN3)
    main_evaldd = main_evaldd + rkhs3d_evaldd(rsum, nrsum, rxijk, x(0), x(1), x(2))
#endif
  end function main_evaldd
#endif
end subroutine intpol_main

integer function binary_search(x, r, n)
  implicit none
  integer a, b, m, n
  real(8) x, r(0:n-1)
  a = -1; b = n
  do while (b - a > 1)
    m = (a + b) / 2
    if (r(m) > x) then
      b = m
    else
      a = m
    end if
  end do
  binary_search = a
end function binary_search

#if defined(FREQ) || defined(OPTM)
subroutine int_eval(i, e)
  implicit none
  interface
    subroutine intpol_main(do_type, x, y, z, v, dv)
      implicit none
      integer :: do_type
      real(8), optional :: x, y, z, v, dv(0:do_type - 1)
    end subroutine intpol_main
  end interface
  real(8) :: i(0:2), e
  real(8) :: x(0:2)
  
  call int_trans(x, i, .false.)
  call intpol_main(1, x(0), x(1), x(2), e)
end subroutine int_eval

subroutine int_evald(i, e)
  implicit none
  interface
    subroutine intpol_main(do_type, x, y, z, v, dv)
      implicit none
      integer :: do_type
      real(8), optional :: x, y, z, v, dv(0:do_type - 1)
    end subroutine intpol_main
  end interface
  real(8) :: i(0:2), e(0:2)
  real(8) :: x(0:2), di(0:2, 0:2), ex(0:2)
  integer :: j, k
  
  call int_trans(x, i, .false.)
  call int_d(i, di)
  call intpol_main(3, x(0), x(1), x(2), 0d0, ex)
  do j = 0, 2
    e(j) = 0d0
    do k = 0, 2
      e(j) = e(j) + ex(k) * di(j, k)
    end do
  end do
end subroutine int_evald

subroutine int_evaldd(i, e)
  implicit none
  interface
    subroutine intpol_main(do_type, x, y, z, v, dv)
      implicit none
      integer :: do_type
      real(8), optional :: x, y, z, v, dv(0:do_type - 1)
    end subroutine intpol_main
  end interface
  real(8) :: i(0:2), e(0:5)
  real(8) :: x(0:2), di(0:2, 0:2), ddi(0:5, 0:2), ex(0:2), eex(0:5)
  integer :: j, k, l0, l1, m0, m1
  
  call int_trans(x, i, .false.)
  call int_d(i, di)
  call int_dd(i, ddi)
  call intpol_main(3, x(0), x(1), x(2), 0d0, ex)
  call intpol_main(6, x(0), x(1), x(2), 0d0, eex)
  do j = 0, 5
    e(j) = 0d0
    do k = 0, 2
      e(j) = e(j) + ex(k) * ddi(j, k)
    end do
    l0 = j/3+j/5; l1 = mod(j,3)+j/3-j/5
    do k = 0, 5
      m0 = k/3+k/5; m1 = mod(k,3)+k/3-k/5
      e(j) = e(j) + eex(k) * di(l0, m0) * di(l1, m1)
      if (m0 /= m1) e(j) = e(j) + eex(k) * di(l0, m1) * di(l1, m0)
    end do
  end do
end subroutine int_evaldd
#endif

#ifdef FREQ
subroutine car_eval(i, e)
  implicit none
  real(8) :: i(0:2), e
  real(8) :: x(0:2)
  
  call car_trans(x, i, .false.)
  call int_eval(x, e)
end subroutine car_eval

subroutine car_evald(i, e)
  implicit none
  real(8) :: i(0:2), e(0:8)
  real(8) :: x(0:2), di(0:8, 0:2), ex(0:2)
  integer :: j, k
  
  call car_trans(x, i, .false.)
  call car_d(i, di)
  call int_evald(x, ex)
  do j = 0, 8
    e(j) = 0d0
    do k = 0, 2
      e(j) = e(j) + ex(k) * di(j, k)
    end do
  end do
end subroutine car_evald

subroutine car_evaldd(i, e)
  implicit none
  real(8) :: i(0:2), e(0:44)
  real(8) :: x(0:2), di(0:8, 0:2), ddi(0:44, 0:2), ex(0:2), eex(0:5)
  integer :: j, k, l0, l1, m0, m1
  
  call car_trans(x, i, .false.)
  call car_d(i, di)
  call car_dd(i, ddi)
  call int_evald(x, ex)
  call int_evaldd(x, eex)
  do l0 = 0, 8
    do l1 = l0, 8
      j = (2*9+1-l0)*l0/2+l1-l0
      e(j) = 0d0
      do k = 0, 2
        e(j) = e(j) + ex(k) * ddi(j, k)
      end do
      do m0 = 0, 2
        do m1 = m0, 2
          k = (2*3+1-m0)*m0/2+m1-m0
          e(j) = e(j) + eex(k) * di(l0, m0) * di(l1, m1)
          if (m0 /= m1) e(j) = e(j) + eex(k) * di(l0, m1) * di(l1, m0)
        end do
      end do
    end do
  end do
end subroutine car_evaldd
#endif
#ifdef POINT
subroutine run_init()
  implicit none
  interface
    subroutine intpol_main(do_type, x, y, z, v, dv)
      implicit none
      integer :: do_type
      real(8), optional :: x, y, z, v, dv(0:do_type - 1)
    end subroutine intpol_main
  end interface
  write (*, '(/,''1'',a)') '*** PROGRAM INTPOL 1.1 ***'
  write (*, '(/,''1'',a)') 'RUN * INITIATION'
  call intpol_main(0)
end subroutine

subroutine run_end()
  implicit none
  interface
    subroutine intpol_main(do_type, x, y, z, v, dv)
      implicit none
      integer :: do_type
      real(8), optional :: x, y, z, v, dv(0:do_type - 1)
    end subroutine intpol_main
  end interface
  write (*, '(/,''1'',a,/)') 'RUN * END'
end subroutine
  
subroutine run_point(x)
  implicit none
  interface
    subroutine intpol_main(do_type, x, y, z, v, dv)
      implicit none
      integer :: do_type
      real(8), optional :: x, y, z, v, dv(0:do_type - 1)
    end subroutine intpol_main
  end interface
  real(8) :: x(0:2), dv(0:2), v
  integer :: i
  
  call intpol_main(1, x(0), x(1), x(2), v)
  call intpol_main(3, x(0), x(1), x(2), 0d0, dv)
  write (*, '(/,''1'',a,3x,60a1)') 'RUN * SINGLE POINT CLACULATION', ('=', i = 1, 60 )
  write (*, '(/,x,a)') 'Geometry:'
  write (*, '(x,a25,a25,a25)') COORDA, COORDB, COORDC
  write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
  write (*, '(x,3(f25.10))') x(0), x(1), x(2)
  write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
  write (*, '(/,x,a,f25.10)') 'Energy:', v
  write (*, '(/,x,a)') 'Forces:'
  write (*, '(x,3(f25.10))') dv(0), dv(1), dv(2)
end subroutine run_point
#endif
#ifdef OPTM
subroutine qsd_eval(x, e)
  real(8) x, e
  call int_eval(x, e)
end subroutine
subroutine qsd_evald(x, e)
  real(8) x, e
  call int_evald(x, e)
end subroutine
subroutine qsd_evaldd(x, e)
  real(8) x, e
  call int_evaldd(x, e)
end subroutine
subroutine run_opt(x, iter, root, step_len, step_conv, f_conv, ierr)
  implicit none
  integer :: iter, root, ierr, i
  real(8) :: x(0:2), step_len, step_conv, f_conv, ix(0:2)
  
  write (*, '(/,''1'',a,3x,60a1)') 'RUN * GEOMETRY OPTIMIZATION', ('=', i = 1, 60 )
  write (*, '(/,x,a)') 'Initial coordinates:'
  write (*, '(x,a25,a25,a25)') COORDA, COORDB, COORDC
  write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
  write (*, '(x,3(f25.10))') x(0), x(1), x(2)
  write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
  call int_trans(x, ix, .true.)
  write (*, '(/,x,a)') 'Translated internal coordinates:'
  write (*, '(x,a25,a25,a25)') COORDIA, COORDIB, COORDIC
  write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
  write (*, '(x,3(f25.10))') ix(0), ix(1), ix(2)
  write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
  write (*, '(/,x,a)') 'Optimization parameters:'
  if (root == 1) then
    write (*, '(4x,a20,a20)') 'type:', 'minimum'
  else
    write (*, '(4x,a20,a20)') 'type:', 'transition state'
  end if
  write (*, '(4x,a20,i20)') 'max iteration:', iter
  write (*, '(4x,a20,f20.10)') 'max step length:', step_len
  write (*, '(4x,a20,f20.10)') 'step convergence:', step_conv
  write (*, '(4x,a20,f20.10)') 'force convergence:', f_conv
  write (*, '(x)')
  call qsd_opt(root, iter, step_len, step_conv, f_conv, ix, 3, ierr)
  if (ierr /= 0) then
    write (*, '(/,x,a)') 'Geometry optimization failed.'
  else
    write (*, '(/,x,a)') 'Final internal coordinates:'
    write (*, '(x,a25,a25,a25)') COORDIA, COORDIB, COORDIC
    write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
    write (*, '(x,3(f25.10))') ix(0), ix(1), ix(2)
    write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
    call int_trans(x, ix, .false.)
    write (*, '(/,x,a)') 'Final coordinates:'
    write (*, '(x,a25,a25,a25)') COORDA, COORDB, COORDC
    write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
    write (*, '(x,3(f25.10))') x(0), x(1), x(2)
    write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
    write (*, '(/,x,a)') 'Geometry optimization finished.'
  end if
end subroutine run_opt

! qsd find minimum/transition state
! stm max step length, stt min step length, qt = 1 min, qt = 2 trans
! x start point, nt max step number, n dimension, gcv gradient convergence
subroutine qsd_opt(qt, nt, stmx, stt, gcv, x, n, ierr)
  implicit none
  integer, parameter :: itam = 5
  integer :: qt, nt, t, i, j, k, ig, n, ita, ierr
  real(8) :: stmx, stm, stt, stl, x(0:n-1)
  real(8) :: h(0:n-1, 0:n-1), g(0:n-1), hg(0:n*(n+1)/2-1), p0(0:n-1), q0(0:n-1), dg(0:n-1), dd
  real(8) :: e(0:n-1), m(0:n-1), p1(0:n-1), q1(0:n-1), e0, e1, de, gm, gl, gr, cvl, cvr, cvm, gcv
  p0 = x; q0 = x; stm = stmx * 2d0
  write (*, '(1x,a6,3a14,3a12,3a12)') 'ITER.', 'ENERGY(OLD)', 'ENERGY(NEW)', 'DE', &
    'GRADMAX', 'GRADNORM', 'GRADRMS', 'STEPMAX', 'STEPLEN', 'STEPRMS'
  ita = 0
  ierr = 0
  do t = 1, nt
    call qsd_evald(q0, g)
    call qsd_lmr(g, n, gl, gm, gr)
    call qsd_evaldd(q0, hg)
    k = 0
    do i = 0, n - 1
      do j = i, n - 1
        h(j, i) = hg(k)
        k = k + 1
      end do
    end do
    call diag(h, e, n)
    call qsd_m(e, h, g, q0, m, n)
    ig = 0
    k = 0
    do i = 0, n - 1
      if (e(i) < 0d0) ig = ig + 1
      if (e(i) < e(k)) k = i
    end do
    if (qt == 2) e(k) = -e(k)
    call qsd_dist(m, q0, n, stl)
    if (stl < stt .and. t > 1 .and. gl < gcv) then
      if (ig == 0) then
        write (*, *) 'Minimum reached.'
        ierr = qt - 1
      else if (ig == 1) then
        write (*, *) 'Transition state reached.'
        ierr = 2 - qt
      else
        write (*, *) 'Critical point reached. Number of negative Hessian eigenvalues: ', ig, '.'
        ierr = 2
      end if
      exit
    end if
    if (t == 1) then
      if (ig /= 0 .and. stl < stt) then
        ! start point is critical point
        do i = 0, n - 1
          p1(i) = m(i) + h(i, k) * stm / 2d0
          q1(i) = m(i) + h(i, k) * stm
        end do
      else
        ! general first step
        call qsd_step(e, h, m, p0, p1, q1, n, stm / 4d0, stm / 4d0)
      end if
    else if (t == 2) then
      call qsd_step(e, h, m, p0, p1, q1, n, stm / 4d0, stm / 2d0)
    else
      call qsd_step(e, h, m, p0, p1, q1, n, stm / 2d0, stm / 2d0)
    end if
    if (t == 1) call qsd_eval(q0, e0)
    call qsd_eval(q1, e1)
    de = e1 - e0
    g = q1 - q0
    call qsd_lmr(g, n, cvl, cvm, cvr)
    if (t /= 1 .and. qt == 2) then
      dd = 0d0
      do i = 0, n - 1
        dd = dd + dg(i)*g(i)
      end do
      if (dd < 0d0) ita = ita + 1
      dg = g
      if (ita == itam) then
        write (*, *) 'A trap region reached. Choose another initial point.'
        ierr = 3
        exit
      end if
    end if
    write (*, '(1x,i3,3f15.8,3f12.6,3f12.8)') t, e0, e1, de, gm, gl, gr, cvm, cvl, cvr
    e0 = e1
    ! write (*, *) q1
    p0 = p1
    q0 = q1
  end do
  x = m
  if (t == nt + 1) then
    write (*, '(x,a,i,a)') 'Failed to converge in ', nt, ' iterations.'
    ierr = 4
  end if
  write (*, *) 'Optimized variables'
  do i = 0, n - 1
    write (*, '(1x,f15.8)', advance = 'no'), x(i)
  end do
  write (*, '(1x)')
end subroutine

! qsd calculate critical point m
! e eigenvalue, v eigenvector, g gradient, q0 expansion center, m critical point
! m = q0 - H^(-1)g = q0 - VE^(-1)V^Tg, v = V^T
subroutine qsd_m(e, v, g, q0, m, n)
  implicit none
  integer :: n, i
  real(8) :: e(0:n-1), v(0:n-1,0:n-1), g(0:n-1)
  real(8) :: q0(0:n-1), m(0:n-1), t(0:n-1)
  real(8), parameter :: eps = 1d-10
  
  call mxv(v, g, n, t, .false., 0d0)
  do i = 0, n - 1
    if (abs(e(i)) > eps) then
      t(i) = -t(i)/e(i)
    else
      t(i) = 0d0
    end if
  end do
  m = q0
  call mxv(v, t, n, m, .true., 1d0)
  end subroutine qsd_m

! calculate length, max component, rms
subroutine qsd_lmr(x, n, l, m, r)
  implicit none
  integer :: n, i
  real(8) :: x(0:n-1), l, m, r
  
  m = x(0); l = 0d0
  do i = 0, n - 1
    if (x(i) > m) m = x(i)
    l = l + x(i)**2
  end do
  l = sqrt(l); r = 0d0
  do i = 0, n - 1
    r = r + (x(i) - l)**2
  end do
  r = sqrt(r/n)
end subroutine

! qsd binary search, solve p1 q1 from m (critical point) and p0
! p0--q0--p1--q1--m
! h1 is distance of p0--p1, h2 is distance of p0--q1
! assume v stored as rows, normalized
subroutine qsd_step(e, v, m, p0, p1, q1, n, h1, h2)
  implicit none
  integer :: n, i, j, t, k
  real(8) :: e(0:n-1), v(0:n-1,0:n-1), h1, h2
  real(8) :: m(0:n-1), p0(0:n-1), p1(0:n-1), q1(0:n-1)
  real(8), parameter :: eps = 1d-7
  integer, parameter :: mstep = 300
  real(8) :: pt(0:n-1), pp(0:n-1), pm(0:n-1), ti, tf, tm, h, dm
  
  ! translate start point p0 to the eigenframe of H, i.e. project p0 on H eigenvectors
  ! let m (lowest point) to be the origin
  do i = 0, n - 1
    pt(i) = p0(i) - m(i)
  end do
  call mxv(v, pt, n, pp, .false., 0d0) ! pp = v*pt
  ! select smallest eigenvalue of H
  k = 0
  do i = 1, n - 1
    if (e(i) < e(k)) k = i
  end do
  ! first loop: pp is start point, pm first result, pp first result
  ! second loop: pp is start point, pm second result
  do t = 0, 1
    h = h1
    if (t == 1) h = h2
    ti = 1d0
    if (e(k) > 0d0) then
      tf = 0d0
    else
      ! solve h = (tf**e(k)-1) |v(k)*pp|
      tf = exp(log(1d0 + h / abs(pp(k))) / e(k))
    end if
    ! if m is already inside the step region
    do i = 0, n - 1
      pm(i) = pp(i) * tf**e(i)
    end do
    call qsd_dist(pm, pp, n, dm)
    if (dm < h) then
      if (t == 0) pp = pm
      exit
    end if
    ! pt with u = 1, pm is mid point
    do j = 0, mstep - 1
      tm = (ti + tf) / 2d0
      do i = 0, n - 1
        pm(i) = pp(i) * tm**e(i)
      end do
      call qsd_dist(pm, pp, n, dm)
      if (abs(dm-h) < eps) exit
      if (dm > h) then
        tf = tm
      else
        ti  = tm
      end if
    end do
    if (j == mstep) write (*, *) 'not converged: abs(d-h) = ', abs(dm-h)
    if (t == 0) pp = pm
  end do
  ! translate to original frame
  p1 = m; q1 = m
  call mxv(v, pp, n, p1, .true., 1d0) ! p1 = v^T*pp + p1
  call mxv(v, pm, n, q1, .true., 1d0) ! p1 = v^T*pp + p1
end subroutine

! qsd calculate distance d between pa, pb
subroutine qsd_dist(pa, pb, n, d)
  implicit none
  integer :: n, i
  real(8) :: pa(0:n-1), pb(0:n-1), d
  d = 0d0
  do i = 0, n - 1
    d = d + (pa(i)-pb(i))**2
  end do
  d = sqrt(d)
end subroutine qsd_dist

! real symmetric matrix diagonalization, without eigenvectors
! use upper part (according to fortran row/column) i.e. a(j,i),j>=i
subroutine diag2(a, d, n)
  implicit none
  integer :: n, ierr
  real(8) :: a(n, n), d(n), work(34*n)
  
  call dsyev ('N', 'L', n, a, n, d, work, 34*n, ierr)
end subroutine diag2

! matrix - vector multiplication
! according to fortran row/column, t - transpose
! t == true: y = a^T*v | t == false: y = a*v
! beta is the coefficient of y
subroutine mxv(a, v, n, y, t, beta)
  implicit none
  integer :: n
  logical :: t
  real(8) :: a(n, n), v(n), y(n), beta
  
  if (t) then
    call dgemv('N', n, n, 1d0, a, n, v, 1, beta, y, 1)
  else
    call dgemv('T', n, n, 1d0, a, n, v, 1, beta, y, 1)
  end if
end subroutine mxv
#endif
#if defined(OPTM) || defined(FREQ)
! real symmetric matrix diagonalization, including eigenvectors return in a
! use upper part (according to fortran row/column) i.e. a(j,i),j>=i
! eigenvectors stored as row vectors, i.e. u1(j)=a(j,0)
subroutine diag(a, d, n)
  implicit none
  integer :: n, ierr
  real(8) :: a(n, n), d(n), work(34*n)
  
  call dsyev ('V', 'L', n, a, n, d, work, 34*n, ierr)
end subroutine diag
#endif
#ifdef FREQ
subroutine run_freq(x)
  implicit none
  integer :: i, j, k
  real(8) :: x(0:2), ix(0:2), cx(0:8), dx(0:44), h(0:8, 0:8), d(0:8), dvx(0:8), s, ms
  real(8) :: gd(0:8, 0:3), grad(0:8), zpe, dxa(0:8)
  integer :: gdn(0:3), gdi(0:8, 0:3)
  real(8), parameter :: ama = AMA, amb = AMB, amc = AMC
  real(8), parameter :: amutokg = 1.660538921d-27, autokg = 9.1093826d-31, &
    pi = 3.14159265358979d0, c = 137.036d0, auvtocm = 1d0/5.291772108d-9, &
    autokj = 2625.49962d0, autokcal = 627.509469d0, autocm = 219474.6313705d0
  real(8), parameter :: zero_cut = 1d0
  integer, external :: binary_search
  logical :: ima
  
  write (*, '(/,''1'',a,3x,60a1)') 'RUN * FREQUENCIES CALCULATION', ('=', i = 1, 60 )
  write (*, '(/,x,a)') 'Initial coordinates:'
  write (*, '(x,a25,a25,a25)') COORDA, COORDB, COORDC
  write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
  write (*, '(x,3(f25.10))') x(0), x(1), x(2)
  write (*, '(x,3(x,24a1))') ('-', i = 1, 72 )
  call int_trans(x, ix, .true.)
  call car_trans(ix, cx, .true.)
  write (*, '(/,x,a)') 'Translated mass-weighted Cartesian coordinates:'
  write (*, '(x,a10,a25,a25,a25)') 'atom', 'X', 'Y', 'Z'
  write (*, '(x,10a1,3(x,24a1))') ('-', i = 1, 82 )
  write (*, '(x,a10,3(f25.10))') ANA, cx(0), cx(1), cx(2)
  write (*, '(x,a10,3(f25.10))') ANB, cx(3), cx(4), cx(5)
  write (*, '(x,a10,3(f25.10))') ANC, cx(6), cx(7), cx(8)
  write (*, '(x,10a1,3(x,24a1))') ('-', i = 1, 82 )
  call car_evaldd(cx, dx)
  i = 0
  do j = 0, 8
    do k = j, 8
      h(k, j) = dx(i)
      h(j, k) = dx(i)
      i = i + 1
    end do
  end do
  write (*, '(/,x,a)') 'Hessian:'
  write (*, '(x,10a10)') '', ANAX, ANAY, ANAZ, ANBX, ANBY, ANBZ, ANCX, ANCY, ANCZ
  write (*, '(x,10a1,9(x,9a1))') ('-', i = 1, 91 )
  write (*, '(x,a10,9(f10.5))') ANAX, (h(i, 0), i = 0, 8)
  write (*, '(x,a10,9(f10.5))') ANAY, (h(i, 1), i = 0, 8)
  write (*, '(x,a10,9(f10.5))') ANAZ, (h(i, 2), i = 0, 8)
  write (*, '(x,a10,9(f10.5))') ANBX, (h(i, 3), i = 0, 8)
  write (*, '(x,a10,9(f10.5))') ANBY, (h(i, 4), i = 0, 8)
  write (*, '(x,a10,9(f10.5))') ANBZ, (h(i, 5), i = 0, 8)
  write (*, '(x,a10,9(f10.5))') ANCX, (h(i, 6), i = 0, 8)
  write (*, '(x,a10,9(f10.5))') ANCY, (h(i, 7), i = 0, 8)
  write (*, '(x,a10,9(f10.5))') ANCZ, (h(i, 8), i = 0, 8)
  write (*, '(x,10a1,9(x,9a1))') ('-', i = 1, 91 )
  call diag(h, d, 9)
  do i = 0, 8
    k = binary_search(abs(d(i)), dxa, i)
    do j = i - 1, k + 1, -1
      dvx(j+1) = dvx(j); dxa(j+1) = dxa(j)
    end do
    dvx(k+1) = d(i); dxa(k+1) = abs(d(i))
  end do
  write (*, '(/,x,a,/,3(/,x,3(g20.10)))') 'Hessian eigenvalues:', dvx
  do i = 0, 8
    s = 0d0
    ms = 0d0
    do j = 0, 2
      h(j, i) = h(j, i) / sqrt(ama)
    end do
    do j = 3, 5
      h(j, i) = h(j, i) / sqrt(amb)
    end do
    do j = 6, 8
      h(j, i) = h(j, i) / sqrt(amc)
    end do
    do j = 0, 8
      s = s + h(j, i)**2
      if (abs(h(j, i)) > abs(ms)) ms = h(j, i)
    end do
    s = sqrt(s)
    if (ms < 0d0) s = -s
    do j = 0, 8
      h(j, i) = h(j, i) / s
    end do
  end do
  gdn = 0
  zpe = 0d0
  do i = 0, 8
    ima = d(i) < 0d0
    d(i) = sqrt(abs(d(i)) / (amutokg / autokg)) / (2d0 * pi * c) * auvtocm
    if (d(i) < zero_cut) then
      gd(gdn(0), 0) = d(i)
      gdi(gdn(0), 0) = i
      gdn(0) = gdn(0) + 1
    else if (ima) then
      gd(gdn(1), 1) = d(i)
      gdi(gdn(1), 1) = i
      gdn(1) = gdn(1) + 1
    else
      gd(gdn(2), 2) = d(i)
      gdi(gdn(2), 2) = i
      gdn(2) = gdn(2) + 1
      zpe = zpe + d(i)
    end if
  end do
  zpe = zpe * 0.5d0
  call car_evald(cx, grad)
  grad(0:2) = grad(0:2) * sqrt(ama)
  grad(3:5) = grad(3:5) * sqrt(amb)
  grad(6:8) = grad(6:8) * sqrt(amc)
  s = 0d0
  do i = 0, 8
    s = s + grad(i)**2
  end do
  write (*, '(/,x,a20,g20.10)') 'Gradient norm:', sqrt(s)
  if (gdn(1) /= 0) then
    write (*, '(/,'' Imaginary Vibration  Wavenumber'',/,''        Nr             [1/cm] '')')
    do i = 0, gdn(1) - 1
      write (*, '(6x,i3,10x,f10.2)') i+1, gd(i, 1)
    end do
  end if
  if (gdn(0) /= 0) then
    write (*, '(/,''   Low Vibration      Wavenumber'',/,''        Nr             [1/cm] '')')
    do i = 0, gdn(0) - 1
      write (*, '(6x,i3,10x,f10.2)') i+1, gd(i, 0)
    end do
  end if
  if (gdn(2) /= 0) then
    write (*, '(/,''     Vibration        Wavenumber'',/,''        Nr             [1/cm] '')')
    do i = 0, gdn(2) - 1
      write (*, '(6x,i3,10x,f10.2)') i+1, gd(i, 2)
    end do
  end if
  do i = 2, 0, -1
    if (gdn(i) /= 0) then
      if (i == 2) write (*, '(/,x,a)') 'Normal Modes'
      if (i == 1) write (*, '(/,x,a)') 'Normal Modes of imaginary frequencies'
      if (i == 0) write (*, '(/,x,a)') 'Normal Modes of low/zero frequencies'
      write (*, '(x,24x,9i12)') (j+1, j = 0, gdn(i)-1)
      write (*, '(x,a24,9f12.2)') 'Wavenumbers [cm-1]', (gd(j, i), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANAX, (h(0, gdi(j, i)), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANAY, (h(1, gdi(j, i)), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANAZ, (h(2, gdi(j, i)), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANBX, (h(3, gdi(j, i)), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANBY, (h(4, gdi(j, i)), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANBZ, (h(5, gdi(j, i)), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANCX, (h(6, gdi(j, i)), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANCY, (h(7, gdi(j, i)), j = 0, gdn(i)-1)
      write (*, '(x,a19,5x,9f12.5)') ANCZ, (h(8, gdi(j, i)), j = 0, gdn(i)-1)
    end if
  end do
  write (*, '(/,x,''Zero point energy:'',f12.8,'' [H]'',f12.2,'' [1/CM]'',f12.2,'' [KJ/MOL]'')') &
    zpe / autocm, zpe, zpe / autocm * autokj
  write (*, '(x,18x,f12.2,'' [KCAL/MOL]'')') zpe / autocm * autokcal
  write (*, '(/,x,a)') 'Frequency calculation finished.'
end subroutine run_freq
#endif