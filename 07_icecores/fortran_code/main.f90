!-----------------------------------------------------------------------
! 
!  main.f90
!  
!  
!-----------------------------------------------------------------------


program main

  use globals
  use param_parser
  use timing
  use utilities
  use fileio
  !  use interp        ! http://people.sc.fsu.edu/~jburkardt/f_src/interp/interp.html
  use fftsh_mat
  use netcdf  

  use, intrinsic :: ISO_C_BINDING, only : C_PTR, C_INT, C_DOUBLE
  implicit none

  integer :: Nexp, expnum, expnum2, expnum3, ii, jj, kk
  real(wp) :: dyrmax, dyrmin, dyr, tempreal, dfreq
  real(wp), allocatable, dimension(:) :: yr_interp, data_interp
  real(wp), allocatable, dimension(:) :: yr_interp2, data_interp2
  real(wp), allocatable, dimension(:) :: yr_interp3, data_interp3  
  complex(C_DOUBLE), allocatable, dimension(:) :: data_cmplx, fft_cmplx
  complex(C_DOUBLE), allocatable, dimension(:) :: data_cmplx2, fft_cmplx2
  complex(C_DOUBLE), allocatable, dimension(:) :: data_cmplx3, fft_cmplx3  

  call parse_params()

  !call printheaders(infilename, inftype)
  !write(*,*) numcols(",1,2, 3,  ",",")
  call firstpass(infilename,data_delim,inftype,intarray,debug_trace)
  dnum = intarray(1); ncols = intarray(2);
  
  call allocarrays()

  call readalldata(infilename,data_delim,inftype,intarray,debug_trace)

  dyrmax = 0.0; dyrmin = 1000000.0;
  do ii = 2,dnum
     tempreal = year(ii-1)-year(ii);
     if(tempreal.gt.dyrmax) dyrmax = tempreal;
     if(tempreal.lt.dyrmin) dyrmin = tempreal;
  end do


  write(*,*) 'dyrmax: ', dyrmax
  write(*,*) 'dyrmin: ', dyrmin
!  write(*,*) 'Num years: ', year(1)- year(dnum)
  !  write(*,*) 'N = ', Nexp, ', dyr = ',(year(1)-year(dnum))/(2**Nexp)

 
  do ii = 1,dnum
     year(ii) = 0.0 - year(ii);
  end do
  
  Nexp = 7; ! 7; ! 11; 15;
  expnum = 2**7; expnum2 = 2**11; expnum3 = 2**15;
  allocate(yr_interp(expnum),data_interp(expnum))
  allocate(yr_interp2(expnum2),data_interp2(expnum2))
  allocate(yr_interp3(expnum3),data_interp3(expnum3))  
  allocate(data_cmplx(expnum),fft_cmplx(expnum))
  allocate(data_cmplx2(expnum2),fft_cmplx2(expnum2))
  allocate(data_cmplx3(expnum3),fft_cmplx3(expnum3))  
  dyr = (year(1)-year(dnum))/(1.0*expnum-1.0);
  yr_interp(1) = year(1); 
  do ii = 2,expnum
     yr_interp(ii) = yr_interp(ii-1)-dyr;
  end do
  call interp_linear(1,dnum,year,delD,expnum,yr_interp,data_interp)
  dyr = (year(1)-year(dnum))/(1.0*expnum2-1.0);
  yr_interp2(1) = year(1); 
  do ii = 2,expnum2
     yr_interp2(ii) = yr_interp2(ii-1)-dyr;
  end do
  call interp_linear(1,dnum,year,delD,expnum2,yr_interp2,data_interp2)
  dyr = (year(1)-year(dnum))/(1.0*expnum3-1.0);
  yr_interp3(1) = year(1); 
  do ii = 2,expnum3
     yr_interp3(ii) = yr_interp3(ii-1)-dyr;
  end do
  call interp_linear(1,dnum,year,delD,expnum3,yr_interp3,data_interp3)

  

  tempreal = 0.0;
  do ii=1,expnum ! Complexify
     data_cmplx(ii) = cmplx(data_interp(ii),0.0);
     tempreal = tempreal + data_interp(ii);
  end do
  do ii=1,expnum2 ! Complexify
     data_cmplx2(ii) = cmplx(data_interp2(ii),0.0);
  end do
  do ii=1,expnum3 ! Complexify
     data_cmplx3(ii) = cmplx(data_interp3(ii),0.0);
  end do

  dfreq = 1.0/abs(yr_interp(expnum));
  call fftsh(data_cmplx, fft_cmplx, expnum, 0) ! Compute Fourier transform
  do ii = 1,dnum
     year(ii) = 0.0 - year(ii);
  end do
  call fftsh(data_cmplx2, fft_cmplx2, expnum2, 0) ! Compute Fourier transform
  do ii = 1,dnum
     year(ii) = 0.0 - year(ii);
  end do
  call fftsh(data_cmplx3, fft_cmplx3, expnum3, 0) ! Compute Fourier transform  

!  jj = expnum3/2+5;
!  write(*,*) Nexp, (real(fft_cmplx3(jj))**2+imag(fft_cmplx3(jj))**2)

  
!  if(.false.) then
  Nexp = 122;
  write(Nexp,*) 'omega,N=7,N=11,N=15'
  do ii = expnum/2+2,expnum/2+2+49
     jj = ii-expnum/2+expnum2/2;
     kk = ii-expnum/2+expnum3/2;     
     write(Nexp,*) (ii-expnum/2-1)*dfreq,',',(real(fft_cmplx(ii))**2 &
          +imag(fft_cmplx(ii))**2)/expnum,',',(real(fft_cmplx2(jj))**2 &
          +imag(fft_cmplx2(jj))**2)/expnum2,',',(real(fft_cmplx3(kk))**2 &
          +imag(fft_cmplx3(kk))**2)/expnum3
  end do
! end if

 if(.false.) then   
     write(Nexp,*) 'year,data'
     do ii = 1,expnum
        write(Nexp,*) -yr_interp3(ii),',',data_interp3(ii)
     end do
  end if

  deallocate(yr_interp,data_interp)
  deallocate(data_cmplx,fft_cmplx)
  
  call dealloc()

  if (time_trace) call print_time('s')

  stop

contains

!-----------------------------------------------------------------------
!
!  subroutine allocarrays()
!
!  Allocate arrays
!
!-----------------------------------------------------------------------

  subroutine allocarrays()
    implicit none

    allocate(testrealarray(testint))
    allocate(testint2darray(testint,testint))

    allocate(year(dnum),delD(dnum),colnames(ncols))

  end subroutine allocarrays
  

!-----------------------------------------------------------------------
!
!  subroutine dealloc()
!
!  deallocate arrays
!
!-----------------------------------------------------------------------

  subroutine dealloc()
    implicit none

    deallocate(testrealarray)
    deallocate(testint2darray)

    deallocate(year,delD,colnames)    
    
  end subroutine dealloc
  
!-----------------------------------------------------------------------
!
!  subroutine reloaddatafile()
!
!  Reloads data file. Takes care of array reallocations.
!
!-----------------------------------------------------------------------

  subroutine reloaddatafile()
    implicit none

    call dealloc()

    call firstpass(infilename,data_delim,inftype,intarray,.true.)
  
    call allocarrays()

    call readalldata(infilename,data_delim,inftype,intarray,debug_trace)
    

  end subroutine reloaddatafile
  

!-----------------------------------------------------------------------
!
!  subroutine timetrial()
!
!  Measures time taken for a specific number of iterations
!
!-----------------------------------------------------------------------

subroutine timetrial()
  implicit none

  call print_time('s')

  call print_time('s')
  
end subroutine timetrial


end program main
