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
  use netcdf  

  implicit none

  call parse_params()

  !call printheaders(infilename, inftype)
  !write(*,*) numcols(",1,2, 3,  ",",")
  call firstpass(infilename,data_delim,inftype,intarray,debug_trace)
  dnum = intarray(1); dcols = intarray(2);
  
  call allocarrays()

  call readalldata(infilename,data_delim,inftype,intarray,debug_trace)

!   call exercise2(.true.)

  !  call exercise6()

  call exercise7()  

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

    allocate(xdat(dnum),ydat(dnum),edat(dnum),wdat(dnum),rdat(dnum))
    allocate(skipdat(dnum),colnames(dcols))

  end subroutine allocarrays
  
!-----------------------------------------------------------------------
!
!  subroutine exercise2(ptrace)
!
!  Performs exercise 2.
!  ptrace is logical in for print_trace.
!
!-----------------------------------------------------------------------

  subroutine exercise2(ptrace)
    implicit none

    logical, intent(in) :: ptrace
    
    ! Local
    integer :: ii
    character(64) :: myfmt

    do ii = 1,dnum
       edat(ii) = 0.0; wdat(ii) = 1.0; skipdat(ii) = .false.;
    end do

    call linreg(xdat,ydat,edat,wdat,skipdat,dnum,linparams,rdat,ptrace)

    ! write file with data points, and linear fit
!    myfmt = "(A1,F4.2,A1,F6.0,A2)"
!    write(122,*) 'x,data,fit'
!    do ii = 1,dnum
!       write(122,myfmt) '[',xdat(ii),',',ydat(ii),'],'
!    end do
!    write(*,*) xdat(1), linparams(1)*xdat(1)+linparams(2)
    !    write(*,*) xdat(dnum), linparams(1)*xdat(dnum)+linparams(2)

    ! Exercise 3
    if(ptrace) then
       write(*,*) 'f(x = 1.2) = ', linparams(1)*1.2+linparams(2), '+/-', &
            linparams(5)*1.2
    end if

    ! Exercise 5, plot residuals
!    myfmt = "(F4.2,A1,F5.0)"
!    write(123,*) 'x,residual'
!    do ii = 1,dnum
!       write(123,myfmt) xdat(ii),',',rdat(ii)
!    end do
    

  end subroutine exercise2

!-----------------------------------------------------------------------
!
!  subroutine exercise6()
!
!  Performs exercise 6.
!
!-----------------------------------------------------------------------

  subroutine exercise6()
    implicit none

    ! Local
    integer :: ii
    real(wp) :: ressig, resmean

    call exercise2(.false.)

    ! Compute residual sigma
    ressig = 0.0; resmean = 0.0;
    do ii = 1,dnum
       ressig = ressig + rdat(ii)*rdat(ii);
       resmean = resmean + rdat(ii);
    end do
    ressig = sqrt((ressig - resmean*resmean/dnum)/dnum);

    ! skip residuals beyond 2.5 sigma
    do ii = 1,dnum
       if(abs(rdat(ii)) .gt. 2.5*ressig) skipdat(ii) = .true.;
    end do

    call linreg(xdat,ydat,edat,wdat,skipdat,dnum,linparams,rdat,.true.)

    write(*,*) 'f(x = 1.2) = ', linparams(1)*1.2+linparams(2), '+/-', &
         linparams(5)*1.2
    

  end subroutine exercise6
    
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

    deallocate(xdat,ydat,edat,wdat,rdat)
    deallocate(skipdat,colnames)
    
  end subroutine dealloc
  
!-----------------------------------------------------------------------
!
!  subroutine exercise7()
!
!  Performs exercise 7.
!
!-----------------------------------------------------------------------

  subroutine exercise7()
    implicit none

    ! Local
    integer :: ii, jj, kk
    real(wp) :: yrm, Tm, Tsig
    character(64) :: myfmt
    
    call dealloc()

    infilename = ""; infilename = "../data/a2.csv"
    call firstpass(infilename,data_delim,inftype,intarray,.true.)
    dnum = intarray(1); dcols = intarray(2);
  
    call allocarrays()

    call readalldata2(infilename,data_delim,inftype,intarray,debug_trace)

    jj = 0; yrm = 0.0; Tm = 0.0; Tsig = 0.0;
    kk = 1;
    do ii = 1,dnum
       jj = jj + 1;
       if(jj .eq. 10) then
          yr_mean(kk) = xdat(ii) - 4.5;
          T_mean(kk) = Tm/9.0;
          Tsig = Tsig/9.0-T_mean(kk)**2;
          T_sig(kk) = sqrt(Tsig);
          T_skip(kk) = .false.;
          T_wei(kk) = 1.0/(Tsig);
          T_ones(kk) = 1.0;
          kk = kk + 1;
          jj = 1; yrm = 0.0; Tm = 0.0; Tsig = 0.0;
       end if
       Tm = Tm + ydat(ii);
       Tsig = Tsig + ydat(ii)*ydat(ii);
    end do

    yr_mean(16) = 2015.5;
    T_mean(16) = (ydat(dnum-1)+ydat(dnum))/2.0;
    T_sig(16) = (ydat(dnum-1)**2+ydat(dnum)**2)/2.0 - T_mean(16)**2;
    T_wei(16) = 1.0/T_sig(16);
    T_sig(16) = sqrt(T_sig(16));
    T_skip(16) = .true.;
    dnum = 16;

    if(.false.) then
       do ii = 1,16
          write(*,*) yr_mean(ii), T_mean(ii), T_wei(ii), T_skip(ii)
       end do
    end if

    write(*,*) 'Unweighted, skip 2015 & 2016'
    call linreg(yr_mean,T_mean,T_sig,T_ones,T_skip,dnum,linparams,rdat,.true.)
    write(*,*) 'Weighted, skip 2015 & 2016'    
    call linreg(yr_mean,T_mean,T_sig,T_wei,T_skip,dnum,linparams,rdat,.true.)    

!    myfmt = "(A1,F6.1,A1,F5.2,A1,F5.2,A2)"
!    myfmt = "(A1,F6.1,A1,F5.2,A2)"    
!    do ii = 1,dnum
!       write(*,myfmt) '[',yr_mean(ii),',',T_mean(ii),'],'       
!       write(*,myfmt) '[',yr_mean(ii),',',T_mean(ii)-T_sig(ii),',',T_sig(ii)+T_mean(ii) &
!            ,'],'
!    end do
!    write(*,*) yr_mean(1), linparams(1)*yr_mean(1)+linparams(2)
!    write(*,*) yr_mean(dnum), linparams(1)*yr_mean(dnum)+linparams(2)

    T_skip(16) = .false.;
    write(*,*) 'Unweighted, skip nothing'    
    call linreg(yr_mean,T_mean,T_sig,T_ones,T_skip,dnum,linparams,rdat,.true.)
    write(*,*) 'T in 2050 = ', linparams(1)*2050.0+linparams(2)
    write(*,*) 'Weighted, skip nothing'        
    call linreg(yr_mean,T_mean,T_sig,T_wei,T_skip,dnum,linparams,rdat,.true.)
    write(*,*) 'T in 2050 = ', linparams(1)*2050.0+linparams(2)    
 
  end subroutine exercise7
  

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
