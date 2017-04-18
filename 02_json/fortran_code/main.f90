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

  implicit none

  call parse_params()

  !call printheaders(infilename, inftype)
  !write(*,*) numcols(",1,2, 3,  ",",")
  call firstpass(infilename,data_delim,inftype,intarray,.true.)
  dnum = intarray(1); dcols = intarray(2);
  
  call allocarrays()

  call readalldata(infilename,data_delim,inftype,intarray,debug_trace)

  call exercise4()

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

    allocate(year(dnum),month(dnum),day(dnum),Tmax(dnum),Tmin(dnum))
    allocate(prec(dnum),snow(dnum),snow_d(dnum))

    allocate(colnames(dcols))

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

    deallocate(year,month,day,Tmax,Tmin)
    deallocate(prec,snow,snow_d)

    deallocate(colnames)    
    
  end subroutine dealloc
  
  
!-----------------------------------------------------------------------
!
!  subroutine exercise4()
!
!  Do exercise 4 of assignment.
!
!-----------------------------------------------------------------------

  subroutine exercise4()
    implicit none

    !Local
    integer :: ii, jj, maxT, minT, iidstart, iidstartmax, dlen, dlenmax
    integer :: iimaxT, iiminT, iimaxprec
    logical :: drought_flag
    real(wp) :: maxprec
    character(64) :: myfmt

    ! Determine maximum temperature, minimum temperature,
    ! maximum precipitation, longest drought
    maxT = 0; minT = 200; maxprec = 0.0; drought_flag = .false.;
    dlen = 0; dlenmax = 0; iidstartmax = 0;
    do ii = 1,dnum
       if(maxT .le. Tmax(ii)) then
          maxT = Tmax(ii); iimaxT = ii;
       end if
       if(minT .ge. Tmin(ii)) then
          minT = Tmin(ii); iiminT = ii;
       end if
       if(maxprec .le. prec(ii)) then
          maxprec = prec(ii); iimaxprec = ii;
       end if
       if(prec(ii) .eq. 0.0) then
          if(drought_flag) then
             dlen = dlen + 1;
          else
             dlen = 1; iidstart = ii; drought_flag = .true.;
          end if
       else if ((prec(ii) .ne. 0.0) .or. (ii .eq. dnum)) then
          if(drought_flag) then
             drought_flag = .false.;
             if(dlen .ge. dlenmax) then
                dlenmax = dlen; iidstartmax = iidstart;
             end if
          end if
       end if
    end do

    ii = iimaxT; myfmt = "(A19,I3,A3,A1,A1,A1,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Date of highest T (',maxT,'F):',char(9),char(9),char(9),month(ii),'-', &
         day(ii),'-', year(ii)
    ii = iiminT; myfmt = "(A18,I3,A3,A1,A1,A1,I2,A1,I2,A1,I4)"    
    write(*,myfmt) 'Date of lowest T (',minT,'F):',char(9),char(9),char(9),month(ii),'-', &
         day(ii),'-', year(ii)    
    ii = iimaxprec; myfmt = "(A31,F4.2,A4,A1,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Date of highest precipitation (',maxprec,'in):',char(9),month(ii),'-', &
         day(ii),'-', year(ii)
    ii = iidstartmax; jj = iidstartmax + dlenmax - 1;
    myfmt = "(A17,I2,A8,A1,A1,A1,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Longest drought (', dlenmax,' days) :',char(9),char(9),char(9), &
         month(ii),'-',day(ii),'-',year(ii),' to ',month(jj),'-',day(jj),'-',year(jj)

    return

  end subroutine exercise4
  
    
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
