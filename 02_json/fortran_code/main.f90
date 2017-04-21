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

!  call exercise4()

!   call exercise5()

  call exercise6()

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
    integer :: iimaxT, iiminT, iimaxprec, iiprec15
    integer :: over90, under32
    logical :: drought_flag
    real(wp) :: maxprec, prec15, maxprec15
    character(64) :: myfmt

    ! Determine maximum temperature, minimum temperature,
    ! maximum precipitation, longest drought,
    ! wettest 15 day period, number of days above 90, and
    ! number of days below 32
    maxT = 0; minT = 200; maxprec = 0.0; drought_flag = .false.;
    dlen = 0; dlenmax = 0; iidstartmax = 0;
    iiprec15 = 0; prec15 = 0.0; maxprec15 = 0.0;
    over90 = 0; under32 = 0;
    do ii = 1,dnum
       
       if(maxT .le. Tmax(ii)) then ! Tmax
          maxT = Tmax(ii); iimaxT = ii;
       end if
       
       if((minT .ge. Tmin(ii)).and.(Tmin(ii).gt.-1000)) then ! Tmin
          minT = Tmin(ii); iiminT = ii;
       end if
       
       if(maxprec .le. prec(ii)) then ! Max precipitation
          maxprec = prec(ii); iimaxprec = ii;
       end if

       if((prec(ii) .le. 0.0001) .and. (snow(ii) .le. 0.0001)) then ! Drought (no rain or snow)
          if(drought_flag) then
             dlen = dlen + 1;
          else
             dlen = 1; iidstart = ii; drought_flag = .true.;
          end if
       end if
       if ((prec(ii) .gt. 0.0001) .or. (snow(ii) .gt. 0.0001) .or. (ii .eq. dnum)) then
          if(drought_flag) then
             drought_flag = .false.;
             if(dlen .ge. dlenmax) then
                dlenmax = dlen; iidstartmax = iidstart;
             end if
          end if
       end if

       if(prec(ii) .gt. -1000) prec15 = prec15 + prec(ii); ! Wettest 15 day period
       if(ii .gt. 15) then
          if (prec(ii-15) .gt. -1000) prec15 = prec15 - prec(ii-15);
       end if
       if(prec15 .ge. maxprec15) then
          maxprec15 = prec15; iiprec15 = maxofint(ii-14,1);
       end if

       if(Tmax(ii) .gt. 90) over90 = over90 + 1; ! Days over 90
       if(Tmin(ii) .lt. 32) under32 = under32 + 1; ! Days under 32
       
    end do

    ii = 1; jj = dnum;
    myfmt = "(A23,A1,A1,A1,A1,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Time period considered:',char(9),char(9),char(9),char(9), &
         month(ii),'-',day(ii),'-',year(ii),' to ',month(jj),'-',day(jj),'-',year(jj)         
    ii = iimaxT; myfmt = "(A29,I3,A4,A1,A1,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Date of highest temperature (',maxT,' F):',char(9),char(9),month(ii),'-', &
         day(ii),'-', year(ii)
    ii = iiminT; myfmt = "(A28,I3,A4,A1,A1,I2,A1,I2,A1,I4)"    
    write(*,myfmt) 'Date of lowest temperature (',minT,' F):',char(9),char(9),month(ii),'-', &
         day(ii),'-', year(ii)    
    ii = iimaxprec; myfmt = "(A31,F4.2,A5,A1,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Date of highest precipitation (',maxprec,' in):',char(9), &
         month(ii),'-',day(ii),'-', year(ii)
    ii = iidstartmax; jj = minofint(iidstartmax + dlenmax - 1,dnum);
    myfmt = "(A17,I3,A7,A1,A1,A1,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Longest drought (', dlenmax,' days):',char(9),char(9),char(9), &
         month(ii),'-',day(ii),'-',year(ii),' to ',month(jj),'-',day(jj),'-',year(jj)
    ii = iiprec15; jj = ii + 14;
    myfmt = "(A23,F5.2,A5,A1,A1,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Wettest 15 day period (',maxprec15,' in):',char(9),char(9), &
         month(ii),'-',day(ii),'-',year(ii),' to ',month(jj),'-',day(jj),'-',year(jj)
    write(*,'(A25,A1,A1,A1,I4)') 'Number of days over 90 F:',char(9),char(9),char(9),over90
    write(*,'(A26,A1,A1,A1,I4)') 'Number of days under 32 F:',char(9),char(9),char(9),under32

    return

  end subroutine exercise4
  

!-----------------------------------------------------------------------
!
!  subroutine exercise5()
!
!  Do exercise 5 of assignment.
!
!-----------------------------------------------------------------------

  subroutine exercise5()
    implicit none

    ! Local
    integer :: bin, ii, jj, iidrfirst, iidrsecond, iidrthird, iidstart
    integer :: dlenf, dlens, dlent, dlen, yday, ydaymaxw, ydaymaxs, ydaymaxt
    integer :: ydayminw, ydaymins, ydaymint
    integer :: ymmaxw, ymmaxs, ymmaxt, ymminw, ymmins, ymmint
    integer :: ydmaxw, ydmaxs, ydmaxt, ydminw, ydmins, ydmint    
    integer, dimension(12) :: daysinmonth
    integer, dimension(14) :: above90
    real(wp) :: maxwr, maxsr, maxtr, minwr, minsr, mintr
    real(wp), dimension(14) :: totprec, waterprec, snowprec
    real(wp), dimension(366) :: ydaywprec, ydaysprec, ydaytprec
    logical :: date_check, date_check2, drought_flag
    character(64) :: myfmt

    daysinmonth(1:12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)

    infilename = ""; infilename = "../data/ex5_processed.csv"
    call reloaddatafile()

    do jj = 1,14
       above90(jj) = 0;
       totprec(jj) = 0.0; waterprec(jj) = 0.0; snowprec(jj) = 0.0;
    end do

    do jj = 1,366
       ydaywprec = 0.0; ydaysprec = 0.0; ydaytprec = 0.0;
    end do
    
    bin = 1; jj = 1;
    dlen = 0; dlenf = 0; dlens = -1; dlent = -2; drought_flag = .false.
    do ii = 1,dnum

       ! Assign day number to date, map 29th Feb. to 366
       yday = 0;
       do jj = 1,maxofint(month(ii)-1,1)
          yday = yday + daysinmonth(jj);
       end do
       if(month(ii) .eq. 1) yday = yday - daysinmonth(1);
       yday = yday + day(ii);
       if((month(ii).eq.2).and.(day(ii).eq.29)) yday = 366;

       bin = floor(0.2*(year(ii) - 1950)) + 1;
       ! Check if date is between July 15 and August 25
       date_check = .false.;       
       if(((month(ii).eq.7).and.(day(ii).ge.15)).or.((month(ii).eq.8).and.(day(ii).le.25))) then
          date_check = .true.
       end if

       ! Check if date is between November and February (winter)
       date_check2 = .false.;       
       if((month(ii).ge.11).or.(month(ii).le.2)) then
          date_check2 = .true.
          if((month(ii).le.2).and.(year(ii).eq.1950)) date_check2 = .false. 
       end if
       
       ! Late summer days above 90, in 5 year periods
       if(date_check .and. (Tmax(ii).gt.90)) then
          above90(bin) = above90(bin) + 1
       end if

       ! Finding three longest drought durations
       if((prec(ii) .le. 0.0001) .and. (snow(ii) .le. 0.0001)) then ! Drought (no rain or snow)
          if(drought_flag) then
             dlen = dlen + 1;
          else
             dlen = 1; iidstart = ii; drought_flag = .true.;
          end if
       end if
       if ((prec(ii) .gt. 0.0001) .or. (snow(ii) .gt. 0.0001) .or. (ii .eq. dnum)) then
          if(drought_flag) then
             drought_flag = .false.;
             if(dlen .ge. dlent) then
                if(dlen .lt. dlens) then
                   dlent = dlen; iidrthird = iidstart;
                else if(dlen .lt. dlenf) then
                   dlent = dlens; dlens = dlen;
                   iidrthird = iidrsecond;
                   iidrsecond = iidstart;
                else
                   dlent = dlens; dlens = dlenf; dlenf = dlen;
                   iidrthird = iidrsecond;
                   iidrsecond = iidrfirst;
                   iidrfirst = iidstart;
                end if
             end if
          end if
       end if

       ! Winter precipitation
       if(date_check2) then
          if((month(ii).le. 2).and.((1.0*bin).eq.(0.2*(year(ii)-1950)+1))) then
             bin = maxofint(bin-1,1)
          end if
          totprec(bin) = totprec(bin) + maxof(0.0_wp,prec(ii)) + maxof(0.0_wp,snow(ii));
          waterprec(bin) = waterprec(bin) + maxof(0.0_wp,prec(ii));
          snowprec(bin) = snowprec(bin) + maxof(0.0_wp,snow(ii));          
       end if

       ! Store cumulative day-of-year precipitation
       ydaywprec(yday) = ydaywprec(yday) + maxof(0.0_wp,prec(ii));
       ydaysprec(yday) = ydaysprec(yday) + maxof(0.0_wp,snow(ii));
       ydaytprec(yday) = ydaytprec(yday) + maxof(0.0_wp,prec(ii)) + maxof(0.0_wp,snow(ii));       

    end do

    ! Continuing day-of-year precipitation
    ydaymaxw = 0; ydaymaxs = 0; ydaymaxt = 0;
    ydayminw = 0; ydaymins = 0; ydaymint = 0;    
    maxwr = 0.0; maxsr = 0.0; maxtr = 0.0;
    minwr = 100000.0; minsr = 100000.0; mintr = 100000.0;    
    ydaywprec(366) = 4.0*ydaywprec(366); ! Scale up Feb. 29th
    ydaysprec(366) = 4.0*ydaysprec(366);
    ydaytprec(366) = 4.0*ydaytprec(366);
!    write(122,*) 'day, snow, rain, total'
    do jj = 1,366
!       write(122,*) jj,',',ydaysprec(jj),',',ydaywprec(jj),',',ydaytprec(jj)
       if(ydaywprec(jj) .ge. maxwr) then
          maxwr = ydaywprec(jj); ydaymaxw = jj;
       end if
       if(ydaysprec(jj) .ge. maxsr) then
          maxsr = ydaysprec(jj); ydaymaxs = jj;
       end if
       if(ydaytprec(jj) .ge. maxtr) then
          maxtr = ydaytprec(jj); ydaymaxt = jj;
       end if
       if(ydaywprec(jj) .le. minwr) then
          minwr = ydaywprec(jj); ydayminw = jj;
       end if
       if(ydaysprec(jj) .le. minsr) then
          minsr = ydaysprec(jj); ydaymins = jj;
       end if
       if(ydaytprec(jj) .le. mintr) then
          mintr = ydaytprec(jj); ydaymint = jj;
       end if
    end do

    ydmaxw = 0; jj = 0;
    do ymmaxw = 1,12
       do ydmaxw = 1,daysinmonth(ymmaxw)
          if(ydaymaxw .eq. jj+ydmaxw) exit
       end do
       if(ydaymaxw .eq. jj+ydmaxw) exit       
       jj = jj + daysinmonth(ymmaxw)
    end do
    if(ydaymaxw .eq. 366) then
       ymmaxw = 2; ydmaxw = 29;
    end if
    ydmaxs = 0; jj = 0;
    do ymmaxs = 1,12
       do ydmaxs = 1,daysinmonth(ymmaxs)
          if(ydaymaxs .eq. jj+ydmaxs) exit
       end do
       if(ydaymaxs .eq. jj+ydmaxs) exit       
       jj = jj + daysinmonth(ymmaxs)
    end do
    if(ydaymaxs .eq. 366) then
       ymmaxs = 2; ydmaxs = 29;
    end if
    ydmaxt = 0; jj = 0;
    do ymmaxt = 1,12
       do ydmaxt = 1,daysinmonth(ymmaxt)
          if(ydaymaxt .eq. jj+ydmaxt) exit
       end do
       if(ydaymaxt .eq. jj+ydmaxt) exit       
       jj = jj + daysinmonth(ymmaxt)
    end do
    if(ydaymaxt .eq. 366) then
       ymmaxt = 2; ydmaxt = 29;
    end if

    ydminw = 0; jj = 0;
    do ymminw = 1,12
       do ydminw = 1,daysinmonth(ymminw)
          if(ydayminw .eq. jj+ydminw) exit
       end do
       if(ydayminw .eq. jj+ydminw) exit       
       jj = jj + daysinmonth(ymminw)
    end do
    if(ydayminw .eq. 366) then
       ymminw = 2; ydminw = 29;
    end if
    ydmins = 0; jj = 0;
    do ymmins = 1,12
       do ydmins = 1,daysinmonth(ymmins)
          if(ydaymins .eq. jj+ydmins) exit
       end do
       if(ydaymins .eq. jj+ydmins) exit       
       jj = jj + daysinmonth(ymmins)
    end do
    if(ydaymins .eq. 366) then
       ymmins = 2; ydmins = 29;
    end if
    ydmint = 0; jj = 0;
    do ymmint = 1,12
       do ydmint = 1,daysinmonth(ymmint)
          if(ydaymint .eq. jj+ydmint) exit
       end do
       if(ydaymint .eq. jj+ydmint) exit       
       jj = jj + daysinmonth(ymmint)
    end do
    if(ydaymint .eq. 366) then
       ymmint = 2; ydmint = 29;
    end if
    
    ii = 1; jj = dnum;
    myfmt = "(A23,A1,A1,A1,A1,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Time period considered:',char(9),char(9),char(9),char(9), &
         month(ii),'-',day(ii),'-',year(ii),' to ',month(jj),'-',day(jj),'-',year(jj)         
    
    write(*,*) 'period, number of days above 90 F between July 15 and Aug 25'
    do jj = 0,12
       write(*,'(I4,A1,I4,A1,I2)') 1950+jj*5,'-',1955+jj*5,',',above90(jj+1)
    end do

    ii = iidrfirst; jj = minofint(iidrfirst+dlenf-1,dnum);
    myfmt = "(A17,I3,A7,A1,A1,A1,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Longest drought (',dlenf,' days):',char(9),char(9),char(9), &
         month(ii),'-',day(ii),'-',year(ii),' to ',month(jj),'-',day(jj),'-',year(jj)
    ii = iidrsecond; jj = minofint(iidrsecond+dlens-1,dnum);
    myfmt = "(A24,I3,A7,A1,A1,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I4)"    
    write(*,myfmt) 'Second longest drought (',dlens,' days):',char(9),char(9), &
         month(ii),'-',day(ii),'-',year(ii),' to ',month(jj),'-',day(jj),'-',year(jj)
    ii = iidrthird; jj = minofint(iidrthird+dlent-1,dnum);
    myfmt = "(A23,I3,A7,A1,A1,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I4)"
    write(*,myfmt) 'Third longest drought (',dlent,' days):',char(9),char(9), &
         month(ii),'-',day(ii),'-',year(ii),' to ',month(jj),'-',day(jj),'-',year(jj)


    write(*,*) 'period, snow, rain, total'
    do jj = 0,12
       write(*,'(I4,A1,I4,A1,F6.2,A1,F6.2,A1,F6.2)') 1950+jj*5,'-',1955+jj*5,',', &
            snowprec(jj+1),',',waterprec(jj+1),',',totprec(jj+1)
    end do
    
    write(*,'(A24,A1,I2,A1,I2)') 'Day of highest rainfall:',char(9),ymmaxw,'-',ydmaxw
    write(*,'(A24,A1,I2,A1,I2)') 'Day of highest snowfall:',char(9),ymmaxs,'-',ydmaxs
    write(*,'(A26,A1,I2,A1,I2)') 'Day of highest tot. prec.:',char(9),ymmaxt,'-',ydmaxt

    write(*,'(A23,A1,A1,I2,A1,I2)') 'Day of lowest rainfall:',char(9),char(9), &
         ymminw,'-',ydminw

    return
    
  end subroutine exercise5


!-----------------------------------------------------------------------
!
!  subroutine exercise6()
!
!  Do exercise 6 of assignment.
!
!-----------------------------------------------------------------------

  subroutine exercise6()
    implicit none

    ! Local
    integer :: ii, jj, kk, lrows, lcols, fid, status
    real, dimension(68) :: wintersnow, latespringsnow
    logical :: winter_check, latespring_check

    real(wp), allocatable, dimension(:) :: lat, lon, oldlat, oldlon
    real(wp), allocatable, dimension(:) :: maxtemp
    real(wp), allocatable, dimension(:,:) :: tempout, tempout2
    character(1024) :: tempbuf

    ! netCDF stuff
    integer :: ncid, latid, lonid, maxtempid
    integer :: latdimid, londimid, dimids(2), dimids2(2)

    
    infilename = ""; infilename = "../data/ex6_processed.csv"
    call reloaddatafile()

    do jj = 1,68
       wintersnow(jj) = 0.0; latespringsnow(jj) = 0.0;
    end do

    do ii = 1,dnum

       ! Check if date is between April 1 and May 15
       latespring_check = .false.;       
       if((month(ii).eq.4).or.((month(ii).eq.5).and.(day(ii).le.15))) then
          latespring_check = .true.
       end if

       ! Check if date is between November 1 and February 15 (winter)
       winter_check = .false.;       
       if((month(ii).ge.11).or.((month(ii).le.2).and.(day(ii).le.15))) then
          winter_check = .true.
       end if

       if(latespring_check.and.(snow(ii).ge.0.0)) then
          jj = year(ii)+1-1950;
          latespringsnow(jj) = latespringsnow(jj) + snow(ii);
       end if

       if(winter_check.and.(snow(ii).ge.0.0)) then
          jj = year(ii)+1-1950;
          wintersnow(jj) = wintersnow(jj) + snow(ii);
       end if
       
    end do

!    write(123,*) 'year, winter_snow, latespring_snow'
!    do jj = 1,68
!       write(123,'(I4,A1,F5.2,A1,F5.2)') jj+1949,',',wintersnow(jj),',', &
!            latespringsnow(jj)
!    end do

    infilename = ""; infilename = "../data/ex6_p2.csv"
    call firstpass(infilename,data_delim,inftype,intarray,debug_trace)
    lrows = intarray(1); lcols = intarray(2);

    allocate(lat(lrows),lon(lrows),maxtemp(lrows),tempout(lrows,lrows))
    allocate(oldlat(lrows),oldlon(lrows),tempout2(lrows,3))


    ! Open file
    status = 0
    fid = getu()
    open( UNIT=fid, FILE=infilename(1:indlnb(infilename)), STATUS='old', &
         ERR=85, IOSTAT=status )

    read(fid,'(A)') tempbuf ! read header line
    do ii = 1,lrows ! read all lat, lon, maxtemp
       read(fid,*) lat(ii), lon(ii), maxtemp(ii)
       do jj = 1,lrows
          tempout(ii,jj) = 0.0; ! NAN;
       end do
       !       tempout(ii,ii) = maxtemp(ii)
       tempout2(ii,1) = lat(ii);
       tempout2(ii,2) = lon(ii);
       tempout2(ii,3) = maxtemp(ii);
    end do


    close(fid)
    
    oldlat = lat; oldlon = lon;

    call sort(lat,lrows); call sort(lon,lrows);

    do ii = 1,lrows
       do jj = 1,lrows
          if(oldlat(ii).eq.lat(jj)) exit
       end do
       do kk = 1,lrows
          if(oldlon(ii).eq.lon(kk)) exit
       end do
       if(maxtemp(ii).ne.0.0) then
          tempout(jj,kk) = maxtemp(ii);
          tempout(maxofint(jj-1,1),kk) = tempout(jj,kk);
          tempout(minofint(jj+1,lrows),kk) = tempout(jj,kk);
          tempout(jj,maxofint(kk-1,1)) = tempout(jj,kk);
          tempout(jj,minofint(kk+1,lrows)) = tempout(jj,kk);
          tempout(maxofint(jj-2,1),kk) = tempout(jj,kk);
          tempout(minofint(jj+2,lrows),kk) = tempout(jj,kk);
          tempout(jj,maxofint(kk-2,1)) = tempout(jj,kk);
          tempout(jj,minofint(kk+2,lrows)) = tempout(jj,kk);
          tempout(maxofint(jj-1,1),maxofint(kk-1,1)) =tempout(jj,kk);
          tempout(minofint(jj+1,lrows),minofint(kk+1,lrows)) = tempout(jj,kk);
          tempout(maxofint(jj-1,1),maxofint(kk-1,1)) = tempout(jj,kk);
          tempout(minofint(jj+1,lrows),minofint(kk+1,lrows)) = tempout(jj,kk);
       end if
    end do

    do ii = 1,lrows
       if(ii.gt.1) then
          if(lat(ii).eq.lat(ii-1)) lat(ii) = lat(ii)+0.001
          if(lon(ii).eq.lon(ii-1)) lon(ii) = lon(ii)+0.001          
       end if
    end do

    ! Remove NANs
!    if(.false.) then
    do kk  = 1,1
       do ii = 1,lrows
          do jj = 1,lrows
             if(tempout(ii,jj).eq.0.0) tempout(ii,jj) = NAN;
          end do
       end do
    end do
! end if
 
    infilename = ""; infilename = "../plots/ex6_p2_netcdf.nc"
    ! Create netCDF file.
    call check(nf90_create(infilename,NF90_CLOBBER,ncid))
    ! Define dimensions
    call check(nf90_def_dim(ncid,"latitude",lrows,latdimid))
    call check(nf90_def_dim(ncid,"longitude",lrows,londimid))
    dimids = (/ latdimid, londimid /)
    ! Define the variables
    call check(nf90_def_var(ncid,"latitude",NF90_REAL,latdimid,latid))
    call check(nf90_def_var(ncid,"longitude",NF90_REAL,londimid,lonid))
    call check(nf90_def_var(ncid,"Tmax",NF90_REAL,dimids,maxtempid))
    ! End define mode
    call check(nf90_enddef(ncid))
    ! Write data to file
    call check(nf90_put_var(ncid, latid, lat))
    call check(nf90_put_var(ncid, lonid, lon))
    call check(nf90_put_var(ncid, maxtempid, tempout))
    ! Close file
    call check(nf90_close(ncid))

!    infilename = ""; infilename = "../plots/ex6_p2_netcdf2.nc"
    ! Create netCDF file.
!    call check(nf90_create(infilename,NF90_CLOBBER,ncid))
    ! Define dimensions
!    call check(nf90_def_dim(ncid,"index",lrows,latdimid))
!    call check(nf90_def_dim(ncid,"value",3,londimid))
!    dimids2 = (/ latdimid, londimid /)
    ! Define the variables
!    call check(nf90_def_var(ncid,"Tmax",NF90_REAL,dimids2,maxtempid))
    ! End define mode
!    call check(nf90_enddef(ncid))
    ! Write data to file
!    call check(nf90_put_var(ncid, maxtempid, tempout2))
    ! Close file
!    call check(nf90_close(ncid))
    
    deallocate(lat,lon,maxtemp,tempout,oldlat,oldlon,tempout2)
    
    return

85 continue
    write(0,*)  'exercise(): Error while opening config file "', infilename,'", error: ', &
         status
   stop      
    

  end subroutine exercise6

!-----------------------------------------------------------------------
!
!  subroutine check()
!
!  Required for netCDF stuff in exercise6()
!
!-----------------------------------------------------------------------
  
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  
  
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

    call firstpass(infilename,data_delim,inftype,intarray,debug_trace)
    dnum = intarray(1); dcols = intarray(2);
  
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
