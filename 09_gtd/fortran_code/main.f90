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
!  use netcdf  

  implicit none

  call parse_params()

  !call printheaders(infilename, inftype)
  !write(*,*) numcols(",1,2, 3,  ",",")
  call firstpass(infilename,data_delim,inftype,intarray,.true.)
  dnum = intarray(1); ncols = intarray(2);
  
  call allocarrays()

  call readalldata(infilename,data_delim,inftype,intarray,debug_trace)

  !  call exercise2()
  call exercise3()

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

    allocate(month(dnum),day(dnum),year(dnum),dayenum(dnum),country(dnum))
    allocate(atype(dnum),tartype(dnum),wtype(dnum),nkill(dnum),nwound(dnum))
    allocate(lat(dnum),lon(dnum),eventid(dnum))
    allocate(colnames(ncols),gid(dnum))

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

    deallocate(month,day,year,dayenum,country)
    deallocate(atype,tartype,wtype,nkill,nwound)
    deallocate(lat,lon,eventid)
    deallocate(colnames,gid)

    deallocate(gname,aname,tname,wname,cname)
    
  end subroutine dealloc

!-----------------------------------------------------------------------
!
!  subroutine exercise2()
!
!  Performs exercise2
!
!-----------------------------------------------------------------------

  subroutine exercise2()
    implicit none

    ! Local
    integer :: ii, jj, kk
    logical :: is911event
    integer, dimension(year(dnum)-year(1)+1) :: USyearly, ryearly
    integer, dimension(ceiling((year(dnum)-year(1)+1)/5.0)) :: U5yr, r5yr
    integer, dimension(year(dnum)-year(1)+1) :: Udyr, rdyr
    integer, dimension(ceiling((year(dnum)-year(1)+1)/5.0)) :: Ud5yr, rd5yr
    integer, dimension(year(dnum)-year(1)+1) :: Utyr, rtyr
    integer, dimension(ceiling((year(dnum)-year(1)+1)/5.0)) :: Ut5yr, rt5yr
    character(64) :: myfmt

    jj = year(dnum)-year(1)+1;
    do ii = 1,jj
       USyearly(ii) = 0; ryearly(ii) = 0; Udyr(ii) = 0; rdyr(ii) = 0;
       Utyr(ii) = 0; rtyr(ii) = 0;
    end do
    do ii = 1,ceiling(jj/5.0)
       U5yr(ii) = 0; r5yr(ii) = 0; Ud5yr(ii) = 0; rd5yr(ii) = 0;
       Ut5yr(ii) = 0; rt5yr(ii) = 0;
    end do

    do ii = 1,dnum
       is911event = .false.;
       do kk = 1,4
          if(ii.eq.ii911s(kk)) is911event = .true.;
       end do
       if(.not.is911event) then
          if(country(ii).eq.217) then ! US
             USyearly(year(ii)-year(1)+1) = USyearly(year(ii)-year(1)+1) + 1;
             Udyr(year(ii)-year(1)+1) = Udyr(year(ii)-year(1)+1) + nkill(ii);
             Utyr(year(ii)-year(1)+1) = Utyr(year(ii)-year(1)+1) + nkill(ii) &
                  + nwound(ii);             
             kk = ceiling((year(ii)-year(1)+1)/5.0);
             U5yr(kk) = U5yr(kk) + 1;
             Ud5yr(kk) = Ud5yr(kk) + nkill(ii);
             Ut5yr(kk) = Ut5yr(kk) + nkill(ii) + nwound(ii);             
          else ! not US
             ryearly(year(ii)-year(1)+1) = ryearly(year(ii)-year(1)+1) + 1;
             rdyr(year(ii)-year(1)+1) = rdyr(year(ii)-year(1)+1) + nkill(ii);
             rtyr(year(ii)-year(1)+1) = rtyr(year(ii)-year(1)+1) + nkill(ii) &
                  + nwound(ii);             
             kk = ceiling((year(ii)-year(1)+1)/5.0);
             r5yr(kk) = r5yr(kk) + 1;
             rd5yr(kk) = rd5yr(kk) + nkill(ii);
             rt5yr(kk) = rt5yr(kk) + nkill(ii) + nwound(ii);             
          end if
       end if
    end do


    write(217,*) 'year, US(x500), non-US'
    write(218,*) 'year, US(x500), non-US'
    write(219,*) 'year, US(x500), non-US'    
    do ii = 1,jj
       kk = ii-1+year(1);
       ! events per year
       write(217,*) kk, ',', USyearly(ii)*500, ',', ryearly(ii)
       ! deaths per hear
       write(218,*) kk, ',', Udyr(ii)*500, ',', rdyr(ii)
       ! deaths and wounded per hear
       write(219,*) kk, ',', Utyr(ii)*500, ',', rtyr(ii)
    end do
    write(517,*) 'years, US(x500), non-US'
    write(518,*) 'years, US(x500), non-US'
    write(519,*) 'years, US(x500), non-US'
    write(myfmt,*) '(I4,A1,I4,A1,I6,A1,I6)'
    do ii = 1,ceiling(jj/5.0)-1
       kk = year(1)+(ii-1)*5;
       write(517,myfmt) kk,'-',kk+5,',',U5yr(ii)*500,',',r5yr(ii)
       write(518,myfmt) kk,'-',kk+5,',',Ud5yr(ii)*500,',',rd5yr(ii)
       write(519,myfmt) kk,'-',kk+5,',',Ut5yr(ii)*500,',',rt5yr(ii)
    end do
    

  end subroutine exercise2


!-----------------------------------------------------------------------
!
!  subroutine exercise3()
!
!  Performs exercise3
!
!-----------------------------------------------------------------------

  subroutine exercise3()
    implicit none

    ! Local
    integer, parameter :: nstrict = 18, nsoviet = 7
    integer, parameter :: edge1 = 153, edge2 = 209 ! Pakistan and Turkey
    integer, dimension(nstrict) :: strictlist
    integer, dimension(nsoviet) :: sovietlist
    integer*8 :: nevUS, nevstrict, nevsoviet, nevedge1, nevedge2
    integer :: ii, jj
    real(wp) :: avg, avgUS, avgedge1, avgedge2, avgsoviet, avgstrict
    logical :: in

    ! Jordan, Egypt, Lebanon, Iran, South Yemen, Kuwait, North Yemen, Syria, UAE,
    ! Afganistan, Iraq, Suadi Arabia, Libya, Bahrain, Qatar, Yemen, Israel, Palastine
    strictlist = (/ 102, 60, 110, 94, 406, 106, 377, 200, 215, 4, 95, 173, 113, &
         18, 164, 228, 97, 155 /)
    ! Azerbaijan, Armenia, Kazakhstan, Tajikistan, Uzbekistan, Kyrgyzstan, Turkmenistan
    sovietlist = (/ 16, 12, 103, 202, 129, 107, 210 /)

    nevUS = 0; nevstrict = 0; nevsoviet = 0; nevedge1 = 0; nevedge2 = 0;
    do ii = 1,dnum
       if(country(ii).eq.217) then ! US
          nevUS = nevUS + 1;
       elseif(country(ii).eq.edge1) then ! Pakistan
          nevedge1 = nevedge1 + 1;
       elseif(country(ii).eq.edge2) then ! Turkey
          nevedge2 = nevedge2 + 1;
       else
          in = .false. ! Strict Middle-East
          do jj = 1,nstrict
             if(country(ii).eq.strictlist(jj)) then
                in = .true.; exit;
             end if
          end do
          if(in) nevstrict = nevstrict + 1;
          in = .false. ! Ex-Soviet Bloc
          do jj = 1,nsoviet
             if(country(ii).eq.sovietlist(jj)) then
                in = .true.; exit;
             end if
          end do
          if(in) nevsoviet = nevsoviet + 1;
       end if
    end do

    ! US
    avg = nevUS*1.0/(year(dnum)-year(1)+1)
    write(*,*) 'Average events per year in the US in 1970-2015: ', avg
    write(*,*) 'Years in which US event rate was < 1% of average:'
    nevUS = 0; jj = year(1);
    do ii = 1,dnum
       if(country(ii).eq.217) then
          if(year(ii).eq.jj) then
             nevUS = nevUS + 1;
          else
             if((1.0*nevUS).le.0.01*avg) write(*,*) jj
             jj = year(ii); nevUS = 1;
          end if
       end if
    end do
    avgUS = avg;

    ! Pakistan
    avg = nevedge1*1.0/(year(dnum)-year(1)+1)
    write(*,*) 'Average events per year in Pakistan in 1970-2015: ', avg
    write(*,*) 'Years in which Pakistan event rate was < 1% of average:'
    nevedge1 = 0; jj = year(1);
    do ii = 1,dnum
       if(country(ii).eq.edge1) then
          if(year(ii).eq.jj) then
             nevedge1 = nevedge1 + 1;
          else
             if((1.0*nevedge1).le.0.01*avg) write(*,*) jj
             jj = year(ii); nevedge1 = 1;
          end if
       end if
    end do
    avgedge1 = avg;


    ! Turkey
    avg = nevedge2*1.0/(year(dnum)-year(1)+1)
    write(*,*) 'Average events per year in Turkey in 1970-2015: ', avg
    write(*,*) 'Years in which Turkey event rate was < 1% of average:'
    nevedge2 = 0; jj = year(1);
    do ii = 1,dnum
       if(country(ii).eq.edge2) then
          if(year(ii).eq.jj) then
             nevedge2 = nevedge2 + 1;
          else
             if((1.0*nevedge2).le.0.01*avg) write(*,*) jj
             jj = year(ii); nevedge2 = 1;
          end if
       end if
    end do
    avgedge2 = avg;
    
  end subroutine exercise3
  
  
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
