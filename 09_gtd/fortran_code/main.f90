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
  call firstpass(infilename,data_delim,inftype,intarray,.true.)
  dnum = intarray(1); ncols = intarray(2);
  
  call allocarrays()

  call readalldata(infilename,data_delim,inftype,intarray,debug_trace)

  !  call exercise2()
  !  call exercise3()
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

    if(allocated(rho_ll)) deallocate(rho_ll)
    if(allocated(rho_xy)) deallocate(rho_xy)    
    if(allocated(lats)) deallocate(lats)
    if(allocated(lons)) deallocate(lons) 
    if(allocated(invxy)) deallocate(invxy)       

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
    integer :: ii, jj, kk
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

    ! Strict Middle-East
    avg = nevstrict*1.0/(year(dnum)-year(1)+1)
    write(*,*) 'Average events per year in strict Middle-East in 1970-2015: ', avg
    write(*,*) 'Years in which strict Middle-East event rate was < 1% of average:'
    nevstrict = 0; jj = year(1);
    do ii = 1,dnum
       in = .false.
       do kk = 1,nstrict
          if(country(ii).eq.strictlist(kk)) then
             in = .true.; exit;
          end if
       end do
       if(in) then
          if(year(ii).eq.jj) then
             nevstrict = nevstrict + 1;
          else
             if((1.0*nevstrict).le.0.01*avg) write(*,*) jj
             jj = year(ii); nevstrict = 1;
          end if
       end if
    end do
    avgstrict = avg;

    ! Soviet Middle-East
    avg = nevsoviet*1.0/(year(dnum)-year(1)+1)
    write(*,*) 'Average events per year in Ex-Soviet block in 1970-2015: ', avg
    write(*,*) 'Years in which Ex-soviet block event rate was < 1% of average:'
    nevsoviet = 0; jj = year(1);
    do ii = 1,dnum
       in = .false.
       do kk = 1,nsoviet
          if(country(ii).eq.sovietlist(kk)) then
             in = .true.; exit;
          end if
       end do
       if(in) then
          if(year(ii).eq.jj) then
             nevsoviet = nevsoviet + 1;
          else
             if((1.0*nevsoviet).le.0.01*avg) write(*,*) jj
             jj = year(ii); nevsoviet = 1;
          end if
       end if
    end do
    avgsoviet = avg;
    
    
  end subroutine exercise3

  
!-----------------------------------------------------------------------
!
!  subroutine exercise4()
!
!  Performs exercise4
!
!-----------------------------------------------------------------------

  subroutine exercise4()
    implicit none

    ! Local
    real(wp), dimension(4) :: ll_lims
    integer :: sx, sy, ii
    logical :: is911event
    character(64) :: fname

    ! (/ minlon, maxlon, minlat, maxlat /)
    ll_lims = (/ -180.0, 180.0, -90.0, 90.0 /);

    sx = 800; sy = 400;
    
!    call meshgrid(sx,sy,ll_lims,lon,lat,dnum,1)
!    call writenetcdf("ex4_allterror_800_400.nc", lons, lats, rho_xy, sx, sy)

!    call meshgrid(sx,sy,ll_lims,lon,lat,dnum,2)
!    call writenetcdf("ex6_amp_800_400.nc", lons, lats, rho_xy, sx, sy)

    do ii = 1,nttypes ! Targets
       write(*,*) ii, 'Mapping casualties for target type: ', trim(tname(ii))
       call meshgrid(sx,sy,ll_lims,lon,lat,dnum,ii+2)
       fname = "";
       write(fname,'(A11,I2,A3)') 'ex8_amplit_', ii,'.nc'
       call writenetcdf(fname,lons,lats,rho_xy,sx,sy)
    end do

  end subroutine exercise4
  
!-----------------------------------------------------------------------
!
!  subroutine meshgrid(sizex,sizey,limits,lonarr,latarr,size,task)
!
!  Creates and populates meshgrid of size sizex by size y.
!
!-----------------------------------------------------------------------

  subroutine meshgrid(sizex,sizey,limits,lonarr,latarr,size,task)
    implicit none

    integer, intent(in) :: sizex, sizey, size, task
    real(wp), intent(in), dimension(size) :: lonarr, latarr
    real(wp), intent(in), dimension(4) :: limits ! (/ minlon, maxlon, minlat, maxlat /)

    ! Local
    integer :: ii, jj, kk, ll
    integer, dimension(dnum,2) :: llid
    real(wp) :: dlon, dlat, minlon, maxlon, minlat, maxlat
    logical :: is911event

    minlon = limits(1); maxlon = limits(2); minlat = limits(3); maxlat = limits(4);

    if(allocated(rho_ll)) deallocate(rho_ll)
    if(allocated(rho_xy)) deallocate(rho_xy)    
    if(allocated(lats)) deallocate(lats)
    if(allocated(lons)) deallocate(lons) 
    if(allocated(invxy)) deallocate(invxy)       

    allocate(rho_ll(sizex,sizey),rho_xy(sizex,sizey))
    allocate(lons(sizex),lats(sizey),invxy(sizey))

    call linspace(minlon,maxlon,sizex,lons)
    call linspace(minlat,maxlat,sizey,lats)

    dlon = 1.0*(maxlon-minlon)/sizex;
    dlat = 1.0*(maxlat-minlat)/sizey;

    rho_ll(:,:) = 0;
    do kk = 1,dnum
       do ii = 1,sizex
          if(contained(lonarr(kk),lons(ii)-dlon*0.5,lons(ii)+dlon*0.5)) then
             llid(kk,1) = ii; exit;
          end if
       end do
       do jj = 1,sizey
          if(contained(latarr(kk),lats(jj)-dlat*0.5,lats(jj)+dlat*0.5)) then
             llid(kk,2) = jj; exit;
          end if
       end do
       if(llid(kk,1).eq.0) llid(kk,1) = 1;
       if(llid(kk,2).eq.0) llid(kk,2) = 1;       
       select case(task)
       case (1) ! Event density
          rho_ll(llid(kk,1),llid(kk,2)) = rho_ll(llid(kk,1),llid(kk,2)) + 1;
       case (2) ! Casualties (dead + wounded)
          is911event = .false.;
          do ll = 1,4
             if(kk.eq.ii911s(ll)) is911event = .true.;
          end do
          if(.not.is911event) then
             rho_ll(llid(kk,1),llid(kk,2)) = rho_ll(llid(kk,1),llid(kk,2)) &
                  + nkill(kk) + nwound(kk);
          end if
       case default ! Target type event/casualty density
          is911event = .false.;
          do ll = 1,4
             if(kk.eq.ii911s(ll)) is911event = .true.;
          end do
          if((.not.is911event).and.((task-2).eq.floor(tartype(kk)))) then
!             rho_ll(llid(kk,1),llid(kk,2)) = rho_ll(llid(kk,1),llid(kk,2)) + 1;             
             rho_ll(llid(kk,1),llid(kk,2)) = rho_ll(llid(kk,1),llid(kk,2)) &
                  + nkill(kk) + nwound(kk);
          end if
       end select
    end do

    do jj = 1,sizey
       ! Scale lon-lat to square meters using WGS84 params
       invxy(jj) = (1-e_geo*e_geo*sin(lat(jj)*pi/180)*sin(lat(jj)*pi/180))**2;
       invxy(jj) = invxy(jj)/(a_geo*a_geo*cos(lat(jj)*pi/180)*(1-e_geo*e_geo));
       invxy(jj) = invxy(jj)*180*180/pi/pi;
!       do ii = 1,sizex
          rho_ll(:,jj) = rho_ll(:,jj)/dlon/dlat;
          rho_xy(:,jj) = rho_ll(:,jj)*invxy(jj)*100; ! per 100 km^2
!          if(rho_ll(ii,jj).eq.0.0) rho_ll(ii,jj) = NAN
!       end do
    end do
    
  end subroutine meshgrid
  
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
!  subroutine writenetcdf(fname,xarr,yarr,dataxy,sizex,sizey)
!
!  Creates and populates meshgrid of size sizex by size y.
!
!-----------------------------------------------------------------------

  subroutine writenetcdf(fname,xarr,yarr,dataxy,sizex,sizey)
    implicit none
    
    character(*), intent(in) :: fname
    integer, intent(in) :: sizex, sizey
    real(wp), dimension(sizex), intent(in) :: xarr
    real(wp), dimension(sizey), intent(in) :: yarr
    real(wp), dimension(sizex,sizey), intent(in) :: dataxy

    ! netCDF stuff
    integer :: ncid, xid, yid, dataid
    integer :: xdimid, ydimid, ddimids(2)

    ! Create netCDF file.
    call check(nf90_create(fname,NF90_CLOBBER,ncid))
    ! Define dimensions
    call check(nf90_def_dim(ncid,"latitude",sizey,ydimid))
    call check(nf90_def_dim(ncid,"longitude",sizex,xdimid))
    ddimids = (/ xdimid, ydimid /)
    ! Define the variables
    call check(nf90_def_var(ncid,"latitude",NF90_REAL,ydimid,yid))
    call check(nf90_def_var(ncid,"longitude",NF90_REAL,xdimid,xid))
    call check(nf90_def_var(ncid,"Density",NF90_REAL,ddimids,dataid))       
    ! End define mode
    call check(nf90_enddef(ncid))
    ! Write data to file
    call check(nf90_put_var(ncid, yid, yarr))
    call check(nf90_put_var(ncid, xid, xarr))
    call check(nf90_put_var(ncid, dataid, dataxy))
    ! Close file
    call check(nf90_close(ncid))
    
    

  end subroutine writenetcdf

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
