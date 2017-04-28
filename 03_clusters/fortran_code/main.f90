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

  ! Local
  integer :: sizex, sizey

  call parse_params()

  !call printheaders(infilename, inftype)
  !write(*,*) numcols(",1,2, 3,  ",",")
  call firstpass(infilename,data_delim,inftype,intarray,debug_trace)
  dnum = intarray(1); dcols = intarray(2);
  
  call allocarrays()

  call readalldata(infilename,data_delim,inftype,intarray,debug_trace)
  minlat = lat(FindMinimum(lat,1,dnum));
  maxlat = lat(FindMaximum(lat,1,dnum));
  minlon = lon(FindMinimum(lon,1,dnum));
  maxlon = lon(FindMaximum(lon,1,dnum));

  testreal = maxlon-minlon;
  minlon = minlon - 0.1*testreal; maxlon = maxlon + 0.1*testreal;
  testreal = maxlat-minlat;
  minlat = minlat - 0.1*testreal; maxlat = maxlat + 0.1*testreal;

!  sizex = 200; sizey = 100;
  !  call exercise2(sizex,sizey)

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

    allocate(lat(dnum),lon(dnum))

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

    deallocate(lat,lon)

    deallocate(colnames)

    if(allocated(rho_ll)) deallocate(rho_ll)
    if(allocated(rho_xy)) deallocate(rho_xy)    
    if(allocated(lats)) deallocate(lats)
    if(allocated(lons)) deallocate(lons) 
    if(allocated(invxy)) deallocate(invxy)       
    
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
!  subroutine exercise2(sx,sy)
!
!  Generates heat map.
!
!-----------------------------------------------------------------------

  subroutine exercise2(sx,sy)
    implicit none

    integer, intent(in) :: sx, sy
    real(wp) :: maxll,maxxy

    call meshgrid(sx,sy,maxll,maxxy)
    
    call writenetcdf("ex2_heatmap.nc",lons,lats,rho_xy,sx,sy)
    
  end subroutine exercise2


!-----------------------------------------------------------------------
!
!  subroutine exercise3()
!
!  Does exercise 3.
!
!-----------------------------------------------------------------------

  subroutine exercise3()
    implicit none

    ! Local
    real(wp), parameter :: lat0 = 38.9_wp
    real(wp), parameter :: lat1 = 41.8_wp
    real(wp), parameter :: lon0 = -86.4_wp
    real(wp), parameter :: lon1 = -81.9_wp
    real(wp) :: clon, clat, dlon, dlat, dlon2, dlat2, blen
    integer :: subnum, ii, jj, sizex, sizey, kk, ll
    integer, dimension(5,17) :: hists
    integer,  dimension(500) :: hist2
    real(wp) :: binw
    real(wp), allocatable, dimension(:) :: sublon, sublat
    real(wp) :: maxll, maxxy
    character(64) :: myfmt

    do ii = 1,5
       do jj = 1,17
          hists(ii,jj) = 0;
       end do
    end do

    do ii = 1,500
       hist2(ii) = 0;
    end do

    dlon = lon1-lon0; dlat = lat1-lat0;
    clon = (lon1+lon0)*0.5; clat = (lat1+lat0)*0.5;
    subnum = nbox_ll(lon,lat,clon,clat,dlon,dlat,dnum);

    allocate(sublon(subnum),sublat(subnum))

    jj = 1; dlon2 = 0.5*dlon; dlat2 = 0.5*dlat;
!    write(333,*) 'lon,lat'
    do ii = 1,dnum
       if(contained(lon(ii),clon-dlon2,clon+dlon2).and. &
            contained(lat(ii),clat-dlat2,clat+dlat2)) then
          sublon(jj) = lon(ii); sublat(jj) = lat(ii);
          jj = jj + 1;
          !        write(333,*) lon(ii),',',lat(ii)
       end if
    end do

    minlon = lon0; maxlon = lon1; minlat = lat0; maxlat = lat1;
    ! Need to make irregularly spaced lats and lons for every type of
    ! N km-square grid. Too hard. Use average.
!    if(.false.) then           
       do ii = 0,4
          blen = sqrt(1.0*(2**ii)); ! km
          dlon2 = blen*180.0*(1-(e_geo*sin(clat*pi/180.0))**2)**(1.5);
          dlon2 = dlon2/(pi*a_geo*(1-e_geo*e_geo));
          dlat2 = blen*180.0*sqrt(1-(e_geo*sin(clat*pi/180.0))**2);
          dlat2 = dlat2/(pi*a_geo*cos(clat*pi/180.0));

          sizex = ceiling(dlon/dlon2); sizey = ceiling(dlat/dlat2);
          call meshgrid(sizex,sizey,maxll,maxxy);

          !       write(*,*) blen**2, maxll*dlon2*dlat2
          rho_ll = rho_ll*dlon2*dlat2;


          do jj = 1,17
             binw = jj*0.25;
             do kk = 1,sizex
                do ll = 1,sizey
                   if((rho_ll(kk,ll).ge.(binw-0.25)).and.(rho_ll(kk,ll).lt.binw)) then
                      hists(ii+1,jj) = hists(ii+1,jj) + 1;
                   end if
                end do
             end do
          end do
          
       end do


       myfmt = "(F4.2,A1,F4.2,A1,I6,A1,I6,A1,I6,A1,I6,A1,I6)"
       write(333,*) 'bin,1 sq-km, 2 sq-km, 4 sq-km, 8 sq-km, 16 sq-km'
       do jj = 1,17
          write(333,myfmt) (jj-1)*0.25,'-',jj*0.25,',',hists(1,jj),',',hists(2,jj), &
               ',',hists(3,jj),',',hists(4,jj),',',hists(5,jj)
       end do
!    end if

    if(.false.) then
    ! Make heat map for 8 sq-km boxes
    blen = sqrt(8.0); ! km
    dlon2 = blen*180.0*(1-(e_geo*sin(clat*pi/180.0))**2)**(1.5);
    dlon2 = dlon2/(pi*a_geo*(1-e_geo*e_geo));
    dlat2 = blen*180.0*sqrt(1-(e_geo*sin(clat*pi/180.0))**2);
    dlat2 = dlat2/(pi*a_geo*cos(clat*pi/180.0));
    sizex = ceiling(dlon/dlon2); sizey = ceiling(dlat/dlat2);
!    write(*,*) clon, clat, dlon, dlat
    call meshgrid(sizex,sizey,maxll,maxxy);
    rho_ll = rho_ll*dlon2*dlat2;
    call writenetcdf("ex3_heatmap2.nc",lons,lats,rho_ll,sizex,sizey)
    end if
    if(.false.) then
       blen = 25.0; ! km
       dlon2 = blen*180.0*(1-(e_geo*sin(clat*pi/180.0))**2)**(1.5);
       dlon2 = dlon2/(pi*a_geo*(1-e_geo*e_geo));
       dlat2 = blen*180.0*sqrt(1-(e_geo*sin(clat*pi/180.0))**2);
       dlat2 = dlat2/(pi*a_geo*cos(clat*pi/180.0));

       call meshgridcirc(sizex,sizey,dlon2,dlat2/dlon2)
       !    rho_ll = rho_ll*dlon2*dlat2
       !    call writenetcdf("ex3_heatmap_circ2.nc",lons,lats,rho_ll,sizex,sizey)

    
       call meshgridvoid(sizex,sizey,dlon2/25.0,dlat2/dlon2,maxll)

       !    call writenetcdf("ex3_heatmap_void.nc",lons,lats,rho_ll,sizex,sizey)
       do ii = 1,sizex
          do jj = 1,sizey
             do kk = 1,10
                if(rho_ll(ii,jj).le.kk*5.0) exit
             end do
             hist2(kk) = hist2(kk) + 1
          end do
       end do

       write(333,*) 'bin,dist'
       myfmt = "(F4.1,A1,F4.1,A1,I4)"
       do kk = 1,10
          write(333,myfmt) (kk-1)*5.0,'-',kk*5.0,',',hist2(kk)
       end do
    end if
 
    deallocate(sublon,sublat)    
    
  end subroutine exercise3
  
!-----------------------------------------------------------------------
!
!  subroutine meshgrid(sizex,sizey)
!
!  Creates and populates meshgrid of size sizex by size y.
!
!-----------------------------------------------------------------------

  subroutine meshgrid(sizex,sizey,maxll,maxxy)
    implicit none

    integer, intent(in) :: sizex, sizey
    real(wp), intent(out) :: maxll, maxxy

    ! Local
    integer :: ii, jj, kk, ll
    real(wp) :: dlon, dlat


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

    maxll = 0.0; maxxy = 0.0; 
    do jj = 1,sizey
       ! Scale lon-lat to square meters using WGS84 params
       invxy(jj) = (1-e_geo*e_geo*sin(lat(jj)*pi/180)*sin(lat(jj)*pi/180))**2;
       invxy(jj) = invxy(jj)/(a_geo*a_geo*cos(lat(jj)*pi/180)*(1-e_geo*e_geo));
       invxy(jj) = invxy(jj)*180*180/pi/pi;
       do ii = 1,sizex
          rho_ll(ii,jj) = 1.0*nbox_ll(lon,lat,lons(ii),lats(jj),dlon,dlat,dnum);
          rho_ll(ii,jj) = rho_ll(ii,jj)/dlon/dlat;
          rho_xy(ii,jj) = rho_ll(ii,jj)*invxy(jj)*100; ! per 100 km^2
          if(maxll .lt. rho_ll(ii,jj)) maxll = rho_ll(ii,jj)
          if(maxxy .lt. rho_xy(ii,jj)) maxxy = rho_xy(ii,jj)
!          if(rho_ll(ii,jj).eq.0.0) rho_ll(ii,jj) = NAN
       end do
    end do
    
  end subroutine meshgrid


!-----------------------------------------------------------------------
!
!  subroutine meshgridcirc(sizex,sizey,rad,ybyx)
!
!  Creates and populates meshgrid of size sizex by size y.
!
!-----------------------------------------------------------------------

  subroutine meshgridcirc(sizex,sizey,rad,ybyx)
    implicit none

    integer, intent(in) :: sizex, sizey
    real(wp), intent(in) :: rad, ybyx

    ! Local
    integer :: ii, jj, kk, ll
    real(wp) :: dlon, dlat


    if(allocated(rho_ll)) deallocate(rho_ll)

    if(allocated(lats)) deallocate(lats)
    if(allocated(lons)) deallocate(lons) 


    allocate(rho_ll(sizex,sizey))
    allocate(lons(sizex),lats(sizey))

    call linspace(minlon,maxlon,sizex,lons)
    call linspace(minlat,maxlat,sizey,lats)

    dlon = 1.0*(maxlon-minlon)/sizex;
    dlat = 1.0*(maxlat-minlat)/sizey;

    do jj = 1,sizey
       do ii = 1,sizex
          rho_ll(ii,jj) = 1.0*ncirc_ll(lon,lat,lons(ii),lats(jj),rad,dnum,ybyx);
!          rho_ll(ii,jj) = rho_ll(ii,jj)/dlon/dlat;
!          if(rho_ll(ii,jj).eq.0.0) rho_ll(ii,jj) = NAN
       end do
    end do
    
  end subroutine meshgridcirc


!-----------------------------------------------------------------------
!
!  subroutine meshgridvoid(sizex,sizey,rad,ybyx,maxll)
!
!  Creates and populates meshgrid of size sizex by size y.
!
!-----------------------------------------------------------------------

  subroutine meshgridvoid(sizex,sizey,rad,ybyx,maxll)
    implicit none

    integer, intent(in) :: sizex, sizey
    real(wp), intent(in) :: rad,ybyx
    real(wp), intent(out) :: maxll

    ! Local
    integer :: ii, jj, kk, ll
    real(wp) :: dlon, dlat, minll


    if(allocated(rho_ll)) deallocate(rho_ll)

    if(allocated(lats)) deallocate(lats)
    if(allocated(lons)) deallocate(lons) 


    allocate(rho_ll(sizex,sizey))
    allocate(lons(sizex),lats(sizey))

    call linspace(minlon,maxlon,sizex,lons)
    call linspace(minlat,maxlat,sizey,lats)

    dlon = 1.0*(maxlon-minlon)/sizex;
    dlat = 1.0*(maxlat-minlat)/sizey;

    maxll = 0.0; 
    do jj = 1,sizey
       do ii = 1,sizex
          minll = dlon*sizex;
          do kk = 1,dnum
             if(minll.gt.vecdist(lon(kk),lat(kk),lons(ii),lats(jj),1.0_wp,ybyx)) then
                minll = vecdist(lon(kk),lat(kk),lons(ii),lats(jj),1.0_wp,ybyx);
             end if
!             if(ncirc_ll(lon,lat,lons(ii),lats(jj),kk*rad,dnum,ybyx).gt.0) exit
          end do
          rho_ll(ii,jj) = minll/rad;
!          if(maxll.lt.(kk-1)) maxll = kk-1;
!          rho_ll(ii,jj) = rho_ll(ii,jj)/dlon/dlat;
!          if(rho_ll(ii,jj).eq.0.0) rho_ll(ii,jj) = NAN
       end do
    end do
    
  end subroutine meshgridvoid
  
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
