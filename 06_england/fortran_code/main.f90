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
  integer :: ii, jj, kk, ystart, yend, ystartbest, yendbest
  real(wp) :: baseline, bestline, Tavg, Tavgold

  call parse_params()

  !call printheaders(infilename, inftype)
  !write(*,*) numcols(",1,2, 3,  ",",")
  call firstpass(infilename,data_delim,inftype,intarray,debug_trace)
  dnum = intarray(1)-1; ncols = intarray(2);
  boxw2 = 2; gauw2 = 10; expalph = 0.2; gausig = 7.0;
  
  call allocarrays()

  call readalldata(infilename,data_delim,inftype,intarray,debug_trace)

  if(.false.) then
     do ii = 1,dnum
        do jj = 1,12
           if(Temp(13,ii).lt.Temp(jj,ii)) Temp(13,ii) = Temp(jj,ii);
        end do
     end do
  end if

  ystart = 84; yend = 113; ! (1961-1990)
  baseline = 0.0;
  do ii = ystart,yend
     baseline = baseline + Temp(13,ii);
  end do
  baseline = baseline/(yend-ystart+1.0);
  do ii = 1,dnum
     baseT(12,ii) = Temp(13,ii)-baseline;
  end do

  ystart = 3; yend = 103; ! (1880-1980)
  baseline = 0.0;
  do ii = ystart,yend
     baseline = baseline + Temp(13,ii);
  end do
  baseline = baseline/(yend-ystart+1.0);
  do ii = 1,dnum
     baseT(13,ii) = Temp(13,ii)-baseline;
  end do

  call box_smooth(Temp(13,:),boxT(13,:),dnum,boxw2)
  call gaus_smooth(Temp(13,:),gausT(13,:),dnum,gauw2,gausig)
  call exp_smooth(Temp(13,:),expT(13,:),dnum,expalph)

  if(.false.) then
     ystart = 84; yend = 113; ! (1961-1990)
     baseline = 0.0;
     do ii = ystart,yend
        baseline = baseline + Temp(13,ii);
     end do
     baseline = baseline/(yend-ystart+1.0);
     do ii = 1,dnum
        baseT(12,ii) = Temp(13,ii)-baseline;
        boxT(12,ii) = boxT(13,ii) - baseline;
        gausT(12,ii) = gausT(13,ii) - baseline;
        expT(12,ii) = expT(13,ii) - baseline;     
     end do

     ystart = 3; yend = 103; ! (1880-1980)
     baseline = 0.0;
     do ii = ystart,yend
        baseline = baseline + Temp(13,ii);
     end do
     baseline = baseline/(yend-ystart+1.0);
     do ii = 1,dnum
        baseT(13,ii) = Temp(13,ii)-baseline;
        boxT(13,ii) = boxT(13,ii) - baseline;
        gausT(13,ii) = gausT(13,ii) - baseline;
        expT(13,ii) = expT(13,ii) - baseline;     
     end do
  end if

  Tavg = 0.0; Tavgold = 0.0;
  do ystart = 1,dnum-29
     do yend = ystart+29,dnum
        baseline = 0.0;
        do ii = ystart,yend
           baseline = baseline + Temp(13,ii);
        end do
        baseline = baseline/(yend-ystart+1.0);
        do ii = 1,dnum
           baseT(12,ii) = Temp(13,ii)-baseline;
        end do
        Tavg = 0.0;
        do ii=130,139
           Tavg = Tavg + baseT(12,ii);
        end do
        if(Tavg.gt.Tavgold) then
           Tavgold = Tavg;
           ystartbest = ystart; yendbest = yend; bestline = baseline;
        end if
     end do
  end do

  do ii = 1,dnum
     baseT(13,ii) = Temp(13,ii)-bestline;
     boxT(13,ii) = boxT(13,ii) - bestline;
     gausT(13,ii) = gausT(13,ii) - bestline;
     expT(13,ii) = expT(13,ii) - bestline;     
  end do
  
  
  if(.false.) then
     write(122,*) 'year,data,boxcar(wid=5yr.),gaussian(sig=7yr.),exp(alph=0.2)'
     do ii = 1,dnum
        if((ii.ge.(gauw2+1)).and.(ii.le.(dnum-gauw2))) then
           write(122,*) year(ii), ',',Temp(13,ii), ',', boxT(13,ii),',',gausT(13,ii), &
                ',',expT(13,ii)
        else if((ii.ge.(boxw2+1)).and.(ii.le.(dnum-boxw2))) then
           write(122,*) year(ii), ',',Temp(13,ii), ',', boxT(13,ii),',,',expT(13,ii)
        else
           write(122,*) year(ii), ',',Temp(13,ii), ',,,',expT(13,ii)           
        end if
     end do
  end if

  write(*,*) 'Baseline: ',ystartbest+1877,'-',yendbest+1877
  jj = 122; kk = 13;
  write(jj,*) 'year,data,boxcar(wid=5yr.),gaussian(sig=7yr.),exp(alph=0.2)'
  do ii = 1,dnum
     if((ii.ge.(gauw2+1)).and.(ii.le.(dnum-gauw2))) then
        write(jj,*) year(ii), ',',baseT(kk,ii), ',', boxT(kk,ii),',',gausT(kk,ii), &
             ',',expT(kk,ii)
     else if((ii.ge.(boxw2+1)).and.(ii.le.(dnum-boxw2))) then
        write(jj,*) year(ii), ',',baseT(kk,ii), ',', boxT(kk,ii),',,',expT(kk,ii)
     else
        write(jj,*) year(ii), ',',baseT(kk,ii), ',,,',expT(kk,ii)           
     end if
  end do
  
  
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

    allocate(year(dnum),colnames(ncols),Temp(ncols-1,dnum),boxT(ncols-1,dnum))
    allocate(gausT(ncols-1,dnum),expT(ncols-1,dnum),baseT(ncols-1,dnum))

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

    deallocate(year,colnames,Temp,boxT)
    deallocate(gausT,expT,baseT)
    
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
