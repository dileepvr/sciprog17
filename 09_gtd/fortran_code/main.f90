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
