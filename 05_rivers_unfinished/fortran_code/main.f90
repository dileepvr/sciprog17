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
  dnum = intarray(1);

  call exercise1();
  
  call allocarrays()

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
    
  end subroutine dealloc

!-----------------------------------------------------------------------
!
!  subroutine exercise1()
!
!  Perform exercise 1
!
!-----------------------------------------------------------------------

  subroutine exercise1()
    implicit none

    ! Local
    integer :: ii, jj, kk, pos, fid, status
    real(wp) :: lat, lon, oldlat, oldlon
    character(128) :: sensorname
    character(1024) :: tempbuf


    ! Open file
    status = 0
    fid = getu()
    open( UNIT=fid, FILE=infilename(1:indlnb(infilename)), STATUS='old', &
         ERR=101, IOSTAT=status )

    jj = 0; kk = 0;
    ! Read first line
    tempbuf = ""; read(fid,'(A)') tempbuf
    pos = index(tempbuf(2:),'"')+1;
    sensorname = tempbuf(1:pos);
    jj = index(tempbuf(pos+2:),trim(data_delim));
    kk = index(tempbuf(pos+jj+3:),trim(data_delim));
    read(tempbuf(pos+2:pos+2+jj+kk),*) lat, lon
    write(*,*) lat, lon

    close(fid)

    return

101 continue
   write(0,*) 'exercise1(): Error while opening config file "', infilename,'", error: ', status
   stop      


  end subroutine exercise1
    
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
