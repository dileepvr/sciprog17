!-----------------------------------------------------------------------
!
!  module fileio
!
!  Contains file - read/write routines 
!
!-----------------------------------------------------------------------

module fileio

    use globals
    use utilities

  contains

!-----------------------------------------------------------------------
!
!  subroutine firstpass(fname,delim,numheaders,intarr,print_trace)
!
!  Runs through data file and computes general data statistics.
!  intarr(:) = { numrows, maxcols, headercols, numrowswithmaxcols }    
!
!-----------------------------------------------------------------------

  subroutine firstpass(fname,delim,numheaders,intarr,print_trace)

      implicit none

    character(64), intent(in) :: fname
    character(*), intent(in) :: delim
    integer, intent(in) :: numheaders
    integer, dimension(:), intent(out) :: intarr
    logical, intent(in) :: print_trace

    ! Local variables
    integer :: ii, fid, status, numrows, maxcols, headercols
    integer :: numrowswithmaxcols
    character(1024) :: tempbuf

    ! Open file
    status = 0
    fid = getu()
    open( UNIT=fid, FILE=fname(1:indlnb(fname)), STATUS='old', &
         ERR=95, IOSTAT=status )

    ! Count maximum number of columns in headers
    headercols = 0
    do ii = 1,numheaders
       tempbuf = ""
       read(fid,'(A)') tempbuf
       headercols = maxofint(headercols,numcols(trim(tempbuf),trim(delim)))
    end do

    ! Count the number of data rows and maximum number of columns
    status = 0
    numrows = 0; maxcols = 0
    do
       tempbuf = ""
       read(fid,'(A)',IOSTAT=status) tempbuf
       if(status .ne. 0) exit
       numrows = numrows + 1
       maxcols = maxofint(maxcols,numcols(tempbuf,trim(delim)))
    end do

    rewind(fid)

    ! Count number of data rows with maxcols columns
    do ii = 1,numheaders
       tempbuf = ""
       read(fid,'(A)') tempbuf
    end do
    numrowswithmaxcols = 0

    do
       tempbuf = ""
       read(fid,'(A)',IOSTAT=status) tempbuf
       if(status .ne. 0) exit
       if(numcols(tempbuf,trim(delim)) .eq. maxcols) then
          numrowswithmaxcols = numrowswithmaxcols + 1
       end if
    end do

    if(print_trace) then
       write(*,*) 'Number of data rows : ', numrows
       write(*,*) 'Number of columns : ', maxcols
       write(*,*) 'Number of header columns : ', headercols
       write(*,*) 'Number of data rows with all columns : ', numrowswithmaxcols       
    end if

    intarr(1) = numrows; intarr(2) = maxcols; intarr(3) = headercols;
    intarr(4) = numrowswithmaxcols;
    
    close(fid)

    return

95 continue
   write(0,*)  'firstpass(): Error while opening config file "', fname,'", error: ', status
   stop      

  end subroutine firstpass

    
!-----------------------------------------------------------------------
!
!  subroutine printheaders(fname,numheaders)
!
!  Reads and prints opening lines (data file headers)
!
!-----------------------------------------------------------------------

  subroutine printheaders(fname,numheaders)

    character(64), intent(in) :: fname
    integer, intent(in) :: numheaders

    ! Local variables
    integer :: ii, fid, status
    character(1024) :: tempbuf

    ! Open file
    status = 0
    fid = getu()
    open( UNIT=fid, FILE=fname(1:indlnb(fname)), STATUS='old', &
         ERR=96, IOSTAT=status )

    ! read and print headers
    do ii = 1,numheaders
       read(fid,'(A)') tempbuf
       write(*,*) trim(tempbuf)
    end do

    close(fid)

    return

96 continue
   write(0,*)  'printheaders(): Error while opening config file "', fname,'", error: ', status
   stop      
    
  end subroutine printheaders
    


end module fileio
