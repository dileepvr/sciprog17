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
!  subroutine printheaders(numheaders)
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
