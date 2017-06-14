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
    
!-----------------------------------------------------------------------
!
!  subroutine readalldata(fname,delim,numheaders,intarr,print_trace)
!
!  Reads data file into arrays.
!  Uses global variables.
!
!-----------------------------------------------------------------------

  subroutine readalldata(fname,delim,numheaders,intarr,print_trace)

      implicit none

    character(64), intent(in) :: fname
    character(*), intent(in) :: delim
    integer, intent(in) :: numheaders
    integer, dimension(:), intent(out) :: intarr
    logical, intent(in) :: print_trace

    ! Local variables
    integer :: ii, jj, fid, status, numrows, maxcols, headercols, pos
    integer :: kk, oldpos, oldpos2
    integer :: numrowswithmaxcols
    integer, dimension(dnum) :: ucountryid
    character(64), dimension(dnum) :: ugrname, ucid, uaid, utid, uwid
    logical :: oldcountry, oldgroup
    character(1024) :: tempbuf
    character(64) :: tbuf2

    numrows = intarr(1); maxcols = intarr(2); headercols = intarr(3);

    ! Open file
    status = 0
    fid = getu()
    open( UNIT=fid, FILE=fname(1:indlnb(fname)), STATUS='old', &
         ERR=97, IOSTAT=status )

    if(numheaders .gt. 0) then
       ! Read column names from header
       do ii = 1,numheaders
          tempbuf = ""
          read(fid,'(A)') tempbuf
          if(headercols .eq. numcols(trim(tempbuf),trim(delim))) then
             jj = 1; pos = 1; 
             do while ( index(tempbuf(pos:),trim(delim)) .ne. 0 )
                oldpos = pos
                pos = pos + index(tempbuf(pos:),trim(delim))
                colnames(jj) = ""
                colnames(jj) = tempbuf(oldpos:pos-2)
                jj = jj + 1
             end do

             colnames(jj) = ""
             colnames(jj) = tempbuf(pos:)
             if(trim(tempbuf(pos:)) .eq. "") colnames(jj) = "unnamed"
             exit
          end if
       end do
    end if
    
    ! Read all the data
    natypes = 0; nttypes = 0; nwtypes = 0; ncountries = 0;
    ngroups = 0; kk = 1;
    do ii = 1, dnum
!    do ii = 1, 6
       tempbuf = ""
       read(fid,'(A)') tempbuf
       ! Read year, month, day, denum, and country
       pos = index(tempbuf,trim(delim))
       read(tempbuf(1:pos-1),'(I4,I2,I2,I4)') year(ii), month(ii), &
            day(ii), dayenum(ii)
       read(tempbuf(1:pos-1),*) eventid(ii)
       pos = pos + index(tempbuf(pos+1:),trim(delim))
       pos = pos + index(tempbuf(pos+1:),trim(delim))
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       read(tempbuf(oldpos+1:pos-1),*) country(ii)
       if(ii.eq.1) then
          ncountries = 1; ucountryid(ii) = country(ii);
          oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
          ucid(ii) = tempbuf(oldpos+1:pos-1);          
       else
          oldcountry = .false.;
          do jj = 1,ncountries
             if(ucountryid(jj).eq.country(ii)) then
                oldcountry = .true.; exit;
             end if
          end do
          if(.not.oldcountry) then
             ncountries = ncountries+1;
             ucountryid(ncountries) = country(ii);
             oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
             ucid(ncountries) = tempbuf(oldpos+1:pos-1);
          else
             pos = pos + index(tempbuf(pos+1:),trim(delim))
          end if
       end if
       ! Read latitude and longitude
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       if(pos-oldpos.gt.1) then
          read(tempbuf(oldpos+1:pos-1),*) lat(ii);
       else
          lat(ii) = -1000.0;
       end if
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       if(pos-oldpos.gt.1) then
          read(tempbuf(oldpos+1:pos-1),*) lon(ii);
       else
          lon(ii) = -1000.0;
       end if       
       ! Read attack type, target type, group name, weapon type, nkill, and nwounded
       pos = pos + index(tempbuf(pos+1:),trim(delim))
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       read(tempbuf(oldpos+1:pos-1),*) atype(ii)
       if(natypes.lt.atype(ii)) natypes = atype(ii)
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       uaid(floor(atype(ii))) = tempbuf(oldpos+1:pos-1);
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       read(tempbuf(oldpos+1:pos-1),*) tartype(ii)
       if(nttypes.lt.tartype(ii)) nttypes = tartype(ii)       
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       utid(floor(tartype(ii))) = tempbuf(oldpos+1:pos-1);
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       tbuf2 = tempbuf(oldpos+1:pos-1)
       if(ii.eq.1) then
          ngroups = 1; ugrname(ii) = tbuf2; gid(ii) = 1;
       else
          oldgroup = .false.;
          do jj = 1,ngroups
             if(ugrname(jj).eq.tbuf2) then
                oldgroup = .true.; exit;
             end if
          end do
          if(.not.oldgroup) then
             ngroups = ngroups+1;
             ugrname(ngroups) = tbuf2; gid(ii) = ngroups;
          end if
       end if
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       read(tempbuf(oldpos+1:pos-1),*) wtype(ii)
       if(nwtypes.lt.wtype(ii)) nwtypes = wtype(ii)       
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       uwid(floor(wtype(ii))) = tempbuf(oldpos+1:pos-1);
       oldpos = pos; pos = pos + index(tempbuf(pos+1:),trim(delim))
       if(pos-oldpos.gt.1) then
          read(tempbuf(oldpos+1:pos-1),*) nkill(ii);
       else
          nkill(ii) = -1;
       end if
       if(trim(tempbuf(pos+1:)) .eq. "") then
          nwound(ii) = -1;
       else
          read(tempbuf(pos+1:),*) nwound(ii)
       end if
       ! Detect 9/11 event
       if((day(ii).eq.11).and.(month(ii).eq.9).and.(year(ii).eq.2001) &
            .and.(country(ii).eq.217)) then
          ii911s(kk) = ii; kk = kk + 1;
       end if
    end do

!    write(*,*) natypes, nttypes, nwtypes, ncountries, ngroups

    ! Allocate remaining global variables
    allocate(gname(ngroups),aname(natypes),tname(nttypes))
    allocate(wname(nwtypes),cname(ncountries))

    cname(1:ncountries) = ucid(1:ncountries);
    aname(1:natypes) = uaid(1:natypes);    
    gname(1:ngroups) = ugrname(1:ngroups);
    tname(1:nttypes) = utid(1:nttypes);
    wname(1:nwtypes) = uwid(1:nwtypes);    

    if(print_trace) then
!    if(.false.) then
       write(*,*) 'Column names are:'
       do ii = 1,headercols
          write(*,*) ii, ':', trim(colnames(ii))
       end do
    end if

    close(fid)

    return

97 continue
   write(0,*)  'readalldata(): Error while opening config file "', fname,'", error: ', status
   stop      

  end subroutine readalldata


end module fileio
