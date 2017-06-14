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

  implicit none

  call parse_params()

  !call printheaders(infilename, inftype)
  call firstpass(infilename,data_delim,inftype,intarray,debug_trace)
  dnum = intarray(1); dcols = intarray(2); hcols = intarray(3);
  
  call allocarrays()

  call readalldata(infilename,data_delim,inftype,intarray,debug_trace)

  !write(*,*) year(1), year(dnum)
  call listuniqueid()
  call listuniquecat()
!  call exercise2()
!  call exercise4()
  call exercise5()
  
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

    allocate(id(dnum),wspeed(dnum),press(dnum),year(dnum),dirt(dnum))
    allocate(lat(dnum),lon(dnum),hour(dnum),scatint(dnum),uid(dnum),idnum(dnum))
    allocate(colnames(hcols),sname(dnum),scat(dnum),uscat(dnum),usname(dnum))

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

    deallocate(id,wspeed,press,year,dirt)
    deallocate(lat,lon,hour,scatint,uid,idnum)
    deallocate(colnames,sname,scat,uscat,usname)
    
  end subroutine dealloc

  
!-----------------------------------------------------------------------
!
!  subroutine listuniqueid()
!
!  Determines and sorts unique storm IDs
!
!-----------------------------------------------------------------------

  subroutine listuniqueid()
    implicit none

    !Local
    integer :: ii, jj
    logical :: old
    
    uid(1) = id(1); nid = 1; idnum(1) = 1;
    usname(1) = sname(1);
    do ii = 2,dnum
       old = .false.
       do jj = 1,nid
          if(id(ii) .eq. uid(jj)) then
             old = .true.; idnum(ii) = jj; exit;
          end if
       end do
       if(.not.old) then
          nid = nid + 1; uid(nid) = id(ii); usname(ii) = sname(ii);
          idnum(ii) = nid;
       end if
    end do

    if(debug_trace)  write(*,*) 'Number of unique storms :', nid

  end subroutine listuniqueid


!-----------------------------------------------------------------------
!
!  subroutine listuniquecat()
!
!  Determines and sorts unique storm IDs
!
!-----------------------------------------------------------------------

  subroutine listuniquecat()
    implicit none

    ! Local
    integer :: ii, jj
    logical :: old
    
    uscat(1) = scat(1); nscat = 1; scatint(1) = 1;
    do ii = 2,dnum
       old = .false.
       do jj = 1,nscat
          if(trim(scat(ii)) .eq. trim(uscat(jj))) then
             old = .true.; scatint(ii) = jj; exit;
          end if
       end do
       if(.not.old) then
          nscat = nscat + 1; uscat(nscat) = scat(ii);
          scatint(ii) = nscat;
       end if
    end do

!    if(debug_trace)  then
    if(.false.) then
       write(*,*) 'Number of unique storm categories :', nscat
       do ii = 1, nscat
          !          if(uscat(ii)(1:9) .eq. 'HURRICANE') write(*,*) trim(uscat(ii))
          write(*,*) trim(uscat(ii))          
       end do
    end if

  end subroutine listuniquecat

!-----------------------------------------------------------------------
!
!  subroutine exercise2()
!
!  Do exercise 2 of assignment.
!
!-----------------------------------------------------------------------

  subroutine exercise2()
    implicit none

    !Local
    integer :: ii, jj, kk, bin, fid, status
    integer, dimension(11) :: numhurr
    logical :: ishurricane
    character(64) :: format_str, fname

    do ii = 1,11
       numhurr(ii) = 0;
    end do
    
    do jj = 1,nid ! For every unique storm id
       ishurricane = .false.

       do kk = 1,dnum ! Go through data to see if it ever was a hurricane
          if(idnum(kk) .eq. jj) then
             if(scat(kk)(1:9) .eq. 'HURRICANE') then
                ishurricane = .true.;
                exit;
             end if
          end if
       end do
       
       bin = 1; ii = 1900;
       do while (ii .lt. 2009)
          if ((year(kk) .ge. ii) .and. (year(kk) .lt. (ii+10))) then
             numhurr(bin) = numhurr(bin) + 1;
          end if
          ii = ii + 10; bin = bin + 1;
       end do
    end do
    
    format_str = "(I4,A1,I4,A1,I2)"

    if(debug_trace) then
       write(*,*) 'decade,No. of Hurricanes'
       do ii = 1,11
          write(*,format_str) 1900+(ii-1)*10,'-',1909+(ii-1)*10,',',numhurr(ii)
       end do
    else
       fname = "../plots/ex2_01.csv"
       status = 0
       fid = getu()
       open( UNIT=fid, FILE=fname(1:indlnb(fname)), STATUS='replace', &
            ERR = 902, IOSTAT=status )
       write(fid,*) 'decade,No. of Hurricanes'
       do ii = 1,11
          write(fid,format_str) 1900+(ii-1)*10,'-',1909+(ii-1)*10,',',numhurr(ii)
       end do
       close(fid)
    end if

    return

902 continue
    write(0,*)  'exercise2(): Error while opening config file "', fname,'", error: ', status
    stop      

  end subroutine exercise2
  
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
    integer :: ii, jj, kk, bin, fid, status
    integer :: minpress, maxpress, numzeropress
    integer :: nstorm3sigma, nhur3sigma
    real(wp) :: meanstorm, stdstorm, meanhur, stdhur
    integer, dimension(21) :: numstorms, numhurr
    integer, dimension(nid) :: idminpress, hurminpress
    logical :: ishurricane
    character(64) :: format_str, fname

    minpress = 1000; maxpress = 0; numzeropress = 0;
    do ii = 1,dnum
       if(press(ii) .gt. 0) then
          minpress = minofint(minpress,press(ii))
       else
          numzeropress = numzeropress + 1
       end if
       maxpress = maxofint(maxpress,press(ii))
    end do

    ! Calculate minimum non-zero pressure of each storm
    do jj = 1,nid
       idminpress(jj) = 2000 ! Really high
       hurminpress(jj) = 2000 ! Really high       
       do ii = 1,dnum
          if(idnum(ii) .eq. jj) then
             if(press(ii) .gt. 0) then
                idminpress(jj) = minofint(idminpress(jj),press(ii))
                if(scat(jj)(1:9) .eq. 'HURRICANE') then
                   hurminpress(jj) = minofint(hurminpress(jj),press(ii))
                end if
             end if
          end if
       end do
    end do

    ! Calculate statistics
    ! Can't use SUM and NORM2 functions because of dirty data
    meanstorm = 0.0; stdstorm = 0.0; meanhur = 0.0; stdhur = 0.0;
    jj = 0; kk = 0;
    do ii = 1,nid
       if(idminpress(ii) .lt. 2000) then
          meanstorm = meanstorm + 1.0*idminpress(ii);
          stdstorm = stdstorm + 1.0*idminpress(ii)*idminpress(ii);
          jj = jj + 1; ! Keep count of good data
       end if
       if(hurminpress(ii) .lt. 2000) then
          meanhur = meanhur + 1.0*hurminpress(ii);
          stdhur = stdhur + 1.0*hurminpress(ii)*hurminpress(ii);
          kk = kk + 1; ! Keep count of good data
       end if
    end do
    meanstorm = meanstorm*(1.0/jj);
    meanhur = meanhur*(1.0/kk);
    stdstorm = stdstorm*(1.0/jj) - meanstorm*meanstorm;
    stdhur = stdhur*(1.0/kk) - meanhur*meanhur;
    stdstorm = stdstorm**(0.5);
    stdhur = stdhur**(0.5);
    write(*,*) 'nstorms: ', jj, 'nhur: ', kk

    ! Compute number of storms/hurricanes 3 sigma below means
    nstorm3sigma = 0; nhur3sigma = 0;
    do ii = 1,nid
       if(1.0*idminpress(ii) .lt. meanstorm-3*stdstorm) then ! Note integer to real
          nstorm3sigma = nstorm3sigma + 1; 
       end if
       if(1.0*hurminpress(ii) .lt. meanhur-3*stdhur) then ! Note integer to real
          nhur3sigma = nhur3sigma + 1;
       end if
    end do

    ! Build histograms for storms and hurricanes
    do ii = 1,21
       numstorms(ii) = 0; numhurr(ii) = 0;
       do jj = 1,nid
          if((idminpress(jj) .ge. 880+(ii-1)*7) .and. (idminpress(jj) .lt. 880+ii*7)) then
             numstorms(ii) = numstorms(ii) + 1
          end if
          if((hurminpress(jj) .ge. 880+(ii-1)*7) .and. (hurminpress(jj) .lt. 880+ii*7)) then
             numhurr(ii) = numhurr(ii) + 1
          end if          
       end do
    end do
    
!    format_str = "(I4,A1,I4,A1,I2)"
    
    if(debug_trace) then
       write(*,*) 'm_storm: ', meanstorm, ', std_storm: ', stdstorm
       write(*,*) 'm_hur: ', meanhur, ', std_hur: ', stdhur
       write(*,*) 'n_storm_3sig: ', nstorm3sigma, 'n_hur_3sig: ', nhur3sigma
!       write(*,*) 'min pressure: ', minpress, ', max pressure: ', maxpress
!       write(*,*) 'numzeropress: ', numzeropress
!       write(122,*) 'Pressure (mb),No. of Storms, No. of Hurricanes'
!       do ii = 1,21
!          write(122,*) 880+(ii-1)*7,'-',880+(ii)*7,',',numstorms(ii),',',numhurr(ii)
!       end do
    else
       fname = "../plots/ex4_01.csv"
       status = 0
       fid = getu()
       open( UNIT=fid, FILE=fname(1:indlnb(fname)), STATUS='replace', &
            ERR=904, IOSTAT=status )
       write(fid,*) 'Pressure (mb),No. of Storms (all types), No. of Hurricanes'
       do ii = 1,21
          write(fid,*) 880+(ii-1)*7,'-',880+(ii)*7,',',numstorms(ii),',',numhurr(ii)
       end do
       close(fid)
    end if

    return

904 continue
    write(0,*)  'exercise4(): Error while opening config file "', fname,'", error: ', status
    stop      

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

    !Local
    integer :: yy, ii, jj, kk, bin, fid, status, tothur
    integer, dimension(5,11) :: numhur
    integer, dimension(nid) :: maxcat
    character(64) :: fname, format_str

    ! Calculate maximum hurricane category of each id
    do jj = 1,nid
       maxcat(jj) = 0
       do ii = 1,dnum
          if(idnum(ii) .eq. jj) then
             if(scat(ii)(1:9) .eq. 'HURRICANE') then
                read(scat(ii)(11:),'(I1)') kk
                if (kk .gt. maxcat(jj)) maxcat(jj) = kk
             end if
          end if
       end do
    end do

    ! Calculate decadel histograms
    yy = 1900; bin = 1;
    do while (yy .lt. 2009)
       do kk = 1,5
          numhur(kk,bin) = 0
       end do
       do jj = 1,nid
          if (maxcat(jj) .gt. 0) then
             do ii = 1,dnum
                if((idnum(ii).eq.jj) .and. (year(ii) .ge. yy) .and. (year(ii) .lt. (yy+10))) then
                   numhur(maxcat(jj),bin) = numhur(maxcat(jj),bin) + 1
                   exit
                end if
             end do
          end if
       end do
       yy = yy + 10; bin = bin + 1;
    end do
    

    
    format_str = "(A4,I1,A1,I2)"
    
    if(debug_trace) then
!    if(.false.) then
       write(*,*) 'Number of Hurricanes'
       do ii = 1,5
!          write(*,*) 'Cat-',ii,numhur(ii,1),',',numhur(ii,2),',',numhur(ii,3),',', &
!               numhur(ii,4),',',numhur(ii,5),',',numhur(ii,6),',',numhur(ii,7),',', &
          !               numhur(ii,8),',',numhur(ii,9),',',numhur(ii,10),',',numhur(ii,11)
          write(*,*) sum(numhur(ii,:))
       end do
    else
       do kk = 1,9
          fname = ""
          write(fname,'(A14,I1,A4)') '../plots/ex5_0',kk,'.csv'
          status = 0
          fid = getu()
          open( UNIT=fid, FILE=fname(1:indlnb(fname)), STATUS='replace', &
               ERR=905, IOSTAT=status )
          write(fid,'(A19,I4,A1,I4)') 'Hurricane Category,',1900+(kk-1)*10,'-',1900+kk*10-1
          do ii = 1,5
             write(fid,format_str) 'Cat-', ii, ',',numhur(ii,kk) 
          end do
          close(fid)
       end do
       do kk = 10,11
          fname = ""
          write(fname,'(A13,I2,A4)') '../plots/ex5_',kk,'.csv'
          status = 0
          fid = getu()
          open( UNIT=fid, FILE=fname(1:indlnb(fname)), STATUS='replace', &
               ERR=905, IOSTAT=status )
          write(fid,'(A19,I4,A1,I4)') 'Hurricane Category,',1900+(kk-1)*10,'-',1900+kk*10-1
          do ii = 1,5
             write(fid,format_str) 'Cat-', ii, ',',numhur(ii,kk) 
          end do
          close(fid)
       end do
    end if

    return

905 continue
    write(0,*)  'exercise5(): Error while opening config file "', fname,'", error: ', status
    stop      

  end subroutine exercise5
    
  
!-----------------------------------------------------------------------
!
!  subroutine timetrial()
!
!  Measures time taken for any code sandwiched between calls.
!
!-----------------------------------------------------------------------

subroutine timetrial()
  implicit none

  call print_time('s')

  call print_time('s')
  
end subroutine timetrial


end program main
