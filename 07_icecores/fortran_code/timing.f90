!-----------------------------------------------------------------------
!
!  module timing
!
!  $Id: timing.f90,v 2.2 2003/08/05 03:13:02 dsteck Exp $
!
!  Contains several routines for keeping track of elapsed time
!  of program execution:
!
!    subroutine initialize_timer() : call at beginning of program;
!                    initializes timers and logs time to std error
!
!    subroutine print_time(unit) : prints time elapsed to std error
!
!    subroutine initialize_checkpoint_timer() : call at beginning of
!                     program and after every checkpoint
!
!    logical function should_checkpoint(interval) : if 'interval'
!                     seconds have elapsed, this function returns
!                     .true.
!
!    character(64) function date_str() : returns Unix-style date string
!  
!  Implementation is via F90 intrinsics.  The checkpoint-related
!  functions are useful for triggering a checkpoint after some
!  physical time interval, especially helpful on supercomputers
!  with time limits.
!
!-----------------------------------------------------------------------

module timing

  integer, save :: timer_init, checkpoint_init

contains


subroutine initialize_module_timing()
  character(127), parameter :: &
    RCS_ID = "@(#)$Id: timing.f90,v 2.2 2003/08/05 03:13:02 dsteck Exp $"
  write(0,*) trim(RCS_ID)
end subroutine initialize_module_timing


!-----------------------------------------------------------------------
!
!  subroutine initialize_timer()
!
!  Call at the beginning of program execution to initilize the 
!  time register.  Also prints a message containing the current
!  date/time to standard error.
!
!-----------------------------------------------------------------------  

subroutine initialize_timer()
  implicit none
  character(64) :: local_date_str
  call system_clock(count=timer_init)
  local_date_str = date_str()
  write(0,*) 'Initialized timer: ', trim(local_date_str)
end subroutine initialize_timer


!-----------------------------------------------------------------------
!
!  subroutine print_time(unit)
!
!  Writes elapsed time of program since the call to initialize_timer
!  to standard error.
!  The input 'unit' is a character string of length 1 denoting the
!  unit of time to be used (s, m, h).
!
!-----------------------------------------------------------------------

subroutine print_time(unit)

  implicit none

  character(1), intent(in) :: unit
  integer :: ntime, time_unit
  real :: rtime
 
  call system_clock(count=ntime, count_rate=time_unit)
  rtime = real(ntime - timer_init) / real(time_unit)

  if ( unit .eq. 's' ) then
    write(0,*) 'Elapsed time: ', rtime, ' seconds'
  else if ( unit .eq. 'm' ) then
    write(0,*) 'Elapsed time: ', rtime/60., ' minutes'
  else if ( unit .eq. 'h' ) then
    write(0,*) 'Elapsed time: ', rtime/3600., ' hours'
  else
    write(0,*) 'Warning (PRINT_TIME): invalid unit specifier'
  end if

  write(0,*)
 
end subroutine print_time


!-----------------------------------------------------------------------
!
!  subroutine initialize_checkpoint_timer()
!
!  Call at the beginning of program execution and after each 
!  scheduled checkpoint to initilize the checkpoint
!  time register.  Also prints a message containing the current
!  date/time to standard error.
!
!-----------------------------------------------------------------------

subroutine initialize_checkpoint_timer()
  implicit none
  character(64) :: local_date_str
  call system_clock(count=checkpoint_init)
  local_date_str = date_str()
  write(0,*) 'Initialized checkpoint timer: ', trim(local_date_str)
end subroutine initialize_checkpoint_timer


!-----------------------------------------------------------------------
!
!  logical function should_checkpoint(interval)
!
!  Measures the time since the last call to 
!  initialize_checkpoint_timer, and returns true if this is greater
!  than the integer 'interval', which is the desired checkpoint 
!  interval in seconds.
!
!-----------------------------------------------------------------------

function should_checkpoint(interval)

  implicit none

  integer, intent(in) :: interval
  logical :: should_checkpoint

  integer :: ntime, time_unit

  call system_clock(count=ntime, count_rate=time_unit)
  ntime = (ntime - checkpoint_init) / time_unit

  if ( ntime .ge. interval ) then
    should_checkpoint = .true.
  else
    should_checkpoint = .false.
  end if

end function should_checkpoint


!-----------------------------------------------------------------------
!
!  character(64) function date_str()
!
!  Returns the current date and time as a Unix-style string.
!
!-----------------------------------------------------------------------

function date_str()

  implicit none

  character(64) :: date_str

  integer :: sec, minut, hour, day, month, year
  integer, dimension(8) :: time_arr
  character(64) :: fmt_str
  character(5) :: zone_str
  character(3), dimension(12) :: monthnames =    &
    (/ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
  character(3), dimension(7) :: daynames =       &
    (/ 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat' /)
 

  ! Get date and time info from Fortran 90 intrinsic
  call date_and_time(zone=zone_str, values=time_arr)
  year  = time_arr(1)
  month = time_arr(2)
  day   = time_arr(3)
  hour  = time_arr(5)
  minut = time_arr(6)
  sec   = time_arr(7)
  
  fmt_str = &
    "(a3,' ',a3,' ',i2.2,' ',i2.2,':',i2.2,':',i2.2,' ',a5,' ',i4.4)" 

  write(date_str,fmt=fmt_str) daynames(day_of_week(month, day, year)), &
    monthnames(month), day, hour, minut, sec, zone_str, year

end function date_str


!-----------------------------------------------------------------------
!
!  integer function day_of_week(month, day, year)
!
!  Returns the day of the week for the given date, with 1 = Sunday,
!  2 = Monday, ..., 7 = Saturday.
!
!  This is an implementation of Lewis Carroll's Algorithm, originally
!  from Martin Gardner's The Universe in a Handkerchief: Lewis Carroll's
!  mathematical recreations, games, puzzles, and word plays (Science
!  Library QA95 .G3325 1996), as reproduced at
!  http://acadprojwww.wlu.edu/vol4/BlackmerH/public_html/xliberty/
!         history/carroll.html
!  This routine only works for the new-style Gregorian dates (e.g.,
!  14 Sept 1752 or later).
!
!-----------------------------------------------------------------------

function day_of_week(month, day, year)

  implicit none

  integer, intent(in) :: month, day, year
  integer :: day_of_week

  integer :: centuries, years_over
  integer :: century_item, year_item, month_item, day_item
  logical :: is_leap_year
  integer, dimension(12) :: days_in_month = &
           (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  integer, dimension(12) :: month_item_arr = &  
           (/ 0, 3, 3, 6, 36, 4, 34, 2, 33, 0, 31, 12 /)


  ! See if this is a leap-year
  is_leap_year = .false. 
  if ( ((year/4)*4 .eq. year) .and. (.not. ((year/400)*400 .eq. year)) ) then
    is_leap_year = .true.
    days_in_month(2) = 29
  end if

  ! Do sanity checks
  if ( (year .lt. 1752) .or.                                           &
       ((year .eq. 1752) .and. (month .lt. 9)) .or.                    &
       ((year .eq. 1752) .and. (month .eq. 9) .and. (day .lt. 14)) .or.&
       (month .lt. 1) .or. (month .gt. 12) .or.                        &
       (day .lt. 1) .or. (day .gt. days_in_month(month)) ) then
    write(0,*) 'Error (DAY_OF_WEEK): unsupported date'
    stop
  end if

  centuries = year / 100
  years_over = mod(year, 100)

  century_item = (3 - mod(centuries, 4)) * 2
  year_item = (years_over/12) + mod(years_over, 12) + mod(years_over, 12)/4
  month_item = month_item_arr(month)
  day_item = 0
  if ( is_leap_year .and. (month .eq. 1 .or. month .eq. 2 ) ) day_item = -1
  day_item = day_item + day

  day_of_week = century_item + year_item + month_item + day_item
  if ( day_of_week .lt. 0 ) day_of_week = day_of_week + 7
  day_of_week = mod(day_of_week, 7) + 1

end function day_of_week


end module timing
