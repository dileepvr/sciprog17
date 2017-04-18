!-----------------------------------------------------------------------
!
!  module param_parser
!
!  Contains the parse_params subroutine for loading parameters from
!  the parameter (.param) file.  Also does some setup, etc.
!
!-----------------------------------------------------------------------


module param_parser

  use timing
  use utilities
  use globals

contains


!-----------------------------------------------------------------------
!
!  subroutine parse_params()
!
!  Loads parameters from the parameter file, which is given on the
!    command line. Also constructs a few other static parameters.
!
!  To get the parameters, use one of 4 functions:
!    get_real_param(p_file, 'str', var, trace)
!    get_int_param(p_file, 'str', var, trace)
!    get_log_param(p_file, 'str', var, trace)
!    get_str_param(p_file, 'str', var, trace)
!
!    for reading in parameters of type real, integer, logical, or
!    string, respectively.  Arguments:
!       p_file (string) -> file name of parameter file
!       'str' (string)  -> name of parameter to get as written in 
!                            param file
!       var (various)   -> value of parameter from file is returned here
!       trace (logical) -> whether or not to print out the parsed
!                            value of the param to std err
!
!    see sample parameter file for more info on parameters
!
!-----------------------------------------------------------------------

subroutine parse_params()

  implicit none

  integer       :: ierr, j
  character(64) :: p_file
  logical       :: exists

  ! parse command line argument (for parameter file name)
  if ( command_argument_count() .ne. 1 ) then
    ierr = 1
    do while ( ierr .ne. 0 )
      write(*,'(30A)', advance='no') 'Enter name of parameter file: '
      read(*,"(A)",iostat=ierr) p_file
    end do
  else
    call get_command_argument(1, p_file)
  end if
  p_file = trim(p_file)


  ! get display modes
  call get_log_param(p_file,'param_trace',param_trace,.false.)
  if ( param_trace ) then
    write(0,*) 'Reading parameters from file ', trim(p_file)
  end if
  call get_log_param(p_file,'time_trace',time_trace,param_trace)
  if ( time_trace ) call initialize_timer()
  call get_log_param(p_file,'step_trace',step_trace,param_trace)
  call get_log_param(p_file,'debug_trace',debug_trace,param_trace)

  call get_log_param(p_file,'foobar',foobar,.false.)

  ! get input config filename
  call get_log_param(p_file,'data_file',data_file,param_trace)
  if ( data_file ) then
     call get_str_param(p_file,'infilename',infilename,param_trace)
     inquire( FILE=infilename(1:indlnb(infilename)), EXIST=exists )
     if ( exists ) then
        call get_int_param(p_file,'inftype',inftype,param_trace)
        call get_str_param(p_file,'data_delim',data_delim,param_trace)        
     else
        goto 201
     end if
  end if

  ! get other parameters
  call get_int_param(p_file,'testint',testint,param_trace)
  call get_real_param(p_file,'testreal',testreal,param_trace)


  ! output file stuff
  call get_str_param(p_file,'base_file_tag',base_file_tag,param_trace)

  if ( time_trace ) then
    write(0,*)
    write(0,*) 'Initializing ...'
  end if

  if (testint .le. 0) then
     testint = 1
  end if
     
  return

201 continue

   write(0,*)  'parse_params(): Config file: "', trim(infilename),'" does not exist!'

   stop
  
end subroutine parse_params

end module param_parser
