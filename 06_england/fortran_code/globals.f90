!=======================================================================
!
!  globals.f90
!
!  Module containing global variables
!
!=======================================================================

module globals

  implicit none

  ! parameters
!  integer, parameter :: wp = selected_real_kind(p=14)
  integer, parameter :: wp = selected_real_kind(p=8)
  real(wp), parameter :: pi = atan(1.0_wp)*4
  real(wp), parameter :: ONE = 1.0_wp
  real(wp), parameter :: NAN = TRANSFER(z'7FF0000000000001', ONE)

  ! control parameters
  logical :: param_trace, time_trace, step_trace, debug_trace
  logical :: data_file, foobar

  ! test paramters
  real(kind=wp) :: testreal
  integer :: testint

  ! system variables
  integer :: dnum, ncols, boxw2, gauw2
  real(wp) :: expalph, gausig
  integer, allocatable, dimension(:) :: year
  character(64), allocatable, dimension(:) :: colnames
  real(wp), allocatable, dimension(:,:) :: Temp, boxT, gausT, expT, baseT

  ! grids, arrays
  integer, dimension(5) :: intarray
  real(wp), allocatable, dimension(:)   :: testrealarray
  integer, allocatable, dimension(:,:) :: testint2darray

  ! where 'ext' is different for each file (but generally ends in .txt)
  character(64) :: base_file_tag, outfilename

  ! input data file
  character(64) :: infilename
  integer :: inftype  ! Number of header lines in data file
  character(5) :: data_delim  ! Data file delimiter


end module globals

