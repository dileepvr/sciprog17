module FFTSH_MAT

!====================================================================
! This module contains function that implements equivalend ones
! in MATLAB.
! fftsh(,,0) is same as fftshift(fft(ifftshift()))
! fftsh(,,1) is same as fftshift(ifft(ifftshift()))
!  This module profices interfaces for the FFTW library.
!====================================================================

Implicit None

Include "fftw3.f"

Integer, parameter :: kreal = kind(1.0D0)

Interface
   Function fftw_plan_dft_1d(n, in, out, sign, flags) BIND(C,name="fftw_plan_dft_1d")
     Use, intrinsic :: ISO_C_BINDING, only : C_PTR, C_INT, C_DOUBLE
     Import         :: kreal
     Integer(C_INT), value, intent(in) :: n, sign, flags
     Complex(C_DOUBLE)                 :: in(n), out(n)
     Type(C_PTR)                       :: fftw_plan_dft_1d
   End Function

   Subroutine fftw_execute(plan) BIND(C,name="fftw_execute")
     Use, intrinsic :: ISO_C_BINDING, only : C_PTR
     Type(C_PTR), value :: plan
   End Subroutine

   Subroutine fftw_destroy_plan(plan) BIND(C,name="fftw_destroy_plan")
     Use, intrinsic :: ISO_C_BINDING, only : C_PTR
     Type(C_PTR), value :: plan
   End Subroutine
End Interface

  public :: fftsh

contains

  Subroutine fftsh(data_in, data_out, N, mode)
  !===================================================================
  ! Subroutine that executes 
  ! fftshift(fft(ifftshift())) if mode == 0, and
  ! fftshift(ifft(ifftshift())) if mode == 1
  ! fftshift(), ifftshift(), fft() and ifft() are MATLAB functions.
  !===================================================================

  Use, intrinsic :: ISO_C_BINDING, only : C_PTR, C_INT, C_DOUBLE
  implicit none

  Integer, intent(in) :: N, mode
  Integer :: N2

  Real(kreal) :: Nsq
  
  Complex(C_DOUBLE), intent(in), dimension(N) :: data_in  
  Complex(C_DOUBLE), intent(out), dimension(N) :: data_out


  Real(kreal), parameter :: zero = 0.0D0
  Real(kreal), parameter :: one  = 1.0D0


  Complex(C_DOUBLE), allocatable, dimension(:) :: data

  Type(C_PTR) :: plan

  Integer  :: id


  Allocate(data(N))
  if (mode == 0) then
     plan  = fftw_plan_dft_1d (N, data, data_out, FFTW_FORWARD,  FFTW_ESTIMATE)
  else
     plan = fftw_plan_dft_1d (N, data, data_out, FFTW_BACKWARD, FFTW_ESTIMATE)
  end if

  Nsq = 1.0/sqrt(1.0*N)


  !... perform ifftshift()
  !----------------------------------------------------
  N2 = ceiling(0.5*N)
  if ( mod(N,2) == 0 ) then
    data = data_in((/ (id, id=N2+1,N) , (id, id=1,N2) /))
  else 
    data = data_in((/ (id, id=N2,N) , (id, id=1,N2-1) /))
  end if 

  !... Call fftw
  !----------------------------------------------------
  Call fftw_execute (plan)


  !... perform fftshift()
  !----------------------------------------------------
  data_out = data_out((/ (id, id=N2+1,N) , (id, id=1,N2) /))*Nsq

  Call fftw_destroy_plan (plan)

  Deallocate(data)

  end Subroutine fftsh

end module FFTSH_MAT
