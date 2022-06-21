module mod_format_time

  implicit none
  save
  private

  public :: format_time_s

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

character(120) function format_time_s(a,r)

  use, intrinsic :: iso_fortran_env

  integer(INT64), intent(in) :: a
  integer(INT64), intent(in) :: r
  integer(INT64) :: s
  integer(INT64) :: m
  integer(INT64) :: h
  integer(INT64) :: d
  real(REAL64) :: sr
  character(10) :: str

  s = a/r
  m = s/60
  s = mod(s,int(60,INT64))
  h = m/60
  m = mod(m,int(60,INT64))
  d = h/24
  h = mod(h,int(24,INT64))
  sr = mod(real(a,REAL64)/real(r,REAL64),60.0_REAL64)

  format_time_s = ""
  if (d /= 0) then
    write(str,'(I10)') d
    str = adjustl(str)
    format_time_s = trim(format_time_s)//trim(str)//" d "
  end if
  if ((h /= 0).or.(d /= 0)) then
    write(str,'(I10)') h
    str = adjustl(str)
    format_time_s = trim(format_time_s)//trim(str)//" h "
  end if
  if ((m /= 0).or.(h /= 0).or.(d /= 0)) then
    write(str,'(I10)') m
    str = adjustl(str)
    format_time_s = trim(format_time_s)//trim(str)//" m "
  end if
  write(str,'(F10.3)') sr
  str = adjustl(str)
  format_time_s = trim(format_time_s)//trim(str)//" s"

end function format_time_s

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_format_time
