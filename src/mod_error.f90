module mod_error

  implicit none
  save
  public

  integer, parameter :: fout_numb = 700
  integer        :: err_n
  character(120) :: err_msg

contains

!==============================================================================

subroutine error(msg)

  use iso_fortran_env

  character(*), intent(in) :: msg

  logical :: flag_opened

  inquire(unit=fout_numb,opened=flag_opened)
  if (flag_opened) then
    write(fout_numb,*) "ERR "//msg
  end if
  write(ERROR_UNIT,*)  "ERR "//msg
  stop 1

end subroutine error

!==============================================================================

end module mod_error
