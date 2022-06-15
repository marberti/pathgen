module mod_error

  implicit none
  save
  public

  integer, parameter :: fout_numb = 700

contains

!==============================================================================

subroutine error(proc_name,proc_msg,fnumb)

  use, intrinsic :: iso_fortran_env

  character(*), intent(in) :: proc_name
  character(*), intent(in) :: proc_msg
  integer, optional, intent(in) :: fnumb
  logical :: flag_opened

  if (present(fnumb)) then
    inquire(unit=fnumb,opened=flag_opened)
    if (flag_opened) then
      write(fnumb,'(" ERR ",A,": ",A)') trim(proc_name), trim(proc_msg)
    end if
  end if

  write(ERROR_UNIT,'(" ERR ",A,": ",A)') trim(proc_name), trim(proc_msg)
  stop 1

end subroutine error

!==============================================================================

end module mod_error
