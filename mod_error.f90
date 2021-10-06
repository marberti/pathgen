module mod_error

  implicit none
  save
  public

  integer        :: err_n
  character(120) :: err_msg

contains

!==============================================================================

subroutine error(msg)

  use iso_fortran_env

  character(*), intent(IN) :: msg

  write(ERROR_UNIT,*)  "ERR "//msg
  write(OUTPUT_UNIT,*) "ERR "//msg
  stop 1

end subroutine error

!==============================================================================

end module mod_error
