module mod_output

  use mod_error
  use mod_graph

  implicit none
  save
  private

  ! public procedures ---------------------------------------------------------
  public :: open_output_file, &
            write_input_graph

contains

!==============================================================================

subroutine open_output_file()

  character(*), parameter :: my_name = "open_output_file"

  character(*), parameter :: fname = "graph.out"

  open(unit=fout_numb,file=fname,action="write",iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))

end subroutine open_output_file

!==============================================================================

subroutine write_input_graph()



end subroutine write_input_graph

!==============================================================================

end module mod_output
