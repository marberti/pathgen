program main

  use mod_input
  use mod_output
  use mod_graph

  implicit none

  character(120) :: prog_name
  character(120) :: fin_name

  call get_command_argument(0,prog_name)

  if (command_argument_count() /= 1) then
    call help(prog_name)
    stop 1
  end if

  call get_command_argument(1,fin_name)
  call set_output_file(fin_name)
  call read_input(fin_name)
  call write_input_graph()
  call find_graph_paths()
  call write_graph_paths(.true.)

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine help(prog_name)

  character(*), intent(in) :: prog_name

  write(*,*) "usage: "//trim(prog_name)//" <file>"

end subroutine help

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program main
