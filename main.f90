program main

  use mod_input
  use mod_output

  implicit none

  call read_input()
  call open_output_file()
  call write_input_graph()

end program main
