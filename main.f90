program main

  use mod_input
  use mod_output
  use mod_graph

  implicit none

  call read_input()
  call open_output_file()
  call write_input_graph()
  call find_graph_paths()
  call write_graph_paths()

end program main
