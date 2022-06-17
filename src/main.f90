program main

  use mod_input
  use mod_output
  use mod_graph

  implicit none

  character(*), parameter :: fname = "graph.dat"

  call read_input(fname)
  call write_input_graph()
  !@@@
  stop 42
  !@@@
  call find_graph_paths()
  call write_graph_paths(.true.)

end program main
