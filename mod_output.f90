module mod_output

  use mod_error
  use mod_graph

  implicit none
  save
  private

  ! public procedures ---------------------------------------------------------
  public :: open_output_file,  &
            write_input_graph, &
            write_graph_paths

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

  character(*), parameter :: my_name = "write_input_graph"
  logical :: flag_opened
  integer :: i
  integer :: sz
  character(8) :: i_str
  character(120) :: frmt

  inquire(unit=fout_numb,opened=flag_opened)
  if (flag_opened.eqv..false.) call error(my_name//": output file not opened")

  sz = size(graph_conn,1)
  write(i_str,'(I8)') sz
  i_str = adjustl(i_str)
  frmt = "("//trim(i_str)//"(X,L1))"

  write(fout_numb,*) "Graph connections"
  write(fout_numb,*)
  do i = 1, sz
    write(fout_numb,frmt) graph_conn(i,:)
  end do
  write(fout_numb,*)
  write(fout_numb,*) "Start vertex: ", start_vert
  write(fout_numb,*) "End   vertex: ", end_vert
  write(fout_numb,*)

end subroutine write_input_graph

!==============================================================================

subroutine write_graph_paths()

  character(*), parameter :: my_name = "write_graph_paths"
  logical :: flag_opened
  integer :: i
  integer :: j

  inquire(unit=fout_numb,opened=flag_opened)
  if (flag_opened.eqv..false.) call error(my_name//": output file not opened")

  write(fout_numb,*) "Paths:"
  do i = 1, size(graph_paths)
    do j = 1, graph_paths(i)%sz
      write(fout_numb,'(1X,I4)',advance="no") graph_paths(i)%node(j)
    end do
    write(fout_numb,*)
  end do
  write(fout_numb,*)
  write(fout_numb,*) "Total paths found: ", paths_found
  write(fout_numb,*) "Dead  paths found: ", dead_paths

end subroutine write_graph_paths

!==============================================================================

end module mod_output
