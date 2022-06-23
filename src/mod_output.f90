module mod_output

  use mod_error
  use mod_graph

  implicit none
  save
  private

  ! public procedures
  public :: set_output_file,        &
            write_input_graph

  ! private variables
  integer, parameter :: fout_numb = 700
  character(120) :: fout_name

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_output_file(fin_name)

  character(*), intent(in) :: fin_name
  character(*), parameter :: my_name = "set_output_file"

  if (fin_name(len_trim(fin_name)-3:) == ".dat") then
    fout_name = trim(fin_name(:len_trim(fin_name)-4))//".out"
  else
    if (len_trim(fin_name) > len(fout_name) - 4) then
      call error(my_name,"input file name too long")
    end if
    fout_name = trim(fin_name)//".out"
  end if

end subroutine set_output_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine write_input_graph()

  character(*), parameter :: my_name = "write_input_graph"
  integer :: i
  integer :: sz
  character(8) :: i_str
  character(120) :: frmt
  integer :: err_n
  character(120) :: err_msg

  open(unit=fout_numb,file=fout_name,status="replace",action="write", &
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

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
  if (allocated(graph_nodelist)) then
    write(i_str,'(I8)') start_vert
    i_str = adjustl(i_str)
    write(fout_numb,*) "Start vertex: "//trim(graph_nodelist(start_vert))// &
      " ("//trim(i_str)//")"
    write(i_str,'(I8)') end_vert
    i_str = adjustl(i_str)
    write(fout_numb,*) "End   vertex: "//trim(graph_nodelist(end_vert))// &
      " ("//trim(i_str)//")"
  else
    write(fout_numb,*) "Start vertex: ", start_vert
    write(fout_numb,*) "End   vertex: ", end_vert
  end if

  close(unit=fout_numb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine write_input_graph

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_output
