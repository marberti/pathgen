module mod_input

  use mod_error
  use mod_graph

  implicit none
  save
  private

  public :: read_input

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine read_input()

  ! Input file structure:
  !
  ! number_of_vertices_V (one integer)
  ! number_of_edges_E    (one integer)
  ! edge_descriptor_1    (two integers, the two connected vertices)
  ! ...                  (there are E lines of edge descriptors)
  ! edge_descriptor_E
  ! start_node end_node  (two integers, the nodes between which
  !                         the paths are to be found)

  character(*), parameter :: my_name = "read_input"
  character(*), parameter :: fname   = "graph.dat"
  integer, parameter      :: fnumb   = 600
  character :: ch
  integer :: vert_n
  integer :: edge_n
  integer :: i
  integer :: a
  integer :: b
  integer :: start_vert
  integer :: end_vert
  logical, dimension(:,:), allocatable :: graph_conn
  integer :: err_n
  character(120) :: err_msg

  ! open input file
  open(unit=fnumb,file=fname,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! read input file
  read(fnumb,*,iostat=err_n,iomsg=err_msg) vert_n
  if (err_n /= 0) call error(my_name,err_msg)
  if (vert_n < 2) call error(my_name,"at least 2 vertices required")

  read(fnumb,*,iostat=err_n,iomsg=err_msg) edge_n
  if (err_n /= 0) call error(my_name,err_msg)
  if (edge_n < 0) call error(my_name,"positive number of edges required")

  allocate(graph_conn(vert_n,vert_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  graph_conn = .false.

  do i = 1, edge_n
    read(fnumb,*,iostat=err_n,iomsg=err_msg) a, b
    if (err_n /= 0) call error(my_name,err_msg)
    if ((a < 1).or.(a > vert_n)) call error(my_name,"vertex out of bounds")
    if ((b < 1).or.(b > vert_n)) call error(my_name,"vertex out of bounds")
    graph_conn(a,b) = .true.
    graph_conn(b,a) = .true.
  end do

  call init_graph_conn(graph_conn)

  read(fnumb,*,iostat=err_n,iomsg=err_msg) start_vert, end_vert
  if (err_n /= 0) call error(my_name,err_msg)
  if ((start_vert < 1).or.(start_vert > vert_n)) then
    call error(my_name,"start vertex out of bounds")
  end if
  if ((end_vert < 1).or.(end_vert > vert_n)) then
    call error(my_name,"end vertex out of bounds")
  end if

  call set_start_vert(start_vert)
  call set_end_vert(end_vert)

  read(fnumb,*,iostat=err_n,iomsg=err_msg) ch
  if (err_n == 0) call error(my_name,"too many lines in the input file")

  ! close input file
  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  deallocate(graph_conn,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine read_input

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_input
