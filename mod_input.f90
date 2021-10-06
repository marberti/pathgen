module mod_input

  use mod_error

  implicit none
  save
  private

  public :: read_input

contains

!==============================================================================

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
  integer :: ver_n
  integer :: edg_n
  integer :: i
  integer :: a
  integer :: b
  integer :: start_ver
  integer :: end_ver
  logical, dimension(:,:), allocatable :: graph_conn

  ! open input file -----------------------------------------------------------
  open(unit=fnumb,file=fname,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))

  ! read input file -----------------------------------------------------------
  read(fnumb,*,iostat=err_n,iomsg=err_msg) ver_n
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  if (ver_n < 2)  call error(my_name//": at least 2 vertices required")

  read(fnumb,*,iostat=err_n,iomsg=err_msg) edg_n
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  if (edg_n < 0)  call error(my_name//": positive number of edges required")

  allocate(graph_conn(ver_n,ver_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  graph_conn = .false.

  do i = 1, edg_n
    read(fnumb,*,iostat=err_n,iomsg=err_msg) a, b
    if (err_n /= 0) call error(my_name//": "//trim(err_msg))
    if ((a < 1).or.(a > ver_n)) call error(my_name//": vertex out of bounds")
    if ((b < 1).or.(b > ver_n)) call error(my_name//": vertex out of bounds")
    graph_conn(a,b) = .true.
    graph_conn(b,a) = .true.
  end do

  read(fnumb,*,iostat=err_n,iomsg=err_msg) start_ver, end_ver
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  if ((start_ver < 1).or.(start_ver > ver_n)) then
    call error(my_name//": start vertex out of bounds")
  end if
  if ((end_ver < 1).or.(end_ver > ver_n)) then
    call error(my_name//": end vertex out of bounds")
  end if

  read(fnumb,*,iostat=err_n,iomsg=err_msg) ch
  if (err_n == 0) call error(my_name//": too many lines in the input file")

  ! close input file ----------------------------------------------------------
  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))

end subroutine read_input

!==============================================================================

end module mod_input
