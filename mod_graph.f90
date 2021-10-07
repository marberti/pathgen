module mod_graph

  use mod_error

  implicit none
  save
  private

  ! public procedures ---------------------------------------------------------
  public :: set_start_vert,   &
            set_end_vert,     &
            init_graph_conn,  &
            find_graph_paths

  ! protected variables -------------------------------------------------------
  public    :: start_vert,     &
               end_vert,       &
               graph_conn,     &
               flag_graph_conn
  protected :: start_vert,     &
               end_vert,       &
               graph_conn,     &
               flag_graph_conn

  integer :: start_vert = -1
  integer :: end_vert   = -1
  logical, dimension(:,:), allocatable :: graph_conn
  logical :: flag_graph_conn = .false.

contains

!==============================================================================
! Public
!==============================================================================

subroutine set_start_vert(a)

  integer, intent(in) :: a

  character(*), parameter :: my_name = "set_start_vert"

  ! preliminary checks --------------------------------------------------------
  if (flag_graph_conn.eqv..false.) then
    call error(my_name//": graph connections not initialized")
  end if

  if ((a < 1).or.(a > size(graph_conn,1))) then
    call error(my_name//": start vertex out of bounds")
  end if

  start_vert = a

end subroutine set_start_vert

!==============================================================================

subroutine set_end_vert(a)

  integer, intent(in) :: a

  character(*), parameter :: my_name = "set_end_vert"

  ! preliminary checks --------------------------------------------------------
  if (flag_graph_conn.eqv..false.) then
    call error(my_name//": graph connections not initialized")
  end if

  if ((a < 1).or.(a > size(graph_conn,1))) then
    call error(my_name//": end vertex out of bounds")
  end if

  end_vert = a

end subroutine set_end_vert

!==============================================================================

subroutine init_graph_conn(gph)

  logical, dimension(:,:), intent(in) :: gph

  character(*), parameter :: my_name = "init_graph_conn"

  if (size(gph,1) /= size(gph,2)) then
    call error(my_name//": expected square matrix as argument")
  end if

  allocate(graph_conn(size(gph,1),size(gph,1)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))

  graph_conn = gph
  flag_graph_conn = .true.

end subroutine init_graph_conn

!==============================================================================

subroutine find_graph_paths()

  character(*), parameter :: my_name = "find_graph_paths"
  logical, dimension(:), allocatable :: visited

  ! preliminary checks --------------------------------------------------------
  if (flag_graph_conn.eqv..false.) then
    call error(my_name//": graph connections not initialized")
  end if

  if (start_vert == -1) then
    call error(my_name//": start vertex not initialized")
  end if

  if (end_vert == -1) then
    call error(my_name//": end vertex not initialized")
  end if

  ! call the private subroutine -----------------------------------------------
  allocate(visited(size(graph_conn,1)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  visited = .false.
  call priv_find_graph_paths(start_vert,end_vert,visited,"")

end subroutine find_graph_paths

!==============================================================================
! Private
!==============================================================================

recursive subroutine priv_find_graph_paths(i,f,visited,out_str)

  integer, intent(in) :: i
  integer, intent(in) :: f
  logical, dimension(:), intent(in) :: visited
  character(*), intent(in) :: out_str

end subroutine priv_find_graph_paths

!==============================================================================

end module mod_graph
