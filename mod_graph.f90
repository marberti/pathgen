module mod_graph

  use mod_error
  use mod_utility

  implicit none
  save
  private

  ! public procedures ---------------------------------------------------------
  public :: set_start_vert,   &
            set_end_vert,     &
            init_graph_conn,  &
            find_graph_paths

  ! protected variables -------------------------------------------------------
  public    :: graph_paths,    &
               start_vert,     &
               end_vert,       &
               graph_conn,     &
               flag_graph_conn
  protected :: graph_paths,    &
               start_vert,     &
               end_vert,       &
               graph_conn,     &
               flag_graph_conn

  type :: graph_paths_t
    integer, dimension(:), allocatable :: node
    integer :: sz
  end type graph_paths_t

  type(graph_paths_t), dimension(:), allocatable :: graph_paths
  integer :: start_vert = -1
  integer :: end_vert   = -1
  integer :: paths_found
  integer :: dead_paths
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
  logical :: flag_opened

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

  inquire(unit=fout_numb,opened=flag_opened)
  if (flag_opened.eqv..false.) call error(my_name//": output file not opened")

  ! call the private subroutine -----------------------------------------------
  write(fout_numb,*) "Paths:"
  paths_found = 0
  dead_paths  = 0
  allocate(visited(size(graph_conn,1)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  visited = .false.
  call priv_find_graph_paths(start_vert,end_vert,visited,"")
  write(fout_numb,*)
  write(fout_numb,*) "Total paths found: ", paths_found
  write(fout_numb,*) "Dead  paths found: ", dead_paths

end subroutine find_graph_paths

!==============================================================================
! Private
!==============================================================================

recursive subroutine priv_find_graph_paths(i,f,visited,out_str)

  integer, intent(in) :: i
  integer, intent(in) :: f
  logical, dimension(:), intent(in) :: visited
  character(*), intent(in) :: out_str

  character(*), parameter :: my_name = "priv_find_graph_paths"
  integer :: j
  logical, dimension(:), allocatable :: nw_visited
  character(8) :: i_str
  character(8) :: f_str
  logical :: found_conn

  found_conn = .false.

  allocate(nw_visited(size(visited)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  nw_visited = visited
  nw_visited(i) = .true.

  do j = 1, size(graph_conn,1)
    if (nw_visited(j)) cycle

    if (graph_conn(i,j)) then
      found_conn = .true.
      write(i_str,'(I8)') i
      i_str = adjustl(i_str)
      if (j == f) then
        write(f_str,'(I8)') f
        f_str = adjustl(f_str)
        write(fout_numb,*) out_str//" "//trim(i_str)//" "//trim(f_str)
        call add_path(out_str//" "//trim(i_str)//" "//trim(f_str))
        paths_found = paths_found + 1
      else
         call priv_find_graph_paths(j,f,nw_visited,out_str//" "//trim(i_str))
      end if
    end if
  end do

  if (found_conn.eqv..false.) dead_paths = dead_paths + 1

end subroutine priv_find_graph_paths

!==============================================================================

subroutine add_path(pathstr)

  character(*), intent(in) :: pathstr

  character(*), parameter :: my_name = "add_path"
  type(graph_paths_t), dimension(:), allocatable :: tmp
  character(8) :: node
  integer :: i
  integer :: new_size
  integer :: tot_nodes

  ! count total number of nodes in the path -----------------------------------
  tot_nodes = 0
  do
    call get_field(pathstr,node,tot_nodes+1,err_n,err_msg)
    if (err_n /= 0) exit
    tot_nodes = tot_nodes + 1
  end do

  ! allocate new graph_path ---------------------------------------------------
  if (allocated(graph_paths)) then
    new_size = size(graph_paths) + 1
    call move_alloc(graph_paths,tmp)
    allocate(graph_paths(new_size),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name//": "//trim(err_msg))
    graph_paths(:new_size-1) = tmp
    deallocate(tmp,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  else
    new_size = 1
    allocate(graph_paths(new_size),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name//": "//trim(err_msg))
  end if

  graph_paths(new_size)%sz = tot_nodes
  allocate(graph_paths(new_size)%node(tot_nodes),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name//": "//trim(err_msg))

  ! store the path ------------------------------------------------------------
  do i = 1, tot_nodes
    call get_field(pathstr,node,i,err_n,err_msg)
    read(node,*) graph_paths(new_size)%node(i)
  end do

end subroutine add_path

!==============================================================================

end module mod_graph
