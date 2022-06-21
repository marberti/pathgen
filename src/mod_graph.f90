module mod_graph

  use mod_error
  use mod_format_time
  use mod_get_field

  implicit none
  save
  private

  ! public procedures
  public :: set_graphtype,          &
            set_nodetype,           &
            set_graph_nodes,        &
            set_graph_nodelist,     &
            set_graph_grouplist,    &
            set_fromto_vert,        &
            init_graph_conn,        &
            find_graph_paths

  ! protected variables
  public    :: graphtype,       &
               nodetype,        &
               graph_nodes,     &
               graph_nodelist,  &
               graph_grouplist, &
               graph_paths,     &
               start_vert,      &
               end_vert,        &
               graph_conn,      &
               flag_graph_conn, &
               paths_found,     &
               dead_paths
  protected :: graphtype,       &
               nodetype,        &
               graph_nodes,     &
               graph_nodelist,  &
               graph_grouplist, &
               graph_paths,     &
               start_vert,      &
               end_vert,        &
               graph_conn,      &
               flag_graph_conn, &
               paths_found,     &
               dead_paths

  type :: graph_paths_t
    integer, dimension(:), allocatable :: node
    integer :: sz
  end type graph_paths_t

  character(20) :: graphtype
  character(20) :: nodetype
  integer :: graph_nodes = -1
  character(16), dimension(:), allocatable :: graph_nodelist
  character(16), dimension(:), allocatable :: graph_grouplist
  character(16), dimension(:), allocatable :: graph_unique_groups
  type(graph_paths_t), dimension(:), allocatable :: graph_paths
  integer :: start_vert = -1
  integer :: end_vert   = -1
  integer :: paths_found
  integer :: dead_paths
  logical, dimension(:,:), allocatable :: graph_conn
  logical :: flag_graph_conn = .false.

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_graphtype(str)

  character(*), intent(in) :: str
  character(*), parameter :: my_name = "set_graphtype"

  select case (str)
  case ("directed","undirected")
    graphtype = str
  case default
    call error(my_name,"unknown type "//str)
  end select

end subroutine set_graphtype

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_nodetype(str)

  character(*), intent(in) :: str
  character(*), parameter :: my_name = "set_nodetype"

  select case (str)
  case ("individual","group")
    nodetype = str
  case default
    call error(my_name,"unknown type "//str)
  end select

end subroutine set_nodetype

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_graph_nodes(n)

  integer :: n
  character(*), parameter :: my_name = "set_graph_nodes"

  if (n <= 0) call error(my_name,"number of nodes must be > 0")

  graph_nodes = n

end subroutine set_graph_nodes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_graph_nodelist(list)

  character(*), dimension(:), intent(in) :: list
  character(*), parameter :: my_name = "set_graph_nodelist"
  integer :: n
  integer :: err_n
  character(120) :: err_msg

  n = size(list)

  allocate(graph_nodelist(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  graph_nodelist = list

end subroutine set_graph_nodelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_graph_grouplist(list)

  character(*), dimension(:), intent(in) :: list
  character(*), parameter :: my_name = "set_graph_grouplist"
  integer :: n
  integer :: err_n
  character(120) :: err_msg

  n = size(list)

  allocate(graph_grouplist(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  graph_grouplist = list

  call set_graph_unique_groups()

end subroutine set_graph_grouplist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_fromto_vert(mode,str)

  character(*), intent(in) :: mode
  character(*), intent(in) :: str
  character(*), parameter :: my_name = "set_fromto_vert"
  integer :: n
  integer :: i
  integer :: err_n
  character(120) :: err_msg

  if (graph_nodes <= 0) then
    call error(my_name,"graph_nodes not setted")
  end if

  if (allocated(graph_nodelist)) then
    n = -1
    do i=1, size(graph_nodelist)
      if (graph_nodelist(i) == str) then
        n = i
        exit
      end if
    end do

    if (n == -1) call error(my_name,"invalid node "//trim(str))
  else
    read(str,*,iostat=err_n,iomsg=err_msg) n
    if (err_n /= 0) call error(my_name,"expected integer: "//trim(err_msg))

    if ((n < 1).or.(n > graph_nodes)) then
      call error(my_name,trim(str)//" out of range")
    end if
  end if

  select case (mode)
  case ("from")
    start_vert = n
  case ("to")
    end_vert = n
  case default
    call error(my_name,"invalid mode "//mode)
  end select

end subroutine set_fromto_vert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine init_graph_conn(gph)

  logical, dimension(:,:), intent(in) :: gph
  character(*), parameter :: my_name = "init_graph_conn"
  integer :: err_n
  character(120) :: err_msg

  if (size(gph,1) /= size(gph,2)) then
    call error(my_name,"expected square matrix as argument")
  end if

  allocate(graph_conn(size(gph,1),size(gph,1)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  graph_conn = gph
  flag_graph_conn = .true.

end subroutine init_graph_conn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine find_graph_paths()

  use, intrinsic :: iso_fortran_env

  character(*), parameter :: my_name = "find_graph_paths"
  logical, dimension(:), allocatable :: visited
  logical, dimension(:), allocatable :: grp_visited
  integer(INT64) :: time_start
  integer(INT64) :: time_end
  integer(INT64) :: time_rate
  integer(INT64) :: time_max
  integer :: err_n
  character(120) :: err_msg

  ! preliminary checks
  if (flag_graph_conn.eqv..false.) then
    call error(my_name,"graph connections not initialized")
  end if

  if (start_vert == -1) then
    call error(my_name,"start vertex not initialized")
  end if

  if (end_vert == -1) then
    call error(my_name,"end vertex not initialized")
  end if

  ! call the private subroutine
  paths_found = 0
  dead_paths  = 0

  allocate(visited(size(graph_conn,1)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  visited = .false.

  if (allocated(graph_unique_groups)) then
    allocate(grp_visited(size(graph_unique_groups)),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    grp_visited = .false.
  end if

  call system_clock(time_start,time_rate,time_max)
  call priv_find_graph_paths(start_vert,end_vert,visited,grp_visited,"")
  call system_clock(time_end,time_rate,time_max)
  write(*,*) my_name//": Elapsed wall time: "// &
    trim(format_time_s(time_end-time_start,time_rate))

  ! deallocation
  deallocate(visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  if (allocated(grp_visited)) then
    deallocate(grp_visited,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if

end subroutine find_graph_paths

!!! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive subroutine priv_find_graph_paths(i,f,visited,grp_visited,out_str)

  integer, intent(in) :: i
  integer, intent(in) :: f
  logical, dimension(:), intent(in) :: visited
  logical, dimension(:), intent(in) :: grp_visited
  character(*), intent(in) :: out_str
  character(*), parameter :: my_name = "priv_find_graph_paths"
  integer :: j
  integer :: indx1
  integer :: indx2
  logical, dimension(:), allocatable :: nw_visited
  logical, dimension(:), allocatable :: nw_grp_visited
  character(8) :: i_str
  character(8) :: f_str
  logical :: flag_groups
  logical :: found_conn
  integer :: err_n
  character(120) :: err_msg

  ! set working variables
  found_conn = .false.

  allocate(nw_visited(size(visited)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  nw_visited = visited
  nw_visited(i) = .true.

  if (allocated(graph_unique_groups)) then
    flag_groups = .true.
  else
    flag_groups = .false.
  end if
  if (flag_groups) then
    allocate(nw_grp_visited(size(graph_unique_groups)), &
      stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    nw_grp_visited = grp_visited
  end if

  ! core
  do j = 1, size(graph_conn,1)
    if (nw_visited(j)) cycle

    if (flag_groups) then
      indx1 = get_graph_unique_groups_index(graph_grouplist(j))
      if (nw_grp_visited(indx1)) cycle
    end if

    if (graph_conn(i,j)) then
      found_conn = .true.
      write(i_str,'(I8)') i
      i_str = adjustl(i_str)
      if (j == f) then
        write(f_str,'(I8)') f
        f_str = adjustl(f_str)
        call add_path(out_str//" "//trim(i_str)//" "//trim(f_str))
        paths_found = paths_found + 1
      else
        if (flag_groups) then
          indx1 = get_graph_unique_groups_index(graph_grouplist(j))
          indx2 = get_graph_unique_groups_index(graph_grouplist(i))
          if (indx1 /= indx2) nw_grp_visited(indx2) = .true.
        end if

        call priv_find_graph_paths(j,f,nw_visited, &
          nw_grp_visited,out_str//" "//trim(i_str))
      end if
    end if
  end do

  if (found_conn.eqv..false.) dead_paths = dead_paths + 1

  ! deallocation
  deallocate(nw_visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  if (flag_groups) then
    deallocate(nw_grp_visited,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if

end subroutine priv_find_graph_paths

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine add_path(pathstr)

  character(*), intent(in) :: pathstr
  character(*), parameter :: my_name = "add_path"
  type(graph_paths_t), dimension(:), allocatable :: tmp
  character(8) :: node
  integer :: i
  integer :: new_size
  integer :: tot_nodes
  integer :: err_n
  character(120) :: err_msg

  ! count total number of nodes in the path
  tot_nodes = 0
  do
    call get_field(pathstr,node,tot_nodes+1,err_n,err_msg)
    if (err_n /= 0) exit
    tot_nodes = tot_nodes + 1
  end do

  ! allocate new graph_path
  if (allocated(graph_paths)) then
    new_size = size(graph_paths) + 1
    call move_alloc(graph_paths,tmp)
    allocate(graph_paths(new_size),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    graph_paths(:new_size-1) = tmp
    deallocate(tmp,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  else
    new_size = 1
    allocate(graph_paths(new_size),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if

  graph_paths(new_size)%sz = tot_nodes
  allocate(graph_paths(new_size)%node(tot_nodes),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! store the path
  do i = 1, tot_nodes
    call get_field(pathstr,node,i,err_n,err_msg)
    read(node,*) graph_paths(new_size)%node(i)
  end do

end subroutine add_path

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_graph_unique_groups()

  character(*), parameter :: my_name = "set_graph_unique_groups"
  integer :: n
  integer :: i
  integer :: j
  integer :: counter
  character(16), dimension(:), allocatable :: enc_groups
  logical :: flag_new_group
  integer :: err_n
  character(120) :: err_msg

  n = size(graph_grouplist)
  allocate(enc_groups(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! search unique groups
  enc_groups(1) = graph_grouplist(1)
  counter = 1
  do i = 2, n
    flag_new_group = .true.

    do j = 1, counter
      if (graph_grouplist(i) == enc_groups(j)) then
        flag_new_group = .false.
        exit
      end if
    end do

    if (flag_new_group) then
      counter = counter + 1
      enc_groups(counter) = graph_grouplist(i)
    end if
  end do

  ! set global variable for unique groups
  allocate(graph_unique_groups(counter),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  do i = 1, counter
    graph_unique_groups(i) = enc_groups(i)
  end do

  ! deallocation
  deallocate(enc_groups,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine set_graph_unique_groups

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer function get_graph_unique_groups_index(g)

  character(*), intent(in) :: g
  integer :: n
  integer :: i

  n = size(graph_unique_groups)

  do i = 1, n
    if (graph_unique_groups(i) == g) then
      get_graph_unique_groups_index = i
      exit
    end if
  end do

end function get_graph_unique_groups_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_graph
