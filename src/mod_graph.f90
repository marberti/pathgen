module mod_graph

  use, intrinsic :: iso_fortran_env
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
            set_search_from_groups, &
            init_graph_conn,        &
            find_graph_paths

  ! protected variables
  public    :: graphtype,       &
               nodetype,        &
               graph_nodes,     &
               graph_nodelist,  &
               graph_grouplist, &
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
               start_vert,      &
               end_vert,        &
               graph_conn,      &
               flag_graph_conn, &
               paths_found,     &
               dead_paths

  character(*), parameter :: graph_fileout_name = "graph_paths.out"
  integer, parameter :: graph_fileout = 701
  integer, parameter :: graph_pathstrings_len = 100000
  character(20) :: graphtype
  character(20) :: nodetype
  integer :: graph_nodes = -1
  character(16), dimension(:), allocatable :: graph_nodelist
  character(16), dimension(:), allocatable :: graph_grouplist
  character(16), dimension(:), allocatable :: graph_unique_groups
  character(400), dimension(graph_pathstrings_len) :: graph_pathstrings
  integer :: graph_pathstrings_index = 0
  integer :: start_vert = -1
  integer :: end_vert   = -1
  integer :: start_vert_grp = -1
  integer :: end_vert_grp   = -1
  integer(INT64) :: paths_found
  integer(INT64) :: dead_paths
  logical, dimension(:,:), allocatable :: graph_conn
  logical, dimension(:,:), allocatable :: graph_conn_grp
  logical :: flag_graph_conn = .false.
  logical :: flag_graph_grouplist = .false.
  logical :: flag_search_from_groups = .false.

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

  if (n /= graph_nodes) call error(my_name,"wrong size of argument list")

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

  if (n /= graph_nodes) call error(my_name,"wrong size of argument list")

  allocate(graph_grouplist(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  graph_grouplist = list

  call set_graph_unique_groups()

  flag_graph_grouplist = .true.

end subroutine set_graph_grouplist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine set_search_from_groups()

  character(*), parameter :: my_name = "set_search_from_groups"
  integer :: n
  integer :: i
  integer :: j
  integer :: indx1
  integer :: indx2
  integer :: err_n
  character(120) :: err_msg

  ! checks
  if ((start_vert == -1).or.(end_vert == -1)) then
    call error(my_name,"start or end vertices not set")
  end if

  if (.not.flag_graph_conn) call error(my_name,"graph connections not init")

  if (.not.flag_graph_grouplist) call error(my_name,"groups required")

  ! set
  start_vert_grp = get_graph_unique_groups_index(graph_grouplist(start_vert))
  end_vert_grp   = get_graph_unique_groups_index(graph_grouplist(end_vert))
  if (start_vert_grp == end_vert_grp) then
    call error(my_name,"search from groups requires that "// &
      "start and end vertices belong to different groups")
  end if

  n = size(graph_unique_groups)
  allocate(graph_conn_grp(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  graph_conn_grp = .false.

  do i = 1, graph_nodes
    do j = 1, graph_nodes
      if (graph_conn(i,j)) then
        indx1 = get_graph_unique_groups_index(graph_grouplist(i))
        indx2 = get_graph_unique_groups_index(graph_grouplist(j))
        graph_conn_grp(indx1,indx2) = .true.
      end if
    end do
  end do

  do i = 1, n
    graph_conn_grp(i,i) = .false.
  end do

  flag_search_from_groups = .true.

end subroutine set_search_from_groups

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
    call error(my_name,"graph_nodes not set")
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

  if (size(gph,1) /= graph_nodes) then
    call error(my_name,"wrong size of argument gph")
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

  if (flag_graph_grouplist) then
    allocate(grp_visited(size(graph_unique_groups)),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
    grp_visited = .false.
  end if

  call system_clock(time_start,time_rate,time_max)
  call open_graph_fileout()
  if (flag_graph_grouplist) then
    if (flag_search_from_groups) then
      call priv_find_graph_paths_from_groups(start_vert_grp,end_vert_grp, &
        grp_visited,"")
    else
      call priv_find_graph_paths_grp(start_vert,end_vert,visited, &
        grp_visited,"")
    end if
  else
    call priv_find_graph_paths_std(start_vert,end_vert,visited,"")
  end if
  call flush_graph_pathstrings()
  call close_graph_fileout()
  call system_clock(time_end,time_rate,time_max)
  write(*,*) my_name//": Elapsed wall time: "// &
    trim(format_time_s(time_end-time_start,time_rate))

  ! deallocation
  deallocate(visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  if (flag_graph_grouplist) then
    deallocate(grp_visited,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if

end subroutine find_graph_paths

!!! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Original priv_find_graph_paths used with graphs with and without groups
! before two separate versions were implemented

!recursive subroutine priv_find_graph_paths(i,f,visited,grp_visited,out_str)
!
!  integer, intent(in) :: i
!  integer, intent(in) :: f
!  logical, dimension(:), intent(in) :: visited
!  logical, dimension(:), intent(in) :: grp_visited
!  character(*), intent(in) :: out_str
!  character(*), parameter :: my_name = "priv_find_graph_paths"
!  integer :: j
!  integer :: indx1
!  integer :: indx2
!  logical, dimension(:), allocatable :: nw_visited
!  logical, dimension(:), allocatable :: nw_grp_visited
!  character(8) :: i_str
!  character(8) :: f_str
!  logical :: found_conn
!  logical :: flag_update_grp
!  integer :: err_n
!  character(120) :: err_msg
!
!  ! set working variables
!  found_conn = .false.
!
!  allocate(nw_visited(size(visited)),stat=err_n,errmsg=err_msg)
!  if (err_n /= 0) call error(my_name,err_msg)
!  nw_visited = visited
!  nw_visited(i) = .true.
!
!  if (flag_graph_grouplist) then
!    allocate(nw_grp_visited(size(graph_unique_groups)), &
!      stat=err_n,errmsg=err_msg)
!    if (err_n /= 0) call error(my_name,err_msg)
!    nw_grp_visited = grp_visited
!  end if
!
!  ! core
!  do j = 1, size(graph_conn,1)
!    if (nw_visited(j)) cycle
!
!    if (flag_graph_grouplist) then
!      indx1 = get_graph_unique_groups_index(graph_grouplist(j))
!      if (grp_visited(indx1)) cycle
!    end if
!
!    if (graph_conn(i,j)) then
!      found_conn = .true.
!      write(i_str,'(I8)') i
!      i_str = adjustl(i_str)
!      if (j == f) then
!        write(f_str,'(I8)') f
!        f_str = adjustl(f_str)
!        call add_graph_pathstrings(                   &
!          out_str//" "//trim(i_str)//" "//trim(f_str) &
!        )
!        paths_found = paths_found + 1
!      else
!        flag_update_grp = .false.
!        if (flag_graph_grouplist) then
!          indx1 = get_graph_unique_groups_index(graph_grouplist(j))
!          indx2 = get_graph_unique_groups_index(graph_grouplist(i))
!          if (indx1 /= indx2) flag_update_grp = .true.
!        end if
!
!        if (flag_update_grp) then
!          nw_grp_visited(indx2) = .true.
!          call priv_find_graph_paths(j,f,nw_visited, &
!            nw_grp_visited,out_str//" "//trim(i_str))
!          nw_grp_visited(indx2) = .false.
!        else
!          call priv_find_graph_paths(j,f,nw_visited, &
!            nw_grp_visited,out_str//" "//trim(i_str))
!        end if
!      end if
!    end if
!  end do
!
!  if (found_conn.eqv..false.) dead_paths = dead_paths + 1
!
!  ! deallocation
!  deallocate(nw_visited,stat=err_n,errmsg=err_msg)
!  if (err_n /= 0) call error(my_name,err_msg)
!  if (flag_graph_grouplist) then
!    deallocate(nw_grp_visited,stat=err_n,errmsg=err_msg)
!    if (err_n /= 0) call error(my_name,err_msg)
!  end if
!
!end subroutine priv_find_graph_paths

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive subroutine priv_find_graph_paths_std(i,f,visited,out_str)

  integer, intent(in) :: i
  integer, intent(in) :: f
  logical, dimension(:), intent(in) :: visited
  character(*), intent(in) :: out_str
  character(*), parameter :: my_name = "priv_find_graph_paths_std"
  integer :: j
  logical, dimension(:), allocatable :: nw_visited
  character(8) :: i_str
  character(8) :: f_str
  logical :: found_conn
  integer :: err_n
  character(120) :: err_msg

  ! set working variables
  found_conn = .false.

  allocate(nw_visited(size(visited)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  nw_visited = visited
  nw_visited(i) = .true.

  ! core
  do j = 1, size(graph_conn,1)
    if (nw_visited(j)) cycle

    if (graph_conn(i,j)) then
      found_conn = .true.
      write(i_str,'(I8)') i
      i_str = adjustl(i_str)
      if (j == f) then
        write(f_str,'(I8)') f
        f_str = adjustl(f_str)
        call add_graph_pathstrings(                   &
          out_str//" "//trim(i_str)//" "//trim(f_str) &
        )
        paths_found = paths_found + 1
      else
        call priv_find_graph_paths_std(j,f,nw_visited, &
          out_str//" "//trim(i_str))
      end if
    end if
  end do

  if (found_conn.eqv..false.) dead_paths = dead_paths + 1

  ! deallocation
  deallocate(nw_visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine priv_find_graph_paths_std

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive subroutine priv_find_graph_paths_grp(i,f,visited,grp_visited,out_str)

  integer, intent(in) :: i
  integer, intent(in) :: f
  logical, dimension(:), intent(in) :: visited
  logical, dimension(:), intent(in) :: grp_visited
  character(*), intent(in) :: out_str
  character(*), parameter :: my_name = "priv_find_graph_paths_grp"
  integer :: j
  integer :: indx1
  integer :: indx2
  logical, dimension(:), allocatable :: nw_visited
  logical, dimension(:), allocatable :: nw_grp_visited
  character(8) :: i_str
  character(8) :: f_str
  logical :: found_conn
  integer :: err_n
  character(120) :: err_msg

  ! assumption:
  ! flag_graph_grouplist == .true.

  ! set working variables
  found_conn = .false.

  allocate(nw_visited(size(visited)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  nw_visited = visited
  nw_visited(i) = .true.

  allocate(nw_grp_visited(size(graph_unique_groups)), &
    stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! core
  do j = 1, size(graph_conn,1)
    if (nw_visited(j)) cycle

    indx1 = get_graph_unique_groups_index(graph_grouplist(j))
    if (grp_visited(indx1)) cycle

    if (graph_conn(i,j)) then
      found_conn = .true.
      write(i_str,'(I8)') i
      i_str = adjustl(i_str)
      if (j == f) then
        write(f_str,'(I8)') f
        f_str = adjustl(f_str)
        call add_graph_pathstrings(                   &
          out_str//" "//trim(i_str)//" "//trim(f_str) &
        )
        paths_found = paths_found + 1
      else
        nw_grp_visited = grp_visited
        indx1 = get_graph_unique_groups_index(graph_grouplist(j))
        indx2 = get_graph_unique_groups_index(graph_grouplist(i))
        if (indx1 /= indx2) nw_grp_visited(indx2) = .true.
        call priv_find_graph_paths_grp(j,f,nw_visited, &
          nw_grp_visited,out_str//" "//trim(i_str))
      end if
    end if
  end do

  if (found_conn.eqv..false.) dead_paths = dead_paths + 1

  ! deallocation
  deallocate(nw_visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(nw_grp_visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine priv_find_graph_paths_grp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive subroutine priv_find_graph_paths_from_groups(i,f,grp_visited,out_str)

  integer, intent(in) :: i
  integer, intent(in) :: f
  logical, dimension(:), intent(in) :: grp_visited
  character(*), intent(in) :: out_str
  character(*), parameter :: my_name = "priv_find_graph_paths_from_groups"
  integer :: j
  logical, dimension(:), allocatable :: nw_grp_visited
  character(8) :: i_str
  character(8) :: f_str
  logical :: found_conn
  integer :: err_n
  character(120) :: err_msg

  ! set working variables
  found_conn = .false.

  allocate(nw_grp_visited(size(grp_visited)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  nw_grp_visited = grp_visited
  nw_grp_visited(i) = .true.

  ! core
  do j = 1, size(graph_conn_grp,1)
    if (nw_grp_visited(j)) cycle

    if (graph_conn_grp(i,j)) then
      found_conn = .true.
      write(i_str,'(I8)') i
      i_str = adjustl(i_str)
      if (j == f) then
        write(f_str,'(I8)') f
        f_str = adjustl(f_str)
        call priv_find_graph_paths_on_groups(         &
          out_str//" "//trim(i_str)//" "//trim(f_str) &
        )
        paths_found = paths_found + 1
      else
        call priv_find_graph_paths_from_groups(j,f,nw_grp_visited, &
          out_str//" "//trim(i_str))
      end if
    end if
  end do

  if (found_conn.eqv..false.) dead_paths = dead_paths + 1

  ! deallocation
  deallocate(nw_grp_visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine priv_find_graph_paths_from_groups

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine priv_find_graph_paths_on_groups(pathstr)

  character(*), intent(in) :: pathstr
  character(*), parameter :: my_name = "priv_find_graph_paths_on_groups"
  logical, dimension(:), allocatable :: visited
  logical, dimension(:), allocatable :: grp_visited
  integer, dimension(:), allocatable :: grp_path
  logical, dimension(:,:), allocatable :: holy_graph_conn
  integer :: g1
  integer :: g2
  integer :: i
  integer :: j
  integer :: k
  integer :: n
  integer :: indx1
  integer :: indx2
  character(20) :: field
  integer :: err_n
  character(120) :: err_msg

  allocate(visited(size(graph_conn,1)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  visited = .false.

  allocate(grp_visited(size(graph_unique_groups)),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  grp_visited = .false.

  ! save the original graph_conn
  n = size(graph_conn,1)
  allocate(holy_graph_conn(n,n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  holy_graph_conn = graph_conn

  ! allocate array with path on groups
  n = count_fields(pathstr)
  allocate(grp_path(n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  do i = 1, n
    call get_field(pathstr,field,i,err_n,err_msg)

    read(field,*,iostat=err_n,iomsg=err_msg) j
    if (err_n /= 0) call error(my_name,err_msg)
    grp_path(i) = j
  end do

  graph_conn = .false.
  do k = 1, n-1
    g1 = grp_path(k)
    g2 = grp_path(k+1)
    do i = 1, size(graph_conn,1)
      do j = 1, size(graph_conn,1)
        indx1 = get_graph_unique_groups_index(graph_grouplist(i))
        indx2 = get_graph_unique_groups_index(graph_grouplist(j))
        if (((indx1 == g1).or.(indx1 == g2)).and. &
            ((indx2 == g1).or.(indx2 == g2))) then
          graph_conn(i,j) = holy_graph_conn(i,j)
        end if
      end do
    end do
  end do

  call priv_find_graph_paths_grp(start_vert,end_vert,visited,grp_visited,"")

  graph_conn = holy_graph_conn

  ! deallocation
  deallocate(visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(grp_visited,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(holy_graph_conn,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  deallocate(grp_path,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine priv_find_graph_paths_on_groups

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine add_graph_pathstrings(pathstr)

  character(*), intent(in) :: pathstr

  if (graph_pathstrings_index + 1 > graph_pathstrings_len) then
    call flush_graph_pathstrings()
    graph_pathstrings_index = 1
  else
    graph_pathstrings_index = graph_pathstrings_index + 1
  end if

  graph_pathstrings(graph_pathstrings_index) = pathstr

end subroutine add_graph_pathstrings

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

subroutine open_graph_fileout()

  character(*), parameter :: my_name = "open_graph_fileout"
  integer :: err_n
  character(120) :: err_msg

  open(unit=graph_fileout,file=graph_fileout_name, &
    status="replace",action="write",iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine open_graph_fileout

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine close_graph_fileout()

  character(*), parameter :: my_name = "close_graph_fileout"
  integer :: err_n
  character(120) :: err_msg

  close(unit=graph_fileout,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine close_graph_fileout

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine flush_graph_pathstrings()

  integer :: i

  do i = 1, graph_pathstrings_index
    write(graph_fileout,*) trim(graph_pathstrings(i))
  end do

end subroutine flush_graph_pathstrings

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_graph
