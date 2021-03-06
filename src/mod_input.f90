module mod_input

  use mod_error
  use mod_graph
  use mod_get_field

  implicit none
  save
  private

  public :: read_input

contains

!!! Public !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine read_input(fname)

  ! Input file structure:
  !
  ! graphtype <directed|undirected>
  !
  ! nodetype  <individual|group>
  !
  ! nodenumber <n>          | set a graph with n (> 0) nodes.
  !                         | can be used only with "nodetype individual".
  !                         | mutually exclusive with "nodelist".
  !
  ! nodelist                | can be used with both types of "nodetype".
  !                         | mutually exclusive with "nodenumber".
  !                         | list of nodes in the form:
  !   nodelabel [nodegroup] | nodegroup only required with "nodetype group".
  !   ...                   | list all nodes
  ! endnodelist
  !
  ! edgelist                | list all edges between two nodes
  !   n1 n2                 | edge connecting node n1 to node n2.
  !                         | n1 and n2 must be:
  !                         |   * integers if nodenumber was used
  !                         |   * the appropriate label if nodelist was used
  !   ...                   | list all edges
  ! endedgelist
  !
  ! fromto n1 n2            | search all paths from node n1 to n2.
  !
  ! search_from_groups      | (optional) if this keyword is encountered,
  !                         | then the search of paths will start from groups

  character(*), intent(in) :: fname
  character(*), parameter :: my_name = "read_input"
  integer, parameter :: fnumb = 600
  character(200) :: buff
  character(20)  :: keyword
  character(100) :: arg
  integer :: node_n
  logical :: flag_error
  logical :: flag_graphtype
  logical :: flag_nodetype
  logical :: flag_nodenumber
  logical :: flag_nodelist
  logical :: flag_edgelist
  logical :: flag_fromto
  logical :: flag_sfg
  integer :: err_n
  character(120) :: err_msg

  flag_error      = .false.
  flag_graphtype  = .false.
  flag_nodetype   = .false.
  flag_nodenumber = .false.
  flag_nodelist   = .false.
  flag_edgelist   = .false.
  flag_fromto     = .false.
  flag_sfg        = .false.

  ! open input file
  open(unit=fnumb,file=fname,status="old",action="read",&
    iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! read input
  do
    read(fnumb,'(A200)',iostat=err_n,iomsg=err_msg) buff
    if (err_n /= 0) exit

    if (len_trim(buff) == 0) cycle
    buff = adjustl(buff)
    if (buff(1:1) == "#") cycle

    call get_field(buff,keyword,1,err_n,err_msg)
    if (err_n /= 0) call error(my_name,err_msg)

    select case (keyword)
    case ("graphtype")
      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n /= 0) then
        call error(my_name,"missing argument of "//trim(keyword))
      end if
      call set_graphtype(arg)
      flag_graphtype = .true.
    case ("nodetype")
      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n /= 0) then
        call error(my_name,"missing argument of "//trim(keyword))
      end if
      call set_nodetype(arg)
      flag_nodetype = .true.
    case ("nodenumber")
      if (flag_nodelist) then
        call error(my_name, &
          "keywords "//trim(keyword)//" and nodelist are mutually exclusive")
      end if

      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n /= 0) then
        call error(my_name,"missing argument of "//trim(keyword))
      end if

      read(arg,*,iostat=err_n,iomsg=err_msg) node_n
      if (err_n /= 0) then
        call error(my_name,err_msg)
      end if

      call set_graph_nodes(node_n)
      flag_nodenumber = .true.
    case ("nodelist")
      if (.not.flag_nodetype) then
        call error(my_name,"nodetype must be specified before "// &
          trim(keyword))
      end if
      if (flag_nodenumber) then
        call error(my_name, &
          "keywords "//trim(keyword)//" and nodenumber are mutually exclusive")
      end if

      call get_nodelist(fnumb)
      flag_nodelist = .true.
    case ("edgelist")
      if (.not.flag_graphtype) then
        call error(my_name,"graphtype must be specified before "// &
          trim(keyword))
      end if
      if (.not.(flag_nodenumber.or.flag_nodelist)) then
        call error(my_name, &
          "nodenumber or nodelist must be specified before "//trim(keyword))
      end if

      call get_edgelist(fnumb)
      flag_edgelist = .true.
    case ("fromto")
      if (.not.(flag_nodenumber.or.flag_nodelist)) then
        call error(my_name, &
          "nodenumber or nodelist must be specified before "//trim(keyword))
      end if

      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n /= 0) then
        call error(my_name,"missing first argument of "//trim(keyword))
      end if
      call set_fromto_vert("from",arg)

      call get_field(buff,arg,3,err_n,err_msg)
      if (err_n /= 0) then
        call error(my_name,"missing second argument of "//trim(keyword))
      end if
      call set_fromto_vert("to",arg)
      flag_fromto = .true.
    ! select case - optional keywords
    case ("search_from_groups")
      flag_sfg = .true.
    case default
      call error(my_name,"invalid keyword "//trim(keyword))
    end select
  end do

  ! check input
  if (.not.flag_graphtype) then
    write(*,*) my_name,"mandatory keyword graphtype", &
      " was not specified in input file"
    flag_error = .true.
  end if
  if (.not.flag_nodetype) then
    write(*,*) my_name,"mandatory keyword nodetype", &
      " was not specified in input file"
    flag_error = .true.
  end if
  if (.not.(flag_nodenumber.or.flag_nodelist)) then
    write(*,*) my_name,"one of the mandatory keyword nodenumber or nodelist", &
      " was not specified in input file"
    flag_error = .true.
  end if
  if (.not.flag_edgelist) then
    write(*,*) my_name,"mandatory keyword edgelist", &
      " was not specified in input file"
    flag_error = .true.
  end if
  if (.not.flag_fromto) then
    write(*,*) my_name,"mandatory keyword fromto", &
      " was not specified in input file"
    flag_error = .true.
  end if

  ! optional keywords
  if (flag_sfg) call set_search_from_groups()

  ! close input file
  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! exit on error
  if (flag_error) call error(my_name,"missing mandatory keyword(s)")

end subroutine read_input

!!! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_nodelist(fnumb)

  integer, intent(in) :: fnumb
  character(*), parameter :: my_name = "get_nodelist"
  character(16), dimension(:), allocatable :: nodelist
  character(16), dimension(:), allocatable :: grouplist
  integer :: lines
  integer :: i
  character(50) :: buff
  character(16) :: arg
  logical :: flag_group
  integer :: err_n
  character(120) :: err_msg

  lines = read_lines(fnumb,"endnodelist")
  if (lines == 0) then
    call error(my_name,"no node specified")
  end if

  ! allocation section
  allocate(nodelist(lines),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  if (nodetype == "group") then
    flag_group = .true.
    allocate(grouplist(lines),stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  else
    flag_group = .false.
  end if

  ! read nodes and groups
  do i = 1, lines
    read(fnumb,'(A50)',iostat=err_n,iomsg=err_msg) buff
    if (err_n /= 0) call error(my_name,err_msg)

    call get_field(buff,arg,1,err_n,err_msg)
    if (err_n /= 0) then
      call error(my_name,"cannot read node name from line: "//trim(buff))
    end if
    nodelist(i) = arg

    if (flag_group) then
      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n /= 0) then
        call error(my_name,"cannot read group from line: "//trim(buff))
      end if
      grouplist(i) = arg
    end if
  end do

  read(fnumb,'(A200)') buff

  ! set nodelist and grouplist
  call set_graph_nodes(size(nodelist))
  call set_graph_nodelist(nodelist)
  if (flag_group) then
    call set_graph_grouplist(grouplist)
  end if

  ! deallocation
  deallocate(nodelist,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)
  if (flag_group) then
    deallocate(grouplist,stat=err_n,errmsg=err_msg)
    if (err_n /= 0) call error(my_name,err_msg)
  end if

end subroutine get_nodelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_edgelist(fnumb)

  integer, intent(in) :: fnumb
  character(*), parameter :: my_name = "get_edgelist"
  logical, dimension(:,:), allocatable :: cm
  integer :: lines
  integer :: i
  integer :: j
  integer :: i1
  integer :: i2
  integer :: cm_n
  character(50) :: buff
  character(16) :: n1
  character(16) :: n2
  logical :: flag_undirected
  integer :: err_n
  character(120) :: err_msg

  if (graphtype == "undirected") then
    flag_undirected = .true.
  else
    flag_undirected = .false.
  end if

  lines = read_lines(fnumb,"endedgelist")

  ! allocation
  if (allocated(graph_nodelist)) then
    cm_n = size(graph_nodelist)
  else
    cm_n = graph_nodes
  end if

  allocate(cm(cm_n,cm_n),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  cm = .false.

  ! read edges
  if (allocated(graph_nodelist)) then
    do i = 1, lines
      read(fnumb,'(A50)',iostat=err_n,iomsg=err_msg) buff
      if (err_n /= 0) call error(my_name,err_msg)

      call get_field(buff,n1,1,err_n,err_msg)
      if (err_n /= 0) call error(my_name,err_msg)
      call get_field(buff,n2,2,err_n,err_msg)
      if (err_n /= 0) call error(my_name,err_msg)

      i1 = -1
      i2 = -1
      do j = 1, cm_n
        if (graph_nodelist(j) == n1) i1 = j
        if (graph_nodelist(j) == n2) i2 = j
      end do

      if (i1 < 0) then
        call error(my_name,"unknow node name 1 in line: "//trim(buff))
      else if (i2 < 0) then
        call error(my_name,"unknow node name 2 in line: "//trim(buff))
      end if

      cm(i1,i2) = .true.
      if (flag_undirected) cm(i2,i1) = .true.
    end do
  else
    do i = 1, lines
      read(fnumb,*,iostat=err_n,iomsg=err_msg) i1, i2
      if (err_n /= 0) call error(my_name,err_msg)

      if ((i1 < 0).or.(i1 > graph_nodes)) then
        call error(my_name,"node 1 out of range in line:"//trim(buff))
      else if ((i2 < 0).or.(i2 > graph_nodes)) then
        call error(my_name,"node 2 out of range in line:"//trim(buff))
      end if

      cm(i1,i2) = .true.
      if (flag_undirected) cm(i2,i1) = .true.
    end do
  end if

  read(fnumb,'(A200)') buff

  ! set edges
  call init_graph_conn(cm)

  ! deallocation
  deallocate(cm,stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

end subroutine get_edgelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer function read_lines(fnumb,str)

  integer, intent(in) :: fnumb
  character(*), intent(in) :: str
  character(*), parameter :: my_name = "read_lines"
  character(200) :: buff
  integer :: i
  integer :: err_n

  i = 0
  do
    read(fnumb,'(A200)',iostat=err_n) buff
    if (err_n /= 0) call error(my_name,"string "//trim(str)//" not found")

    if (buff == str) exit

    i = i+1
  end do

  read_lines = i

  do i = 1, read_lines+1
    backspace(unit=fnumb,iostat=err_n)
    if (err_n /= 0) call error(my_name," error during backspace")
  end do

end function read_lines

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_input
