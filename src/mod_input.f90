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

  character(*), intent(in) :: fname
  character(*), parameter :: my_name = "read_input"
  integer, parameter :: fnumb = 600
  character(200) :: buff
  character(20)  :: keyword
  character(100) :: arg
  character(16), dimension(:), allocatable :: nodelist
  character(16), dimension(:), allocatable :: grouplist
  integer :: node_n
  logical, dimension(:,:), allocatable :: graph_conn
  logical :: flag_error
  logical :: flag_graphtype
  logical :: flag_nodetype
  logical :: flag_nodenumber
  logical :: flag_nodelist
  logical :: flag_edgelist
  logical :: flag_fromto
  integer :: err_n
  character(120) :: err_msg

  flag_error      = .false.
  flag_graphtype  = .false.
  flag_nodetype   = .false.
  flag_nodenumber = .false.
  flag_nodelist   = .false.
  flag_edgelist   = .false.
  flag_fromto     = .false.

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

      call get_nodelist(fnumb,nodelist,grouplist)
      call set_graph_nodes(size(nodelist))
      call set_graph_nodelist(nodelist)
      deallocate(nodelist,stat=err_n,errmsg=err_msg)
      if (err_n/= 0) call error(my_name,err_msg)
      if (allocated(grouplist)) then
        call set_graph_grouplist(grouplist)
        deallocate(grouplist,stat=err_n,errmsg=err_msg)
        if (err_n/= 0) call error(my_name,err_msg)
      end if
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

      call get_edgelist(fnumb,graph_conn)
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
!      call set_start_vert(arg)

      call get_field(buff,arg,3,err_n,err_msg)
      if (err_n /= 0) then
        call error(my_name,"missing second argument of "//trim(keyword))
      end if
!      call set_end_vert(arg)
      flag_fromto = .true.
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

  ! close input file
  close(unit=fnumb,iostat=err_n,iomsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  ! exit on error
  if (flag_error) call error(my_name,"missing mandatory keyword(s)")

  !@@@
  stop 42
  !@@@

end subroutine read_input

!!! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_nodelist(fnumb,nodelist,grouplist)

  integer, intent(in) :: fnumb
  character(*), dimension(:), allocatable, intent(out) :: nodelist
  character(*), dimension(:), allocatable, intent(out) :: grouplist
  character(*), parameter :: my_name = "get_nodelist"
  integer :: lines
  integer :: i
  character(200) :: buff
  character(100) :: arg
  logical :: flag_group
  integer :: err_n
  character(120) :: err_msg

  lines = read_lines(fnumb,"endnodelist")

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
    read(fnumb,'(A200)',iostat=err_n,iomsg=err_msg) buff
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

end subroutine get_nodelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_edgelist(fnumb,cm)

  integer, intent(in) :: fnumb
  logical, dimension(:,:), allocatable, intent(out) :: cm
  character(*), parameter :: my_name = "get_edgelist"
  integer :: lines
  integer :: i
  character(200) :: buff
  integer :: err_n
  character(120) :: err_msg

  lines = read_lines(fnumb,"endedgelist")

  allocate(cm(lines,lines),stat=err_n,errmsg=err_msg)
  if (err_n /= 0) call error(my_name,err_msg)

  do i = 1, lines
    read(fnumb,'(A200)') buff
  end do

  read(fnumb,'(A200)') buff

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
