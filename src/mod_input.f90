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
  character :: ch
  integer :: node_n
  integer :: vert_n
  integer :: edge_n
  integer :: i
  integer :: a
  integer :: b
  integer :: start_vert
  integer :: end_vert
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
    if (err_n == 0) call error(my_name,err_msg)

    select case (keyword)
    case ("graphtype")
      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n == 0) then
        call error(my_name,"missing argument of "//trim(keywork))
      end if
      call set_graphtype(arg)
      flag_graphtype = .true.
    case ("nodetype")
      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n == 0) then
        call error(my_name,"missing argument of "//trim(keywork))
      end if
      call set_nodetype(arg)
      flag_nodetype = .true.
    case ("nodenumber")
      if (flag_nodelist) then
        call error(my_name, &
          "keywords "//trim(keyword)//" and nodelist are mutually exclusive")
      end if

      call get_field(buff,node_n,2,err_n,err_msg)
      if (err_n == 0) then
        call error(my_name,"missing argument of "//trim(keywork))
      end if
      flag_nodenumber = .true.
    case ("nodelist")
      if (flag_nodenumber) then
        call error(my_name, &
          "keywords "//trim(keyword)//" and nodenumber are mutually exclusive")
      end if

      call get_nodelist(fnumb,nodelist,grouplist)
      flag_nodelist = .true.
    case ("edgelist")
      if (.not.(flag_nodenumber.or.flag_nodelist)) then
        call error(my_name, &
          "nodenumber or nodelist must be specified before "//trim(keyword))
      end if

      call get_edgelist(fnumb,edgelist)
      flag_edgelist = .true.
    case ("fromto")
      if (.not.(flag_nodenumber.or.flag_nodelist)) then
        call error(my_name, &
          "nodenumber or nodelist must be specified before "//trim(keyword))
      end if

      call get_field(buff,arg,2,err_n,err_msg)
      if (err_n == 0) then
        call error(my_name,"missing first argument of "//trim(keywork))
      end if
      call set_start_vert(arg)

      call get_field(buff,arg,3,err_n,err_msg)
      if (err_n == 0) then
        call error(my_name,"missing second argument of "//trim(keywork))
      end if
      call set_end_vert(arg)
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

end subroutine read_input

!!! Private !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_nodelist(fnumb,nodelist,grouplist)

  integer, intent(in) :: fnumb
  character(*), dimension(:), allocatable, intent(out) :: nodelist
  character(*), dimension(:), allocatable, intent(out) :: grouplist

end subroutine get_nodelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine get_edgelist(fnumb,edgelist)

  integer, intent(in) :: fnumb
  character(*), dimension(:), allocatable, intent(out) :: edgelist

end subroutine get_edgelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mod_input
