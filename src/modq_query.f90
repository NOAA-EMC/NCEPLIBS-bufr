module modq_query
  use moda_tables
  use moda_msgcwd
  use moda_usrint

  use modq_query_set
  use modq_result_set
  use modq_query_parser

  implicit none

  character(len=3), parameter :: Subset = 'SUB'
  character(len=3), parameter :: DelayedRep = 'DRP'
  character(len=3), parameter :: Sequence = 'SEQ'
  character(len=3), parameter :: FixedRep = 'REP'
  character(len=3), parameter :: Value = 'REP'

  type, private :: Target
    character(len=:) :: name
    integer :: node_ids(:)
  end type Target

  private
  public::query

contains

  subroutine query(lunit, query_set, result_set)
    integer, intent(in) :: lunit
    type(QuerySet), intent(in) :: query_set
    type(ResultSet), intent(out) :: result_set

    integer :: lun, il, im
    call status(lunit, lun, il, im)

    call collect_data(lun, find_targets(lun, query_set), result_set)

    deallocate(targets)

  end subroutine query


  function find_targets(lun, query_set) result(targets)
    integer, intent(in) :: lunit
    type(QuerySet), intent(in) :: query_set
    type(Target), allocatable :: targets(:)

    integer :: target_idx
    character(len=:) :: name
    character(len=:) :: query_str

    do target_idx = 1,query_set%count()
      name = query_set%get_query_names(target_idx)
      query_str = query_set%get_query_strs(target_idx)

      targets = [targets, Target(name, find_target_nodes(lun, query_str))]
    end do
  end function


  function find_target_nodes(lun, query_str) result(target_nodes)
    integer, intent(in) :: lun
    character(len=*), intent(in) :: query_str
    integer, allocatable :: target_nodes(:)

    integer :: current_sequence
    integer :: node_idx
    integer :: index
    integer :: table_cursor, mnemonic_cursor
    character(len=10), allocatable :: mnemonics(:)
    integer :: target_idx

    integer, allocatable :: branches(:)

    call split_query_str(query_str, mnemonics, index)

    allocate(branches(size(mnemonics) - 1))
    allocate(target_nodes(0))

    node_idx = 1
    table_cursor = 0
    mnemonic_cursor = 0
    do node_idx = inode(lun), isc(inode(lun))
      if (mnemonic_cursor == size(mnemonics) - 1 .and. &
          table_cursor == mnemonic_cursor .and. &
          tag(node_idx) == mnemonics(size(mnemonics))) then

        ! We found a target
        target_nodes = [target_nodes, node_idx]

      else if (typ(node_idx) == DelayedRep .or. typ(node_idx) == FixedRep) then
        ! Enter the sequence
        if (tag(node_idx + 1) == mnemonics(mnemonic_cursor + 1) .and. &
            table_cursor == mnemonic_cursor) then

          mnemonic_cursor = mnemonic_cursor + 1
          branches(mnemonic_cursor) = node_idx
        end if

        current_sequence = node_idx
        table_cursor = table_cursor + 1

      else if (link(current_sequence) == node_idx) then
        ! Exit sequence
        if (mnemonic_cursor > 0 .and. &
            table_cursor == mnemonic_cursor .and. &
            link(branches(mnemonic_cursor)) == node_idx) then

          mnemonic_cursor = mnemonic_cursor - 1
        end if

        table_cursor = table_cursor - 1
        current_sequence = jmpb(node_idx - 1)
      end if
    end do

    deallocate(mnemonics)

    if (index > 0 .and. index <= size(target_nodes)) then
      if (index > size(target_nodes)) then
        error stop 'Invalid index in query str ' // query_str // '.'
      end if

      target_nodes = [target_nodes(index)]
    end if

    if (size(target_nodes) == 0) then
      error stop 'Could not find the target node for ' // query_str // '.'
    end if
  end function


  subroutine collect_data(lun, targets, result_set)
    integer, intent(in) :: lun
    type(Target), intent(in) :: targets(:)
    type(ResultSet), intent(inout) :: result_set

    integer :: dims(2)
    integer :: col_idx
    integer :: current_sequence
    integer :: data_cursor, path_cursor
    integer, allocatable :: collected_data_cursor(:)
    logical :: target_found

    dims = result_shape(lun, target_nodes)
    allocate(dat(dims(1), dims(2)))

    path_cursor = 0
    current_sequence = 1
    allocate(collected_data_cursor(dims(2)))
    do data_cursor = 1, nval(lun)
      node_idx = inv(data_cursor, lun)
      target_found = .false.

      do col_idx = 1, dims(2)
        ! Found the target element, record it
        if (node_idx == target_nodes(col_idx)) then

          dat(collected_data_cursor(col_idx), col_idx)  = val(data_cursor, lun)
          collected_data_cursor(col_idx) = collected_data_cursor(col_idx) + 1

          target_found = .true.
        end if
      end do

      if (.not. target_found)
        ! Found a path sequence, go down the tree
        if (typ(node_idx) == DelayedRep .or. typ(node_idx) == FixedRep) then

          path_cursor = path_cursor + 1
          current_sequence = node_idx

        ! Found the end of the sequence, go up the tree
        else if (link(current_sequence) == node_idx) then

          path_cursor = path_cursor - 1
          current_sequence = jmpb(node_idx - 1)
        end if
      end if

    end do
  end subroutine


  function result_shape(lun, target_nodes) result(dims)
    integer, intent(in) :: lun
    integer, intent(in) :: target_nodes(:)

    integer :: dims(2)
    integer :: data_cursor

    dims = 0
    do data_cursor = 1, nval(lun)
      if (inv(data_cursor, lun) == target_nodes(1)) then
        dims(1) = dims(1) + 1
      end if
    end do

    dims(2) = size(target_nodes)
  end function

end module modq_query