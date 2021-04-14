module mod_query
  use moda_tables
  use moda_msgcwd
  use moda_usrint

  implicit none

  character(len=3), parameter :: Subset = 'SUB'
  character(len=3), parameter :: DelayedRep = 'DRP'
  character(len=3), parameter :: Sequence = 'SEQ'
  character(len=3), parameter :: FixedRep = 'REP'

  private
  public::query

contains
  subroutine query(lunit, query_str, data_ptr)
    integer, intent(in) :: lunit
    character(len=*), intent(in) :: query_str
    real(kind=8), pointer, intent(out) :: data_ptr(:)

    integer :: lun, il, im

    character(len=8), allocatable :: mnemonics(:)
    integer, allocatable :: table_path(:)
    real(kind=8), target, allocatable :: data(:)

    call status(lunit, lun, il, im)
    call split_query_str(query_str, mnemonics)
    allocate(table_path(size(mnemonics)))
    call find_table_indices(lun, inode(lun) + 1, mnemonics, 1, table_path)
    call collect_data(lun, table_path, data)

    data_ptr => data

    deallocate(mnemonics)
    deallocate(table_path)

  end subroutine query


  subroutine split_query_str(query_str, mnemonics)
    character, parameter :: Delimiter = "/"

    character(len=*), intent(in) :: query_str
    character(len=8), allocatable, intent(out) :: mnemonics(:)

    integer(kind=8), allocatable :: slash_positions(:)
    integer(kind=8) :: slash_idx, char_idx
    integer(kind=8) :: num_seqs
    integer(kind=8) :: mnemonic_start_pos, mnemonic_end_pos

    allocate(slash_positions(count(transfer(query_str, "a", len(query_str)) == Delimiter)))

    ! Find the index of the delimiters.
    ! NOTE: Intel version of findloc has a bug, so avoid it
    slash_idx = 1
    do char_idx = 1, len(query_str)
      !ignore optional leading slash
      if (query_str(char_idx:char_idx) == Delimiter) then
        slash_positions(slash_idx) = char_idx
        slash_idx = slash_idx + 1
      end if
    end do

    ! Capture the sequence mnemonic strings
    allocate(mnemonics(size(slash_positions)))
    do slash_idx = 1, size(slash_positions) - 1
      mnemonics(slash_idx) = trim(query_str(slash_positions(slash_idx) + 1 : slash_positions(slash_idx + 1) - 1))
    end do

    ! Capture the data mnemonic
    mnemonics(size(slash_positions)) = trim(query_str(slash_positions(size(slash_positions)) + 1 : len(query_str)))

  end subroutine split_query_str


  recursive subroutine find_table_indices(lun, current_node_idx, mnemonics, mnemonic_idx, indices)
    integer, intent(in) :: lun
    integer, intent(in) :: current_node_idx
    character(len=8), intent(in) :: mnemonics(:)
    integer, intent(in) :: mnemonic_idx
    integer, intent(inout) :: indices(*)

    integer :: find_result
    integer :: node_idx

    node_idx = current_node_idx
    do while(node_idx > 0 .and. mnemonic_idx <= size(mnemonics))
      if (tag(node_idx) == mnemonics(mnemonic_idx)) then
        if (has_children(lun, node_idx)) then
          indices(mnemonic_idx) = jmpb(node_idx)
          call find_table_indices(lun, node_idx + 1, mnemonics, mnemonic_idx + 1, indices)
        else
          indices(mnemonic_idx) = node_idx
        end if
        goto 10  ! break loop
      else
        node_idx = next_node(node_idx) ! move to next node
      end if
    end do

    10 continue
  end subroutine


  integer function next_node(node_idx)
    integer, intent(in) :: node_idx

    next_node = link(node_idx)

    if (typ(next_node) == 'DRP' .or. typ(next_node) == 'REP') then  ! if its a delayed replication cnt node
      next_node = next_node + 1  ! skip delayed rep count node
    end if
  end function


  subroutine collect_data(lun, table_path, data)
    integer, intent(in) :: lun
    integer, intent(in) :: table_path(:)
    real(kind=8), intent(out), allocatable :: data(:)

    integer :: table_cursor, idx
    integer :: path_cursor, data_cursor, collected_data_cursor

    allocate(data(rep_count(lun, table_path)))

    path_cursor = 0
    collected_data_cursor = 0
    do data_cursor = 1, nval(lun)
      ! Found the target element, record it
      if (path_cursor == size(table_path) - 1 .and. &
        table_path(path_cursor + 1) == inv(data_cursor, lun)) then

        collected_data_cursor = collected_data_cursor + 1
        data(collected_data_cursor) = val(data_cursor, lun)

      ! Found a path sequence, go down the tree
      else if (path_cursor < size(table_path) .and. &
        table_path(path_cursor + 1) == inv(data_cursor, lun)) then

        path_cursor = path_cursor + 1

      ! Found the end of the sequence, go up the tree
      else if (link(table_path(path_cursor)) == inv(data_cursor, lun)) then

        path_cursor = path_cursor - 1
      end if
    end do
  end subroutine


  function rep_count(lun, table_path) result(count)
    integer, intent(in) :: lun
    integer, intent(in) :: table_path(:)

    integer(kind = 8) :: count
    integer :: path_cursor, data_cursor

    count= 0
    path_cursor = 0
    do data_cursor = 1, nval(lun)

      ! Found the target element, count it
      if (path_cursor == size(table_path) - 1 .and. &
        table_path(path_cursor + 1) == inv(data_cursor, lun)) then

        count = count + 1

      ! Found a path sequence, go down the tree
      else if (path_cursor < size(table_path) .and. &
          table_path(path_cursor + 1) == inv(data_cursor, lun)) then

        path_cursor = path_cursor + 1

      ! Found the end of the sequence, go up the tree
      else if (link(table_path(path_cursor)) == inv(data_cursor, lun)) then

        path_cursor = path_cursor - 1

      end if
    end do
  end function


  logical function has_children(lun, node_idx)
    integer, intent(in) :: lun
    integer, intent(in) :: node_idx

    has_children = .false.
    if (typ(node_idx) == 'RPC' .or. typ(node_idx) == 'SEQ' .or. typ(node_idx) == 'SUB') then
      has_children = .true.
    end if
  end function has_children

end module mod_query