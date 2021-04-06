module mod_query
  use moda_tables
  use moda_msgcwd

  implicit none

  private
  public::query

contains
  subroutine query(lunit, query_str, data_ptr)
    integer, intent(in) :: lunit
    character(len=*), intent(in) :: query_str
    real, pointer, intent(out) :: data_ptr

    integer :: lun
    integer :: il, im
    integer :: query_node_idx
    character(len=8), allocatable :: mnemonics(:)

    call status(lunit, lun, il, im)

    call split_query_str(query_str, mnemonics)
    query_node_idx = find_indicies(lun, inode(lun) + 1, mnemonics, 1)

    deallocate(mnemonics)

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


  recursive function find_indicies(lun, current_node_idx, mnemonics, mnemonic_idx) result(result_idx)
    integer, intent(in) :: lun
    integer, intent(in) :: current_node_idx
    character(len=8), intent(in) :: mnemonics(:)
    integer, intent(in) :: mnemonic_idx

    integer :: result_idx
    integer :: find_result
    integer :: node_idx

    result_idx = -1
    node_idx = current_node_idx
    do while(node_idx > 0 .and. mnemonic_idx <= size(mnemonics))
      if (tag(node_idx) == mnemonics(mnemonic_idx)) then
        if (has_children(lun, node_idx)) then
          print *, '> ', tag(node_idx)
          result_idx = find_indicies(lun, node_idx + 1, mnemonics, mnemonic_idx + 1)
          goto 10
        else
          print *, '** ', tag(node_idx)
          result_idx = node_idx
          goto 10
        end if
      else
        print *, tag(node_idx)
        node_idx = next_node(node_idx) ! move to next node
      end if
    end do

    10 continue !break
  end function


  integer function next_node(node_idx)
    integer, intent(in) :: node_idx

    next_node = link(node_idx)

    if (typ(next_node) == 'DRP') then  ! if its a delayed replication cnt node
      next_node = next_node + 1  ! skip delayed rep count node
    end if
  end function


  logical function has_children(lun, node_idx)
    integer, intent(in) :: lun
    integer, intent(in) :: node_idx

    has_children = .false.
    if (typ(node_idx) == 'RPC' .or. typ(node_idx) == 'SEQ' .or. typ(node_idx) == 'SUB') then
      has_children = .true.
    end if
  end function has_children


!  function get_parent(node_idx) result(parent_idx)
!    integer(kind=8), intent(in) :: node_idx
!    integer(kind=8) :: parent_idx
!
!  end function get_parent



  subroutine count_repeats(seq_mnemonics)
    character(len=8), intent(in) :: seq_mnemonics(*)
  end subroutine count_repeats


  recursive subroutine count_repeats_of_seq(seq_mnemonic)
    character(len=8), intent(in) :: seq_mnemonic

    !    JMPB(NODE)
  end subroutine count_repeats_of_seq

end module mod_query