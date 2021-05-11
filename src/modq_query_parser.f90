
module modq_query_parser
  implicit none

  private
  public::split_query_str

contains
  subroutine split_query_str(query_str, mnemonics, index)
    character, parameter :: PathDelimiter = "/"
    character(len=2), parameter :: SubscriptDelimiters = "[]"

    character(len=*), intent(in) :: query_str
    character(len=10), allocatable, intent(out) :: mnemonics(:)
    integer, intent(out) :: index

    integer(kind=8), allocatable :: slash_positions(:)
    integer(kind=8) :: slash_idx, char_idx
    integer(kind=8) :: num_seqs
    integer(kind=8) :: mnemonic_start_pos, mnemonic_end_pos
    character(len=:), allocatable :: last_element
    integer :: start_subscript, end_subscript

    allocate(slash_positions(count(transfer(query_str, "a", len(query_str)) == PathDelimiter)))
    index = 0

    ! Find the index of the delimiters.
    ! NOTE: Intel version of findloc has a bug, so avoid it
    slash_idx = 1
    do char_idx = 1, len(query_str)
      !ignore optional leading slash
      if (query_str(char_idx:char_idx) == PathDelimiter) then
        slash_positions(slash_idx) = char_idx
        slash_idx = slash_idx + 1
      end if
    end do

    ! Capture the sequence mnemonic strings
    allocate(mnemonics(size(slash_positions)))
    do slash_idx = 1, size(slash_positions) - 1
      mnemonics(slash_idx) = trim(query_str(slash_positions(slash_idx) + 1 : slash_positions(slash_idx + 1) - 1))
    end do

    ! Parse last element
    last_element = query_str(slash_positions(size(slash_positions)) + 1 : len(query_str))

    if (count(transfer(query_str, "a", len(query_str)) == SubscriptDelimiters(1:1)) > 0) then
      if (count(transfer(query_str, "a", len(query_str)) == SubscriptDelimiters(2:2)) /= 1) then
        error stop 'Missing closing paranthesis.'
      end if

      start_subscript = index_of_char(last_element, SubscriptDelimiters(1:1))
      end_subscript = index_of_char(last_element, SubscriptDelimiters(2:2))

      read(last_element(start_subscript + 1:end_subscript - 1), *) index

      mnemonics(size(slash_positions)) = last_element(1:start_subscript - 1)
    else
      if (count(transfer(query_str, "a", len(query_str)) == SubscriptDelimiters(2:2)) /= 1) then
        error stop 'Found unexpected parenthesis.'
      end if

      mnemonics(size(slash_positions)) = last_element
    end if

  end subroutine split_query_str


  function index_of_char(char_str, char) result(char_idx)
    character(len=*), intent(in) :: char_str
    character, intent(in) :: char

    integer :: char_idx
    integer :: idx

    do idx = 1, len(char_str)
      if (char_str(idx:idx) == char) then
        char_idx = idx
        exit
      end if
    end do

  end function index_of_char
end module modq_query_parser