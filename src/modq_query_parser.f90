
module modq_query_parser
  use modq_string
  implicit none

  private
  public::split_into_subqueries
  public::split_query_str

contains
  function split_into_subqueries(query_str) result(subqueries)
    character(len=*), target, intent(in) :: query_str
    Type(String), allocatable :: subqueries(:)

    character(len=2), parameter :: QuerylistDelimiters = "[]"
    character(len=1), parameter :: QuerySplitChar = ","

    character(len=:), allocatable :: working_str
    integer, allocatable :: comma_positions(:)
    integer :: idx, char_idx, comma_idx
    integer :: last_pos

    working_str = trim(adjustl(query_str))

    if (working_str(1:1) == QuerylistDelimiters(1:1)) then
      if (working_str(len(working_str):len(working_str)) /= QuerylistDelimiters(2:2)) then
        call bort("Query Parser: multi query is lacking closing brackets.")
      end if

      working_str = working_str(2:len(working_str) - 1)
      allocate(comma_positions(count(transfer(working_str, "a", len(working_str)) == QuerySplitChar)))
      allocate(subqueries(size(comma_positions) + 1))

      ! Find the index of the commas.
      comma_idx = 1
      do char_idx = 1, len(working_str)
        !ignore optional leading slash
        if (working_str(char_idx:char_idx) == QuerySplitChar) then
          comma_positions(comma_idx) = char_idx
          comma_idx = comma_idx + 1
        end if
      end do

      last_pos = 1
      do idx = 1, size(comma_positions) + 1
        if (idx < size(comma_positions) + 1) then
          subqueries(idx) = String(trim(adjustl(working_str(last_pos:comma_positions(idx)-1))))
          last_pos = comma_positions(idx) + 1
        else
          subqueries(idx) = String(trim(adjustl(working_str(last_pos:len(working_str)))))
        end if
      end do
    else
      ! Not a list of queries, so just return the result
      allocate(subqueries(1))
      subqueries(1) = String(trim(adjustl(working_str)))
    end if
  end function split_into_subqueries


  subroutine split_query_str(query_str, subset, mnemonics, index)
    character(len=*), target, intent(in) :: query_str
    character(len=10), intent(out) :: subset
    character(len=10), allocatable, intent(out) :: mnemonics(:)
    integer, intent(out) :: index

    character, parameter :: PathDelimiter = "/"
    character(len=2), parameter :: SubscriptDelimiters = "[]"

    integer(kind=8), allocatable :: slash_positions(:)
    integer(kind=8) :: slash_idx, char_idx
    character(len=:), pointer :: last_element
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

    ! Capture the subset string
    subset = trim(query_str(1:slash_positions(1) - 1))

    ! Capture the sequence mnemonic strings
    allocate(mnemonics(size(slash_positions)))
    do slash_idx = 1, size(slash_positions) - 1
      mnemonics(slash_idx) = trim(query_str(slash_positions(slash_idx) + 1 : slash_positions(slash_idx + 1) - 1))
    end do

    ! Parse last element
    last_element => query_str(slash_positions(size(slash_positions)) + 1 : len(query_str))

    if (count(transfer(query_str, "a", len(query_str)) == SubscriptDelimiters(1:1)) > 0) then
      if (count(transfer(query_str, "a", len(query_str)) == SubscriptDelimiters(2:2)) /= 1) then
        call bort('Missing closing paranthesis.')
      end if

      start_subscript = index_of_char(last_element, SubscriptDelimiters(1:1))
      end_subscript = index_of_char(last_element, SubscriptDelimiters(2:2))

      read(last_element(start_subscript + 1:end_subscript - 1), *) index

      mnemonics(size(slash_positions)) = last_element(1:start_subscript - 1)
    else
      if (count(transfer(query_str, "a", len(query_str)) == SubscriptDelimiters(2:2)) /= 0) then
        call bort('Found unexpected parenthesis.')
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
