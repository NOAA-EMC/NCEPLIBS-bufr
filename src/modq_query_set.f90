module modq_query_set
  implicit none

  type, private :: String
    private
      character(len=75) :: char_buffer

    contains
      procedure :: chars => string__get_chars
      procedure :: delete => string__delete
  end type

  type, public :: QuerySet
    private
      type(String), allocatable :: names(:)
      type(String), allocatable :: query_strs(:)

    contains
      procedure :: add => query_set__add
      procedure :: delete => query_set__delete
      procedure :: print => query_set__print

  end type QuerySet

contains

  function string__get_chars(self) result(chars)
    class(String), intent(in) :: self
    character(len=:), allocatable :: chars

    chars = trim(self%char_buffer)
  end function string__get_chars


  subroutine string__delete(self)
    class(String), intent(inout) :: self
  end subroutine


  subroutine query_set__add(self, query_str, query_name)
    class(QuerySet), intent(inout) :: self
    character(len=*), intent(in) :: query_str
    character(len=*), intent(in) ::  query_name

    if (.not. allocated(self%names)) allocate(self%names(0))
    if (.not. allocated(self%query_strs)) allocate(self%query_strs(0))

    self%names = [self%names, String(query_name)]
    self%query_strs = [self%query_strs, String(query_str)]
  end subroutine query_set__add


  subroutine query_set__print(self)
    class(QuerySet), intent(in) :: self

    integer :: q_idx

    do q_idx = 1, size(self%names)
      print *, self%query_strs(q_idx)%chars(), " ", self%names(q_idx)%chars()
    end do
  end subroutine query_set__print


  subroutine query_set__delete(self)
    class(QuerySet), intent(inout) :: self

    deallocate(self%names)
    deallocate(self%query_strs)
  end subroutine query_set__delete

end module modq_query_set