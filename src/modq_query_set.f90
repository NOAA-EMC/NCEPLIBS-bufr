module modq_query_set
  use modq_string
  implicit none

  type, public :: QuerySet
    private
      type(String), allocatable :: names(:)
      type(String), allocatable :: query_strs(:)

    contains
      procedure :: add => query_set__add
      procedure :: count => query_set__count
      procedure :: get_query_name => query_set__get_query_name
      procedure :: get_query_str => query_set__get_query_str
      procedure :: print => query_set__print
      final :: query_set__delete
  end type QuerySet

contains

  subroutine query_set__add(self, query_str, query_name)
    class(QuerySet), intent(inout) :: self
    character(len=*), intent(in) :: query_str
    character(len=*), intent(in) ::  query_name

    if (.not. allocated(self%names)) allocate(self%names(0))
    if (.not. allocated(self%query_strs)) allocate(self%query_strs(0))

    self%names = [self%names, String(query_name)]
    self%query_strs = [self%query_strs, String(query_str)]
  end subroutine query_set__add


  integer function query_set__count(self) result(count)
    class(QuerySet), intent(in) :: self
    count = size(self%names)
  end function query_set__count


  function query_set__get_query_name(self, idx) result(name)
    class(QuerySet), intent(inout) :: self
    integer, intent(in) :: idx
    character(len=:), allocatable :: name
    name = self%names(idx)%chars()
  end function query_set__get_query_name


  function query_set__get_query_str(self, idx) result(query_str)
    class(QuerySet), intent(inout) :: self
    integer, intent(in) :: idx
    character(len=:), allocatable :: query_str

    query_str = self%query_strs(idx)%chars()
  end function query_set__get_query_str


  subroutine query_set__print(self)
    class(QuerySet), intent(inout) :: self

    integer :: q_idx

    do q_idx = 1, size(self%names)
      print *, self%query_strs(q_idx)%chars(), " ", self%names(q_idx)%chars()
    end do
  end subroutine query_set__print


  subroutine query_set__delete(self)
    type(QuerySet), intent(inout) :: self

    if (allocated(self%names)) then
      deallocate(self%names)
    end if

    if (allocated(self%query_strs)) then
      deallocate(self%query_strs)
    end if

  end subroutine query_set__delete

end module modq_query_set