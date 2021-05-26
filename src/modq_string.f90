module modq_string
  type String
    character(len=:), allocatable :: char_buffer

  contains
    procedure :: chars => string__get_chars
    procedure :: print => string__print
    procedure, pass(self) :: string__copy
    generic, public :: assignment(=) => string__copy
    procedure :: string__equals
    generic, public :: operator(==) => string__equals
    final :: string__delete
  end type String

  interface String
    module procedure initialize_string_chars
  end interface String

contains

  type(String) function initialize_string_chars(chars) result(str)
    character(len=*), intent(in) :: chars
    str = String()
    allocate(str%char_buffer, source=chars)
  end function initialize_string_chars


  function string__get_chars(self) result(chars)
    class(String), intent(in) :: self
    character(len=:), allocatable :: chars

    chars = self%char_buffer
  end function string__get_chars


  subroutine string__print(self)
    class(String), intent(in) :: self
    print *, self%char_buffer
  end subroutine


  subroutine string__delete(self)
    type(String), intent(inout) :: self
    if (allocated(self%char_buffer)) then
      deallocate(self%char_buffer)
    end if
  end subroutine


  logical function string__equals(self, other) result(are_equal)
    class(String), intent(in) :: self
    class(String), intent(in) :: other

    are_equal = (self%char_buffer == other%char_buffer)
  end function string__equals


  subroutine string__copy(self, other)
    class(String), intent(inout) :: self
    class(String), intent(in) :: other

    if ( allocated(self%char_buffer)) then
      deallocate(self%char_buffer)
    end if

    if ( allocated(other%char_buffer)) then
      allocate(self%char_buffer, source=other%char_buffer)
    end if
  end subroutine string__copy
end module modq_string