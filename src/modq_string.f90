module modq_string
  type String
    character(len=:), allocatable :: char_buffer

  contains
    procedure :: chars => string__get_chars
    final :: string__delete
  end type String

  interface String
    module procedure initialize_string_chars
  end interface String

  interface assignment(=)
    procedure string__copy
  end interface

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


  subroutine string__delete(self)
    type(String), intent(inout) :: self
    if (allocated(self%char_buffer)) then
      deallocate(self%char_buffer)
    end if
  end subroutine


  subroutine string__copy(self, other)
    class(String), allocatable, intent(out) :: self
    type(String), intent(in) :: other

    self%char_buffer = other%char_buffer

    if (.not. allocated(self)) then
      allocate(self)
    end if

    allocate(self%char_buffer, source=other%char_buffer)
  end subroutine string__copy
end module modq_string