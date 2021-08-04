!> @file
!> @brief Fortran object that wraps the concept of a string and
!>        provides a few useful methods.
!>
!> @author Ronald Mclaren
!> @date 2021-05-11
!>

!> @brief A string object
!>
!> @author Ronald Mclaren
!> @date 2021-05-11
!>

module modq_string
  type String
    character(len=:), allocatable :: char_buffer

  contains
    procedure :: chars => string__get_chars
    procedure :: print => string__print
    procedure :: append => string__append
    procedure, pass(self) :: string__copy
!    generic, public :: assignment(=) => string__copy
    procedure :: string__equals
    generic, public :: operator(==) => string__equals
    final :: string__delete
  end type String

  interface String
    module procedure initialize_string_chars
  end interface String

contains

  !> @author Ronald Mclaren
  !> @date 2021-05-24
  !>
  !> @brief Initialize a string with a given character buffer
  !> @param[in] chars - character(*): The character buffer to initialize the string with
  !> @return str - type(String): The initialized string
  !>
  type(String) function initialize_string_chars(chars) result(str)
    character(len=*), intent(in) :: chars
    str = String()
    allocate(str%char_buffer, source=chars)
  end function initialize_string_chars


  !> @author Ronald Mclaren
  !> @date 2021-05-24
  !>
  !> @brief Get the character buffer of the string
  !> @param[in] self - class(String): The string instance
  !> @return chars - character(:): The character buffer
  !>
  function string__get_chars(self) result(chars)
    class(String), intent(in) :: self
    character(len=:), allocatable :: chars

    chars = self%char_buffer
  end function string__get_chars


  !> @author Ronald Mclaren
  !> @date 2021-05-26
  !>
  !> @brief Print the string to stdout
  !> @param[in] self - class(String): The string instance
  !>
  subroutine string__print(self)
    class(String), intent(in) :: self
    print *, self%char_buffer
  end subroutine


  !> @author Ronald Mclaren
  !> @date 2021-06-11
  !>
  !> @brief Append a string to this string
  !> @param[in] self - class(String): The string instance
  !> @param[in] str - type(String): The string to append
  !>
  subroutine string__append(self, str)
    class(String), intent(inout) :: self
    type(String), intent(in) :: str

    character(len=:), allocatable :: tmp_char_buffer
    integer :: new_length

    new_length = len(self%char_buffer) + len(str%chars())
    allocate(character(len=new_length) :: tmp_char_buffer)
    tmp_char_buffer(1:len(self%char_buffer)) = self%char_buffer(1:len(self%char_buffer))
    tmp_char_buffer(len(self%char_buffer) + 1 : len(tmp_char_buffer)) = str%chars()
    deallocate(self%char_buffer)
    call move_alloc(tmp_char_buffer, self%char_buffer)
  end subroutine string__append

  !> @author Ronald Mclaren
  !> @date 2021-05-26
  !>
  !> @brief Check if strings are equal
  !> @param[in] self - class(String): The string instance
  !> @param[in] other - type(String): The string to compare to
  !> @return equal - logical: True if the strings are equal, false otherwise
  !>
  logical function string__equals(self, other) result(are_equal)
    class(String), intent(in) :: self
    class(String), intent(in) :: other

    are_equal = (self%char_buffer == other%char_buffer)
  end function string__equals

  !> @author Ronald Mclaren
  !> @date 2021-05-26
  !>
  !> @brief Copy a string
  !> @param[in] self - class(String): The string instance
  !> @param[in] other - type(String): The string to copy from
  !>
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


  !> @author Ronald Mclaren
  !> @date 2021-06-04
  !>
  !> @brief Delete this string
  !> @param[in] self - class(String): The string instance
  !>
  subroutine string__delete(self)
    type(String), intent(inout) :: self

    if (allocated(self%char_buffer)) then
      deallocate(self%char_buffer)
    end if
  end subroutine
end module modq_string
