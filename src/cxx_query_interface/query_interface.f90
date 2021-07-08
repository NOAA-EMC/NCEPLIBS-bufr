module cxx_query_interface
  use modq_query_set
  use modq_execute
  use modq_result_set

  use iso_c_binding
  implicit none

  contains

    !Private

    !> @author Ronald McLaren
    !> @date 2020-07-29
    !>
    !> @brief This function turns a c string into a fortran string.
    !>
    !> @param[in] c_str - c_char: pointer to a \0 (null) terminated c string
    !> @param[out] f_str - character(:): fortran string
    !>
    function c_f_string(c_str) result(f_str)
      character(kind=c_char,len=1), intent(in) :: c_str(*)
      character(len=:), allocatable :: f_str
      integer :: nchars

      nchars = 1
      do while (c_str(nchars) /= c_null_char)
        nchars = nchars + 1
      end do
      nchars = nchars - 1

      allocate(character(len=nchars) :: f_str)
      f_str = transfer(c_str(1:nchars), f_str)
    end function c_f_string


    !> @author Ronald McLaren
    !> @date 2020-07-29
    !>
    !> @brief This subroutine copies a fortran string into a c string buffer.
    !>
    !> @param[in] f_str - character(*): fortran string to be copied
    !> @param[inout] c_str - c_char: c pointer to the target buffer
    !> @param[in] c_str_len - integer: length of the c target buffer
    !>
    subroutine copy_f_c_str(f_str, c_str, c_str_len)
      character(len=*), target, intent(in) :: f_str
      character(kind=c_char, len=1), intent(inout) :: c_str(*)
      integer, intent(in) :: c_str_len
      integer :: max_str_len

      if (c_str_len /= 0) then
        max_str_len = min(c_str_len, len_trim(f_str) + 1)
        c_str(1)(1:max_str_len) = f_str(1:max_str_len)
        c_str(1)(max_str_len:max_str_len) = c_null_char
      end if
    end subroutine copy_f_c_str

  subroutine allocate__query_set(query_set_cptr)
    type(c_ptr), intent(inout) :: query_set_cptr
    type(QuerySet), pointer :: query_set_fptr

    allocate(query_set_fptr)
    query_set_cptr = c_loc(query_set_fptr)
  end subroutine allocate__query_set

  subroutine deallocate__query_set(query_set_cptr)
    type(c_ptr), value :: query_set_cptr
    type(QuerySet), pointer :: query_set_fptr

    call c_f_pointer(query_set_cptr, query_set_fptr)
    deallocate(query_set_fptr)
  end subroutine deallocate__query_set


  subroutine allocate__result_set(result_set_cptr)
    type(c_ptr), intent(inout) :: result_set_cptr
    type(ResultSet), pointer :: result_set_fptr

    allocate(result_set_fptr)
    allocate(result_set_fptr%names(0))

    result_set_cptr = c_loc(result_set_fptr)
  end subroutine allocate__result_set


  subroutine deallocate__result_set(result_set_cptr)
    type(c_ptr), value :: result_set_cptr
    type(ResultSet), pointer :: result_set_fptr

    call c_f_pointer(result_set_cptr, result_set_fptr)
    deallocate(result_set_fptr)
  end subroutine deallocate__result_set


  subroutine execute_c(file_unit, query_set, next, result_set)
    integer(c_int), value, intent(in) :: file_unit
    type(QuerySet), intent(in) :: query_set
    integer(c_int), value, intent(in) :: next
    type(ResultSet), intent(inout) :: result_set

    call execute_(file_unit, query_set, result_set, next)
  end subroutine execute_c


  subroutine result_set__get_c(cls, field, for_field, data, data_len) bind(C, name="result_set__get_f")
    type(c_ptr), intent(inout) :: cls
    character(kind=c_char, len=1), intent(in) :: field
    character(kind=c_char, len=1), intent(in) :: for_field
    type(c_ptr), intent(inout) :: data
    integer(kind=c_int), intent(out) :: data_len

    character(len=:), allocatable :: f_field, f_for_field
    real(kind=8), allocatable, target :: data_f(:)
    real(kind=8), pointer :: data_f_ptr(:)

    type(ResultSet), pointer :: result_set_fptr
    call c_f_pointer(cls, result_set_fptr)

    f_field = c_f_string(field)
    f_for_field = c_f_string(for_field)

    data_f = result_set_fptr%get(f_field, f_for_field)

    data_len = size(data_f)
    if (data_len > 0) then
      allocate(data_f_ptr, source=data_f)
      data = c_loc(data_f_ptr(1))
    end if
  end subroutine result_set__get_c

end module cxx_query_interface