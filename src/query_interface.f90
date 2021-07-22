module query_interface
  use bufr_c_interface_mod, only:c_f_string
  use modq_query_set
  use modq_execute
  use modq_result_set

  use iso_c_binding
  implicit none

  contains

 ! Query Set Methods
  subroutine query_set__allocate(query_set_cptr) bind(C, name='query_set__allocate_f')
    type(c_ptr), intent(inout) :: query_set_cptr
    type(QuerySet), pointer :: query_set_fptr

    allocate(query_set_fptr)
    query_set_cptr = c_loc(query_set_fptr)
  end subroutine query_set__allocate

  subroutine query_set__add_c(cls, query_str, query_name) bind(C, name='query_set__add_f')
    type(c_ptr), intent(in) :: cls
    character(kind=c_char, len=1), intent(in) :: query_str
    character(kind=c_char, len=1), intent(in) :: query_name

    type(QuerySet), pointer :: query_set
    call c_f_pointer(cls, query_set)

    call query_set%add(c_f_string(query_str), c_f_string(query_name))
  end subroutine query_set__add_c

  subroutine query_set__print_c(cls) bind(C, name='query_set__print_f')
    type(c_ptr), intent(in) :: cls

    type(QuerySet), pointer :: query_set
    call c_f_pointer(cls, query_set)

    call query_set%print()
  end subroutine query_set__print_c

  subroutine query_set__deallocate(query_set_cptr) bind(C, name='query_set__deallocate_f')
    type(c_ptr), value :: query_set_cptr
    type(QuerySet), pointer :: query_set_fptr

    call c_f_pointer(query_set_cptr, query_set_fptr)
    deallocate(query_set_fptr)
  end subroutine query_set__deallocate


  ! Execute Functions
  subroutine execute_c(file_unit, query_set_c, next, result_set_c) bind(C, name='execute_f')
    integer(c_int), value, intent(in) :: file_unit
    type(c_ptr), intent(in) :: query_set_c
    integer(c_int), value, intent(in) :: next
    type(c_ptr), intent(inout) :: result_set_c

    type(QuerySet), pointer :: query_set_f
    type(ResultSet), pointer :: result_set_f
    integer :: int_type

    call c_f_pointer(query_set_c, query_set_f)
    call c_f_pointer(result_set_c, result_set_f)

    call execute_(transfer(file_unit, int_type), &
                  query_set_f, &
                  result_set_f, &
                  transfer(next, int_type))
  end subroutine execute_c

  ! Result Set Methods
  subroutine result_set__allocate(result_set_cptr) bind(C, name='result_set__allocate_f')
    type(c_ptr), intent(inout) :: result_set_cptr
    type(ResultSet), pointer :: result_set_fptr

    allocate(result_set_fptr)
    allocate(result_set_fptr%names(0))
    allocate(result_set_fptr%data_frames(0))
    allocate(result_set_fptr%field_widths(0))

    result_set_cptr = c_loc(result_set_fptr)
  end subroutine result_set__allocate


  subroutine result_set__get_c(cls, field, for_field, data, dim_rows, dim_cols, dim_z) bind(C, name="result_set__get_f")
    type(c_ptr), intent(inout) :: cls
    character(kind=c_char, len=1), intent(in) :: field
    character(kind=c_char, len=1), intent(in) :: for_field
    type(c_ptr), intent(inout) :: data
    integer(kind=c_int), intent(out) :: dim_rows
    integer(kind=c_int), intent(out) :: dim_cols
    integer(kind=c_int), intent(out) :: dim_z


    character(len=:), allocatable :: f_field, f_for_field
    real(kind=8), allocatable :: data_f(:, :, :)
    real(kind=8), pointer :: data_f_ptr(:, :, :)
    integer :: dims(3)

    type(ResultSet), pointer :: result_set_fptr
    call c_f_pointer(cls, result_set_fptr)

    f_field = c_f_string(field)
    f_for_field = c_f_string(for_field)

    data_f = result_set_fptr%get(f_field, f_for_field)

    dims = shape(data_f)
    dim_rows = dims(1)
    dim_cols = dims(2)
    dim_z = dims(3)
    if (dim_rows * dim_cols * dim_z > 0) then
      allocate(data_f_ptr, source=data_f)
      data = c_loc(data_f_ptr(1, 1, 1))
    end if

  end subroutine result_set__get_c


  subroutine result_set__get_as_chars_c(cls, field, for_field, strs_c, num_strs) &
    bind(C, name="result_set__get_as_chars_f")

    type(c_ptr), intent(inout) :: cls
    character(kind=c_char, len=1), intent(in) :: field
    character(kind=c_char, len=1), intent(in) :: for_field
    type(c_ptr), intent(inout) :: strs_c(:)
    integer(kind=c_int), intent(out) :: num_strs

    character(len=:), allocatable :: f_field, f_for_field
    character(len=:), allocatable :: strs_f(:)
    integer :: idx
    
    ! strs_f_targ is needed to overcome a GNU compiler issue 
    character(len=:), target, allocatable :: strs_f_targ(:)

    type(ResultSet), pointer :: result_set_fptr
    call c_f_pointer(cls, result_set_fptr)

    f_field = c_f_string(field)
    f_for_field = c_f_string(for_field)

    strs_f = result_set_fptr%get_as_chars(f_field, f_for_field)

    allocate(character(len=len(strs_f) + 1)::strs_f_targ(size(strs_f)))

    do idx = 1, size(strs_f)
      strs_f_targ(idx) = trim(strs_f(idx))//c_null_char
      strs_c(idx) = c_loc(strs_f_targ(idx))
    end do

    num_strs = size(strs_f)

  end subroutine result_set__get_as_chars_c


  type(logical) function result_set__is_string_c(cls, field) &
    result(is_string) &
    bind(C, name="result_set__is_string_f")
    
    type(c_ptr), intent(inout) :: cls
    character(kind=c_char, len=1), intent(in) :: field

    type(ResultSet), pointer :: result_set_fptr

    call c_f_pointer(cls, result_set_fptr)
    is_string = result_set_fptr%is_string(c_f_string(field))
  end function result_set__is_string_c
  

  subroutine result_set__deallocate(result_set_cptr) bind(C, name='result_set__deallocate_f')
    type(c_ptr), value :: result_set_cptr
    type(ResultSet), pointer :: result_set_fptr

    call c_f_pointer(result_set_cptr, result_set_fptr)
    deallocate(result_set_fptr)
  end subroutine result_set__deallocate

end module query_interface
