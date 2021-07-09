
module modq_test
  use modq_string
  implicit none

  type, public :: TestInstance
    private
      type(String) :: test_name

    contains
      procedure :: failed => test__failed
      procedure :: compare_arrays => test__compare_arrays
  end type TestInstance

  interface TestInstance
    module procedure initialize__test
  end interface TestInstance

  private
  public::reset_file

contains

  type(TestInstance) function initialize__test(name) result(test)
    character(len=*) :: name

    test = TestInstance(String(name))

    print *, "Running ", name
  end function initialize__test

  subroutine test__failed(self, msg)
    class(TestInstance), intent(in) :: self
    character(len=*) :: msg

    character(len=:), allocatable :: abort_msg

    abort_msg = self%test_name%chars() // ": " // msg
    call bort(abort_msg)
  end subroutine

  subroutine test__compare_arrays(self, array1, array2, extra_msg)
    class(TestInstance), intent(in) :: self
    class(*), intent(in) :: array1(:)
    class(*), intent(in) :: array2(:)
    character(len=*), optional, intent(in) :: extra_msg

    character(len=:), allocatable :: extra_msg_

    logical :: value_mismatch

    if (present(extra_msg)) then
      extra_msg_ = extra_msg // " "
    else
      extra_msg_ = ""
    end if

    if (size(array1) /= size(array2)) then
      call self%failed(extra_msg_ // "Array sizes mismatch.")
    end if

    value_mismatch = .true.
    select type (array1)
      type is (real(kind=4))
        select type (array2)
          type is (real(kind=4))
            value_mismatch = any(abs(array1 - array2) > 1e-6)
          type is (real(kind=8))
            value_mismatch = any(abs(array1 - array2) > 1e-6)
        end select
      type is (real(kind=8))
        select type (array2)
          type is (real(kind=4))
            value_mismatch = any(abs(array1 - array2) > 1e-6)
          type is (real(kind=8))
            value_mismatch = any(abs(array1 - array2) > 1e-6)
        end select
    end select

    if (value_mismatch) then
      call self%failed(extra_msg_ // "Value mismatch.")
    end if
  end subroutine test__compare_arrays

  subroutine reset_file(file_unit, file_name)
    integer, intent(in) :: file_unit
    character(len=*), intent(in) :: file_name

    call closbf(file_unit)
    close(file_unit)
    open(file_unit, file=file_name)
    call openbf(file_unit, "IN", file_unit)
  end subroutine reset_file

end module modq_test