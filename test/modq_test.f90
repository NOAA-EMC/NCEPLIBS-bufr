
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
  end function initialize__test

  subroutine test__failed(self, msg)
    class(TestInstance), intent(in) :: self
    character(len=*) :: msg

    character(len=:), allocatable :: abort_msg

    abort_msg = self%test_name%chars() // ": " // msg
    call bort(abort_msg)
  end subroutine

  subroutine test__compare_arrays(self, array1, array2)
    class(TestInstance), intent(in) :: self
    class(*), intent(in) :: array1(:)
    class(*), intent(in) :: array2(:)

    real(kind=8), allocatable :: real_array_1(:)
    real(kind=8), allocatable :: real_array_2(:)

    select type (array1)
      type is (real(kind=4))
        select type (array2)
          type is (real(kind=4))
            call compare_arrays_r_4_4(self, array1, array2)
          type is (real(kind=8))
            call compare_arrays_r_4_8(self, array1, array2)
        end select
      type is (real(kind=8))
        select type (array2)
          type is (real(kind=4))
            call compare_arrays_r_8_4(self, array1, array2)
          type is (real(kind=8))
            call compare_arrays_r_8_8(self, array1, array2)
        end select
    end select
  end subroutine test__compare_arrays

  subroutine compare_arrays_r_8_4(test, array1, array2)
    class(TestInstance), intent(in) :: test
    real(kind=8), intent(in) :: array1(:)
    real(kind=4), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call test%failed("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call test%failed("Value mismatch.")
    end if
  end subroutine compare_arrays_r_8_4


  subroutine compare_arrays_r_4_4(test, array1, array2)
    class(TestInstance), intent(in) :: test
    real(kind=4), intent(in) :: array1(:)
    real(kind=4), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call test%failed("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call test%failed("Value mismatch.")
    end if
  end subroutine compare_arrays_r_4_4


  subroutine compare_arrays_r_4_8(test, array1, array2)
    class(TestInstance), intent(in) :: test
    real(kind=4), intent(in) :: array1(:)
    real(kind=8), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call test%failed("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call test%failed("Value mismatch.")
    end if
  end subroutine compare_arrays_r_4_8


  subroutine compare_arrays_r_8_8(test, array1, array2)
    class(TestInstance), intent(in) :: test
    real(kind=8), intent(in) :: array1(:)
    real(kind=8), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call test%failed("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call test%failed("Value mismatch.")
    end if
  end subroutine compare_arrays_r_8_8


  subroutine reset_file(file_unit, file_name)
    integer, intent(in) :: file_unit
    character(len=*), intent(in) :: file_name

    call closbf(file_unit)
    close(file_unit)
    open(file_unit, file=file_name)
    call openbf(file_unit, "IN", file_unit)
  end subroutine reset_file

end module modq_test