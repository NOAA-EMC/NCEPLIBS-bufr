
module modq_test
  implicit none

  interface compare_arrays
    module procedure :: compare_arrays_r_8_4
    module procedure :: compare_arrays_r_4_4
    module procedure :: compare_arrays_r_4_8
    module procedure :: compare_arrays_r_8_8
  end interface compare_arrays

  private
  public :: compare_arrays
  public :: reset_file

contains

  subroutine compare_arrays_r_8_4(array1, array2)
    real(kind=8), intent(in) :: array1(:)
    real(kind=4), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call bort("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call bort("Value mismatch.")
    end if
  end subroutine compare_arrays_r_8_4


  subroutine compare_arrays_r_4_4(array1, array2)
    real(kind=4), intent(in) :: array1(:)
    real(kind=4), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call bort("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call bort("Value mismatch.")
    end if
  end subroutine compare_arrays_r_4_4


  subroutine compare_arrays_r_4_8(array1, array2)
    real(kind=4), intent(in) :: array1(:)
    real(kind=8), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call bort("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call bort("Value mismatch.")
    end if
  end subroutine compare_arrays_r_4_8


  subroutine compare_arrays_r_8_8(array1, array2)
    real(kind=8), intent(in) :: array1(:)
    real(kind=8), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call bort("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call bort("Value mismatch.")
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