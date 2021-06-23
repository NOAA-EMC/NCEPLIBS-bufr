
module modq_test
  implicit none

contains
  subroutine compare_arrays(array1, array2)
    real(kind=8), intent(in) :: array1(:)
    real(kind=4), intent(in) :: array2(:)

    if (size(array1) /= size(array2)) then
      call bort("Array sizes mismatch.")
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      call bort("Value mismatch.")
    end if
  end subroutine compare_arrays
end module modq_test