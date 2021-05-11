

module modq_test
  implicit none

contains
  subroutine compare_arrays(array1, array2)
    real(kind=8), intent(in) :: array1(:)
    real(kind=4), intent(in) :: array2(:)
    integer :: idx

    if (size(array1) /= size(array2)) then
      error stop "Array sizes mismatch."
    end if

    if (any(abs(array1 - array2) > 1e-6)) then
      error stop "Value mismatch."
    end if
  end subroutine compare_arrays
end module modq_test


subroutine test__query_set
  use modq_query_set
  implicit none

  type(QuerySet) :: query_set

  call query_set%add("*/CLATH", "latitude")
  call query_set%add("*/CLONH", "longitude")
  call query_set%add("*/ROSEQ1/ROSEQ2/BNDA[2]", "bending_angle")
  call query_set%print()

end subroutine test__query_set


subroutine test__result_set
  use modq_result_set
  use modq_string
  use modq_test
  implicit none

  type(ResultSet) :: result_set
  type(DataFrame) :: data_frame
  type(DataField) :: data_field

  data_frame = DataFrame()

  data_field = DataField()
  data_field%name = String("N3")
  data_field%node_id = 7
  data_field%data = (/1, 2, 3, 4, 5, 6/)
  data_field%seq_path = (/0, 1/)
  allocate(data_field%seq_counts(3))
  data_field%seq_counts(1) = SeqCounts((/3/))
  data_field%seq_counts(2) = SeqCounts((/2, 3, 1/))

  call data_frame%add(data_field)

  data_field = DataField()
  data_field%name = String("N7")
  data_field%node_id = 6
  data_field%data = (/1.01, 2.02, 3.03, 4.04, 5.05, 6.06, 7.07, 8.08, 9.09, &
                     10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.1/)
  data_field%seq_path = (/0, 1, 2/)
  allocate(data_field%seq_counts(3))
  data_field%seq_counts(1) = SeqCounts((/3/))
  data_field%seq_counts(2) = SeqCounts((/2, 3, 1/))
  data_field%seq_counts(3) = SeqCounts((/1, 2, 3, 4, 5, 1/))

  call data_frame%add(data_field)

  data_field = DataField()
  data_field%name = String("N0")
  data_field%node_id = 13
  data_field%data = (/10.1, 10.34, 10.49/)
  data_field%seq_path = (/0/)
  allocate(data_field%seq_counts(1))
  data_field%seq_counts(1) = SeqCounts((/3/))

  call data_frame%add(data_field)

  data_field = DataField()
  data_field%name = String("N8")
  data_field%node_id = 16
  data_field%data = (/1.05, 2.05, 3.05, 4.05, 5.05, 6.05, 7.05/)
  data_field%seq_path = (/0, 2, 4/)
  allocate(data_field%seq_counts(3))
  data_field%seq_counts(1) = SeqCounts((/3/))
  data_field%seq_counts(2) = SeqCounts((/2, 1, 1/))
  data_field%seq_counts(3) = SeqCounts((/1, 2, 3, 1/))

  call data_frame%add(data_field)

  result_set = ResultSet()
  call result_set%add(data_frame)

  call compare_arrays(result_set%get("N0"), (/10.1, 10.34, 10.49/))

  call compare_arrays(result_set%get("N7"), (/1.01, 2.02, 3.03, 4.04, 5.05, 6.06, 7.07, 8.08, 9.09, &
                                              10.1, 11.11, 12.12, 13.13, 14.14, 15.15, 16.1/))

  call compare_arrays(result_set%get("N0", for="N7"), (/10.1, 10.1, 10.1, 10.34, 10.34, 10.34, 10.34, 10.34, &
                                                        10.34, 10.34, 10.34, 10.34, 10.34, 10.34, 10.34, &
                                                        10.49/))

  call compare_arrays(result_set%get("N8"), (/1.05, 2.05, 3.05, 4.05, 5.05, 6.05, 7.05/))

  call compare_arrays(result_set%get("N0", for="N8"), (/10.1, 10.1, 10.1, 10.34, 10.34, 10.34, 10.49/))


!  if (result_set%get("N0") /= (/10.1, 10.34, 10.49/)) then
!    error stop "Incorrect N0"
!  end if

end subroutine test__result_set




!subroutine test__list
!  use modq_list
!
!  type(List) :: l
!
!  call l%append(2)
!
!end subroutine test__list


program test_query
!  use mod_strings
!
!  type(String), allocatable :: strs(:)
!  integer :: idx
!
!  allocate(strs(0))
!
!  strs = [strs, String("one")]
!  strs = [strs, String("two")]
!  strs = [strs, String("three")]
!
!  do idx = 1, size(strs)
!    print *, strs(idx)%chars
!  end do
!!  print *, strs

  call test__query_set
  call test__result_set

end program test_query

!program bufr_test
!
!  integer, parameter :: lunit = 12
!
!  character(8) :: subset
!  integer(kind=8) :: idate
!  real(kind=8), pointer :: data_ptr(:, :)
!
!
!  open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
!  !    open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gdas.t00z.1bhrs4.tm00.bufr_d")
!  !    open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gdas.t18z.1bmhs.tm00.bufr_d")
!  call openbf(lunit, "IN", lunit)
!
!  do while (ireadmg(lunit, subset, idate) == 0)
!    do while (ireadsb(lunit) == 0)
!      result_set = ResultSet()
!      call query(lunit, query_set, result_set)
!
!      print *, result_set%as_real("latitude")
!      print *, result_set%as_real("longitude")
!      print *, result_set%as_real("bending_angle")
!
!      call result_set%delete()!    end do
!  end do
!
!  call query_set%delete()
!
!  call closbf(12)
!  close(12)
!end program



