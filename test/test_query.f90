

module modq_test
  implicit none

contains
  subroutine compare_arrays(array1, array2)
    real(kind=8), intent(in) :: array1(:, :)
    real(kind=4), intent(in) :: array2(:, :)
    integer :: shape_1(2) , shape_2(2)

    shape_1 = shape(array1)
    shape_2 = shape(array2)

    if ((shape_1(1) /= shape_2(1)) .or. (shape_1(2) /= shape_2(2))  ) then
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
  use modq_string
  use modq_result_set
  use modq_test
  implicit none

  type(ResultSet) :: result_set
  type(DataFrame) :: data_frame
  type(DataField) :: data_field

  data_frame = DataFrame()

  data_field = DataField()
  data_field%name = String("N3")
  data_field%node_id = 7
  data_field%data = reshape((/1, 2, 3, 4, 5, 6/), (/6, 1/))
  data_field%seq_path = (/0, 1/)
  allocate(data_field%seq_counts(3))
  data_field%seq_counts(1) = SeqCounts((/3/))
  data_field%seq_counts(2) = SeqCounts((/2, 3, 1/))

  call data_frame%add(data_field)

  data_field = DataField()
  data_field%name = String("N7")
  data_field%node_id = 6
  data_field%data = reshape((/1.01, 2.02, 3.03, 4.04, 5.05, 6.06, 7.07, 8.08, 9.09, &
                              10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.1/), (/16, 1/))
  data_field%seq_path = (/0, 1, 2/)
  allocate(data_field%seq_counts(3))
  data_field%seq_counts(1) = SeqCounts((/3/))
  data_field%seq_counts(2) = SeqCounts((/2, 3, 1/))
  data_field%seq_counts(3) = SeqCounts((/1, 2, 3, 4, 5, 1/))

  call data_frame%add(data_field)

  data_field = DataField()
  data_field%name = String("N0")
  data_field%node_id = 13
  data_field%data = reshape((/10.1, 10.34, 10.49/), (/3, 1/))
  data_field%seq_path = (/0/)
  allocate(data_field%seq_counts(1))
  data_field%seq_counts(1) = SeqCounts((/3/))

  call data_frame%add(data_field)

  data_field = DataField()
  data_field%name = String("N8")
  data_field%node_id = 16
  data_field%data = reshape((/1.05, 2.05, 3.05, 4.05, 5.05, 6.05, 7.05/), (/7, 1/))
  data_field%seq_path = (/0, 2, 4/)
  allocate(data_field%seq_counts(3))
  data_field%seq_counts(1) = SeqCounts((/3/))
  data_field%seq_counts(2) = SeqCounts((/2, 1, 1/))
  data_field%seq_counts(3) = SeqCounts((/1, 2, 3, 1/))

  call data_frame%add(data_field)

  result_set = ResultSet()
  call result_set%add(data_frame)

!  call compare_arrays(result_set%get("N0"), reshape((/10.1, 10.34, 10.49/)), (/3, 1/))
!  call compare_arrays(result_set%get("N7"), reshape((/1.01, 2.02, 3.03, 4.04, 5.05, 6.06, 7.07, 8.08, 9.09, &
!                                                      10.1, 11.11, 12.12, 13.13, 14.14, 15.15, 16.1/), (/16, 1/)))
!  call compare_arrays(result_set%get("N0", for="N7"), reshape((/10.1, 10.1, 10.1, 10.34, 10.34, 10.34, 10.34, 10.34, &
!                                                                10.34, 10.34, 10.34, 10.34, 10.34, 10.34, 10.34, &
!                                                                10.49/), (/ 16, 1 /)))
!  call compare_arrays(result_set%get("N8"), reshape((/1.05, 2.05, 3.05, 4.05, 5.05, 6.05, 7.05/), (/7, 1/)))
!  call compare_arrays(result_set%get("N0", for="N8"), reshape((/10.1, 10.1, 10.1, 10.34, 10.34, 10.34, 10.49/), &
!                                                              (/7, 1/)))

end subroutine test__result_set


subroutine test__query__hrs
  use modq_query
  use modq_query_set
  use modq_result_set
  implicit none

  integer :: num_msgs
  integer :: ireadmg, ireadsb
  character(8) :: subset
  integer(kind=8) :: my_idate
  integer(kind=8) :: iret
  integer, parameter :: lunit = 12

  type(QuerySet) :: query_set
  type(ResultSet) :: result_set

  open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gdas.t00z.1bhrs4.tm00.bufr_d")
  call openbf(lunit, "IN", lunit)

  call query_set%add("*/CLAT", "latitude")
  call query_set%add("*/CLON", "longitude")
  call query_set%add("*/BRIT/TMBR", "radiance")
!  call query_set%add("*/ROSEQ1/ROSEQ2/BNDA[2]", "bending_angle")

  num_msgs = 1
  result_set = ResultSet()
  do while (ireadmg(lunit, subset, my_idate) == 0)
    do while (ireadsb(lunit) == 0)
      call query(lunit, query_set, result_set)
      print *, subset
      print *, "Lat", result_set%get("latitude")
!      call all_result_set%add(result_set%data_frames(1))
    end do

    num_msgs = num_msgs + 1
    if (num_msgs > 10) then
      exit
    end if
  end do

!  print *, "Lat", all_result_set%get("latitude") !, "Lon", all_result_set%get("longitude")
!  print *, "Rad", all_result_set%get("radiance")

  call closbf(12)
  close(12)
end subroutine test__query__hrs


subroutine test__query__gnssro
  use modq_query
  use modq_query_set
  use modq_result_set
  implicit none

  integer :: num_msgs
  integer :: ireadmg, ireadsb
  character(8) :: subset
  integer(kind=8) :: my_idate
  integer(kind=8) :: iret
  integer, parameter :: lunit = 12
  integer :: idx

  type(QuerySet) :: query_set
  type(ResultSet) :: result_set, all_result_set

  open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
  call openbf(lunit, "IN", lunit)

  call query_set%add("*/ROSEQ1/CLATH", "latitude")
  call query_set%add("*/ROSEQ1/CLONH", "longitude")
  call query_set%add("*/ROSEQ1/ROSEQ2/IMPP", "frequency")
  call query_set%add("*/ROSEQ1/ROSEQ3/HEIT", "heit")
!  call query_set%add("*/BRIT/TMBR", "radiance")
  !  call query_set%add("*/ROSEQ1/ROSEQ2/BNDA[2]", "bending_angle")

  num_msgs = 1
  result_set = ResultSet()
  do while (ireadmg(lunit, subset, my_idate) == 0)
    do while (ireadsb(lunit) == 0)
      call query(lunit, query_set, result_set)
    end do

    num_msgs = num_msgs + 1
    if (num_msgs > 2) then
      exit
    end if
  end do

  print *, "Lat", result_set%get("latitude")
  print *, "Lon", result_set%get("longitude")
!  print *, "Freq", size(result_set%get("heit"))


  call closbf(12)
  close(12)
end subroutine test__query__gnssro


subroutine test__query
  use modq_execute
  use modq_query_set
  use modq_result_set
  implicit none

  integer, parameter :: lunit = 12

  type(QuerySet) :: query_set
  type(ResultSet) :: result_set

  open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
  call openbf(lunit, "IN", lunit)

!  call query_set%add("*/ROSEQ1/CLATH", "latitude")
!  call query_set%add("*/ROSEQ1/CLONH", "longitude")
  call query_set%add("*/ROSEQ1/ROSEQ2/BNDA[1]", "bending_angle")

  result_set = execute(lunit, query_set, 2)

  print *, "Bending Angle", result_set%get("bending_angle")

end subroutine test__query

!subroutine test_list
!  use modq_list
!  implicit none
!
!  type(IntList) :: my_list
!
!  my_list = IntList()
!  call my_list%append(100)
!  call my_list%append(10)
!  call my_list%append(32)
!
!  if (mylist%count() /= 3)
!    error stop "List count is wrong"
!  end if
!
!  if (mylist%at(2) /= 10)
!    error stop "List contents is wrong"
!  end if
!
!  call my_list%delete()
!
!
!
!
!end subroutine test_list




program test_query
!  use modq_result_set
  implicit none


!  call test__query_set
!  call test__result_set
!  call test__query__gnssro
  call test__query

end program test_query



