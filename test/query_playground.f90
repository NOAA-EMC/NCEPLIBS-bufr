




!subroutine test__query_set
!  use modq_query_set
!  implicit none
!
!  type(QuerySet) :: query_set
!
!  call query_set%add("*/CLATH", "latitude")
!  call query_set%add("*/CLONH", "longitude")
!  call query_set%add("*/ROSEQ1/ROSEQ2/BNDA[2]", "bending_angle")
!  call query_set%print()
!
!end subroutine test__query_set


!subroutine test__result_set
!  use modq_string
!  use modq_result_set
!  use modq_test
!  implicit none
!
!  type(ResultSet) :: result_set
!  type(DataFrame) :: data_frame
!  type(DataField) :: data_field
!
!  data_frame = DataFrame()
!
!  data_field = DataField()
!  data_field%name = String("N3")
!  data_field%node_id = 7
!  data_field%data = (/1, 2, 3, 4, 5, 6/)
!  data_field%seq_path = (/0, 1/)
!  allocate(data_field%seq_counts(3))
!  data_field%seq_counts(1) = SeqCounts((/3/))
!  data_field%seq_counts(2) = SeqCounts((/2, 3, 1/))
!
!  call data_frame%add(data_field)
!
!  data_field = DataField()
!  data_field%name = String("N7")
!  data_field%node_id = 6
!  data_field%data = (/1.01, 2.02, 3.03, 4.04, 5.05, 6.06, 7.07, 8.08, 9.09, &
!                      10.10, 11.11, 12.12, 13.13, 14.14, 15.15, 16.1/)
!  data_field%seq_path = (/0, 1, 2/)
!  allocate(data_field%seq_counts(3))
!  data_field%seq_counts(1) = SeqCounts((/3/))
!  data_field%seq_counts(2) = SeqCounts((/2, 3, 1/))
!  data_field%seq_counts(3) = SeqCounts((/1, 2, 3, 4, 5, 1/))
!
!  call data_frame%add(data_field)
!
!  data_field = DataField()
!  data_field%name = String("N0")
!  data_field%node_id = 13
!  data_field%data = (/10.1, 10.34, 10.49/)
!  data_field%seq_path = (/0/)
!  allocate(data_field%seq_counts(1))
!  data_field%seq_counts(1) = SeqCounts((/3/))
!
!  call data_frame%add(data_field)
!
!  data_field = DataField()
!  data_field%name = String("N8")
!  data_field%node_id = 16
!  data_field%data = (/1.05, 2.05, 3.05, 4.05, 5.05, 6.05, 7.05/)
!  data_field%seq_path = (/0, 2, 4/)
!  allocate(data_field%seq_counts(3))
!  data_field%seq_counts(1) = SeqCounts((/3/))
!  data_field%seq_counts(2) = SeqCounts((/2, 1, 1/))
!  data_field%seq_counts(3) = SeqCounts((/1, 2, 3, 1/))
!
!  call data_frame%add(data_field)
!
!  result_set = ResultSet()
!  call result_set%add(data_frame)
!
!  call compare_arrays(result_set%get("N0"), (/10.1, 10.34, 10.49/))
!  call compare_arrays(result_set%get("N7"), (/1.01, 2.02, 3.03, 4.04, 5.05, 6.06, 7.07, 8.08, 9.09, &
!                                                      10.1, 11.11, 12.12, 13.13, 14.14, 15.15, 16.1/))
!  call compare_arrays(result_set%get("N0", for="N7"), (/10.1, 10.1, 10.1, 10.34, 10.34, 10.34, 10.34, 10.34, &
!                                                        10.34, 10.34, 10.34, 10.34, 10.34, 10.34, 10.34, &
!                                                        10.49/))
!  call compare_arrays(result_set%get("N8"), (/1.05, 2.05, 3.05, 4.05, 5.05, 6.05, 7.05/))
!  call compare_arrays(result_set%get("N0", for="N8"), (/10.1, 10.1, 10.1, 10.34, 10.34, 10.34, 10.49/))
!
!end subroutine test__result_set


subroutine test__query_gnssro
  use modq_execute
  use modq_query_set
  use modq_result_set
  implicit none

  integer, parameter :: lunit = 12

  type(QuerySet) :: query_set
  type(ResultSet) :: result_set

  open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
  call openbf(lunit, "IN", lunit)

  call query_set%add("*/ROSEQ3/ROSEQ5/HEIT", "test")
  call query_set%add("NC003011/ROSEQ1/CLATH", "latitude")
  call query_set%add("*/ROSEQ1/CLONH", "longitude")
  call query_set%add("*/ROSEQ1/ROSEQ2/BNDA[1]", "bending_angle")

!  print *, "Num Messages", count_msgs(lunit)
  result_set = execute(lunit, query_set, next=1)

  print *, "Test", result_set%get("test")
  print *, "Latitude", result_set%get("latitude",  for="bending_angle")
  print *, "Longitude", result_set%get("longitude", for="bending_angle")
  print *, "Bending Angle", result_set%get("bending_angle")

  result_set = execute(lunit, query_set, next=1)

  print *, "Test", result_set%get("test")
  print *, "Latitude", result_set%get("latitude",  for="bending_angle")
  print *, "Longitude", result_set%get("longitude", for="bending_angle")
  print *, "Bending Angle", result_set%get("bending_angle")

  result_set = execute(lunit, query_set, next=1)

  print *, "Test", result_set%get("test")
  print *, "Latitude", result_set%get("latitude",  for="bending_angle")
  print *, "Longitude", result_set%get("longitude", for="bending_angle")
  print *, "Bending Angle", result_set%get("bending_angle")

end subroutine test__query_gnssro

subroutine test_int_list
  use modq_list
  implicit none

  type(IntList) :: my_list

  my_list = IntList()
  call my_list%push(100)
  call my_list%push(10)
  call my_list%push(32)
  call my_list%push(100)
  call my_list%push(10)
  call my_list%push(32)
  call my_list%push(100)
  call my_list%push(10)
  call my_list%push(32)
  call my_list%push(100)
  call my_list%push(10)
  call my_list%push(32)


  print *, my_list%at(1)
  print *, my_list%at(2)
  print *, my_list%at(3)

  my_list%at(2) = 23

  print *, my_list%at(2)
  print *, size(my_list%array()), my_list%array()

!  if (mylist%count() /= 3)
!    error stop "List count is wrong"
!  end if

!  if (mylist%at(2) /= 10) then
!    error stop "List contents is wrong"
!  end if

end subroutine test_int_list

subroutine test_query_parser
  use modq_string
  use modq_query_parser
  implicit none

  integer :: idx

  type(String), allocatable :: query_strs(:)

  query_strs = split_into_subqueries("*/ABCD/CDDC/CLONH")

  do idx = 1, size(query_strs)
    print *, query_strs(idx)%chars()
  end do

end subroutine test_query_parser

subroutine test_table
  use modq_string
  use modq_table
  implicit none

  integer, parameter :: lunit = 12
  type(String), allocatable :: subsets(:)
  type(String), allocatable :: q_paths(:)
  integer :: subset_idx, q_idx
  integer :: ierr

  open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
  call openbf(lunit, "IN", lunit)
!  subsets = all_subsets(lunit)
!
!  call fseek(lunit, 0, 0, ierr)
!
!  do subset_idx = 1, size(subsets)
!    q_paths = all_queries(lunit, subsets(subset_idx))
!    do q_idx = 1, size(q_paths)
!      print *, q_paths(q_idx)%chars()
!    end do
!  end do

  q_paths = all_queries(lunit, String("NC003010"))
  do q_idx = 1, size(q_paths)
    print *, q_paths(q_idx)%chars()
  end do

  close(lunit)
end subroutine test_table


program test_query
  use modq_table
  implicit none


!  call test__query_set
!  call test__result_set
  call test__query_gnssro
!  call test_int_list
!  call test_query_parser
!  call test_table

end program test_query



