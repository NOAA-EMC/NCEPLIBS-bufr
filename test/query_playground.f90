




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
!  call compare_arrays(result_set%get("N0", group_by="N7"), (/10.1, 10.1, 10.1, 10.34, 10.34, 10.34, 10.34, 10.34, &
!                                                        10.34, 10.34, 10.34, 10.34, 10.34, 10.34, 10.34, &
!                                                        10.49/))
!  call compare_arrays(result_set%get("N8"), (/1.05, 2.05, 3.05, 4.05, 5.05, 6.05, 7.05/))
!  call compare_arrays(result_set%get("N0", group_by="N8"), (/10.1, 10.1, 10.1, 10.34, 10.34, 10.34, 10.49/))
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
  real(kind=8), allocatable :: dat(:,:,:)

  open(lunit, file="/home/rmclaren/Work/ioda-bundle/iodaconv/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
  call openbf(lunit, "IN", lunit)

  call query_set%add("*/CLATH", "latitude_root")
  call query_set%add("*/CLONH", "longitude_root")
  call query_set%add("*/ROSEQ1/CLATH", "latitude")
  call query_set%add("*/ROSEQ1/CLONH", "longitude")
  call query_set%add("*/ROSEQ1/ROSEQ2/IMPP", "impactParam")

  !  print *, "Num Messages", count_msgs(lunit)
  result_set = execute(lunit, query_set, next=10)

  ! print *, "Impact", shape(result_set%get("impactParam", group_by="latitude_root"))
!  print *, "Lat ", shape(result_set%get("latitude"))
!  print *, "Lon ", shape(result_set%get("longitude"))
!  print *, "Freq ", result_set%get("longitude", group_by="meanFrequency")
!  print *, "Freq", result_set%get("meanFrequency")

  call closbf(lunit)
  close(lunit)

end subroutine test__query_gnssro


subroutine test__query_radiance
  use modq_execute
  use modq_query_set
  use modq_result_set
  implicit none

  integer, parameter :: lunit = 12

  type(QuerySet) :: query_set
  type(ResultSet) :: result_set
  real(kind=8), allocatable :: dat(:,:,:)

  
!  open(lunit,
!  file="/home/rmclaren/Work/ioda-bundle/iodaconv/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
  open(lunit, file="/home/rmclaren/Work/ioda-bundle/iodaconv/test/testinput/gdas.t18z.1bmhs.tm00.bufr_d")
  call openbf(lunit, "IN", lunit)

  call query_set%add("*/CLAT", "latitude")
  call query_set%add("*/CLON", "longitude")
  call query_set%add("[*/BRITCSTC/TMBR, */BRIT/TMBR]", "radiance")
  call query_set%add("[*/BRITCSTC/CHNM, */BRIT/CHNM]", "channel")

!  print *, "Num Messages", count_msgs(lunit)
  result_set = execute(lunit, query_set, next=1)

  ! print *, "Longitude", result_set%get("longitude", group_by="radiance")
  print *, "Radiance", result_set%get("radiance", group_by="longitude")

  call closbf(lunit)
  close(lunit)

end subroutine test__query_radiance

subroutine test__query_ia5
  use modq_execute
  use modq_query_set
  use modq_result_set
  implicit none

  integer, parameter :: lunit = 12

  type(QuerySet) :: query_set
  type(ResultSet) :: result_set
  character(:), allocatable :: data(:)


  open(lunit, file="/home/rmclaren/Work/ioda-bundle/iodaconv/test/testinput/bufr_satwnd_old_format.bufr")
  call openbf(lunit, "IN", lunit)

  call query_set%add("*/BORG", "resistance_is_futile")

!  print *, "Num Messages", count_msgs(lunit)
  result_set = execute(lunit, query_set, next=5)

  data = result_set%get_as_chars("resistance_is_futile")

  print *, "Type: "
  block  ! print data
    integer :: idx

    do idx = 1, size(data)
      print *, data(idx), transfer(data(idx)(5:5), idx)
    end do
  end block ! print data

  call closbf(lunit)
  close(lunit)
end subroutine test__query_ia5

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

  open(lunit, file="/home/rmclaren/Data/ADPSFC_split.prepbufr")
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

  q_paths = all_queries(lunit, String("ADPSFC"))
  do q_idx = 1, size(q_paths)
    print *, q_paths(q_idx)%chars()
  end do

  call closbf(lunit)
  close(lunit)
end subroutine test_table


subroutine test_adpsfc
  use modq_execute
  use modq_query_set
  use modq_result_set
  use modq_test
  implicit none

  integer, parameter :: lunit = 12

  type(QuerySet) :: query_set
  type(ResultSet) :: result_set
  real(kind=8), allocatable :: dat(:,:,:)
  real(kind=8), allocatable :: old_interface_data(:)

  open(lunit, file="/home/rmclaren/Data/ADPSFC_split.prepbufr")
  call openbf(lunit, "IN", lunit)

!  ! Get data using old interface
!  block  ! Old Interface
!    integer :: ireadmg, ireadsb
!    character(8) :: subset
!    integer(kind=8) :: my_idate
!    integer :: lun, il, im
!    integer :: iret
!    integer :: data_size
!    integer :: data_idx
!    real(kind=8) :: msg_data
!
!    call status(lunit, lun, il, im)
!
!    allocate(old_interface_data(1000))
!
!    ! Get the data
!    data_idx = 0
!    do while (ireadmg(lunit, lun, subset, my_idate) == 0)
!      do while (ireadsb(lunit) == 0)
!        data_idx = data_idx + 1
!        call ufbint(lunit, msg_data, 1, 1, iret, "YOB")
!        old_interface_data(data_idx) = msg_data
!      end do
!    end do
!  end block  ! Old Interface
!
!  print *, old_interface_data



  !  call query_set%add("*/XOB", "longitude")
  call query_set%add("*/YOB", "latitude")
  call query_set%add("*/T___INFO/T__EVENT/TOB", "temperature")

  result_set = execute(lunit, query_set, next=5)

!  print *, "", shape(result_set%get("temperature", group_by="latitude"))
!  print *, "Longitude: ", shape(result_set%get("longitude"))

  print *, result_set%get("latitude")
  dat = result_set%get("temperature") + 273.15
!  print *, dat(2,:,1)
  print *, shape(dat)

  call closbf(lunit)
  close(lunit)
end subroutine test_adpsfc


program test_query
  use modq_table
  implicit none


!  call test__query_set
!  call test__result_set
!  call test__query_gnssro
!  call test__query_ia5
!  call test_int_list
!  call test_query_parser
 call test__query_radiance
!  call test__query_gnssro
 call test_adpsfc
  ! call test_table
end program test_query



