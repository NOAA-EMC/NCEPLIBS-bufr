module test_query_root
  use modq_test
  use modq_query_set
  use modq_result_set
  use modq_execute
  implicit none


  contains
    subroutine test__get_root_data
      character(len=*), parameter :: FilePath = "testfiles/IN_5"
      integer, parameter :: FileUnit = 12

      type(TestInstance) :: test
      real(kind=8), allocatable :: old_interface_data(:), query_interface_data(:, :, :)

      test = TestInstance("test__get_root_data")

      ! Open the file
      open(FileUnit, file=FilePath)
      call openbf(FileUnit, "IN", FileUnit)

      ! Get data using old interface
      block  ! Old Interface
        integer :: ireadmg, ireadsb
        character(8) :: subset
        integer(kind=8) :: my_idate
        integer :: lun, il, im
        integer :: iret
        integer :: data_size
        integer :: data_idx
        real(kind=8) :: msg_data

        call status(FileUnit, lun, il, im)

        ! Count the number of data elements
        data_size = 0
        do while (ireadmg(FileUnit, lun, subset, my_idate) == 0)
          do while (ireadsb(FileUnit) == 0)
            data_size = data_size + 1
          end do
        end do

        call reset_file(FileUnit, FilePath)
        call status(FileUnit, lun, il, im)

        allocate(old_interface_data(data_size))

        ! Get the data
        data_idx = 0
        do while (ireadmg(FileUnit, lun, subset, my_idate) == 0)
          do while (ireadsb(FileUnit) == 0)
            data_idx = data_idx + 1
            call ufbint(FileUnit, msg_data, 1, 1, iret, "YOB")
            old_interface_data(data_idx) = msg_data
          end do
        end do
      end block  ! Old Interface

      ! Rewind File
      call reset_file(FileUnit, FilePath)


      ! Get the same data with the query interface
      block  ! Query Interface
        type(QuerySet) :: query_set
        type(ResultSet) :: result_set

        ! Use query interface to get the same data
        call query_set%add("*/YOB", "latitude")
        result_set = execute(FileUnit, query_set)
        query_interface_data = result_set%get("latitude")
      end block  ! Query Interface

!      print *, "Old", old_interface_data
!      print *, "New", query_interface_data

      ! Compare the arrays
      call test%compare_arrays(old_interface_data, pack(query_interface_data, .true.))

      !Clean Up
      call closbf(FileUnit)
      close(FileUnit)

    end subroutine test__get_root_data


    subroutine test__get_root_missing_data
      character(len=*), parameter :: FilePath = "testfiles/IN_5"
      integer, parameter :: FileUnit = 12

      type(TestInstance) :: test
      real(kind=8), allocatable :: old_interface_data(:), query_interface_data(:, :, :)

      test = TestInstance("test__get_root_missing_data")

      ! Open the file
      open(FileUnit, file=FilePath)
      call openbf(FileUnit, "IN", FileUnit)


      ! Get data using old interface
      block  ! Old Interface
        integer :: ireadmg, ireadsb
        character(8) :: subset
        integer(kind=8) :: my_idate
        integer :: lun, il, im
        integer :: iret
        integer :: data_size
        integer :: data_idx
        real(kind=8) :: msg_data

        call status(FileUnit, lun, il, im)

        ! Count the number of data elements
        data_size = 0
        do while (ireadmg(FileUnit, lun, subset, my_idate) == 0)
          do while (ireadsb(FileUnit) == 0)
            data_size = data_size + 1
          end do
        end do

        call reset_file(FileUnit, FilePath)
        call status(FileUnit, lun, il, im)

        allocate(old_interface_data(data_size))

        ! Get the data
        data_idx = 0
        do while (ireadmg(FileUnit, lun, subset, my_idate) == 0)
          do while (ireadsb(FileUnit) == 0)
            data_idx = data_idx + 1
            call ufbint(FileUnit, msg_data, 1, 1, iret, "ABC")
            old_interface_data(data_idx) = msg_data
          end do
        end do
      end block  ! Old Interface

      ! Rewind File
      call reset_file(FileUnit, FilePath)


      ! Get the same data with the query interface
      block  ! Query Interface
        type(QuerySet) :: query_set
        type(ResultSet) :: result_set

        ! Use query interface to get the same data
        call query_set%add("*/ABC", "latitude")
        result_set = execute(FileUnit, query_set)
        query_interface_data = result_set%get("latitude")
      end block  ! Query Interface

!      print *, "Old", old_interface_data
!      print *, "New", query_interface_data

      ! Compare the arrays
      call test%compare_arrays(old_interface_data, pack(query_interface_data, .true.))

      !Clean Up
      call closbf(FileUnit)
      close(FileUnit)

    end subroutine test__get_root_missing_data


    subroutine test__get_root_for_seq
      character(len=*), parameter :: FilePath = "testfiles/IN_5"
      integer, parameter :: FileUnit = 12

      type(TestInstance) :: test
      real(kind=8), allocatable :: old_interface_data(:), query_interface_data(:,:,:)

      test = TestInstance("test__get_root_for_seq")

      ! Open the file
      open(FileUnit, file=FilePath)
      call openbf(FileUnit, "IN", FileUnit)


!      ! Get data using old interface
!      block  ! Old Interface
!        integer :: ireadmg, ireadsb
!        character(8) :: subset
!        integer(kind=8) :: my_idate
!        integer :: lun, il, im
!        integer :: iret
!        integer :: data_size
!        integer :: data_idx
!        real(kind=8) :: msg_data
!
!        call status(FileUnit, lun, il, im)
!
!        ! Count the number of data elements
!        data_size = 0
!        do while (ireadmg(FileUnit, lun, subset, my_idate) == 0)
!          do while (ireadsb(FileUnit) == 0)
!            data_size = data_size + 1
!          end do
!        end do
!
!        call reset_file(FileUnit, FilePath)
!        call status(FileUnit, lun, il, im)
!
!        allocate(old_interface_data(data_size))
!
!        ! Get the data
!        data_idx = 0
!        do while (ireadmg(FileUnit, lun, subset, my_idate) == 0)
!          do while (ireadsb(FileUnit) == 0)
!            data_idx = data_idx + 1
!            call ufbint(FileUnit, msg_data, 1, 1, iret, "YOB")
!            old_interface_data(data_idx) = msg_data
!          end do
!        end do
!      end block  ! Old Interface
!
!      ! Rewind File
!      call reset_file(FileUnit, FilePath)


      ! Get the same data with the query interface
      block  ! Query Interface
        type(QuerySet) :: query_set
        type(ResultSet) :: result_set

        ! Use query interface to get the same data
        call query_set%add("*/YOB", "latitude")
        call query_set%add("*/PRSLEVEL/CAT", "data_level_category")
        result_set = execute(FileUnit, query_set)
        query_interface_data = result_set%get("latitude", group_by="data_level_category")
      end block  ! Query Interface

      !      print *, "Old", old_interface_data
!      print *, "New", size(query_interface_data)

      ! Compare the arrays
      call test%compare_arrays(old_interface_data, pack(query_interface_data, .true.), "Doomed to fail!")

      !Clean Up
      call closbf(FileUnit)
      close(FileUnit)

    end subroutine test__get_root_for_seq

end module test_query_root


program main
  use test_query_root
  implicit none

  call test__get_root_data
  call test__get_root_missing_data
  ! call test__get_root_for_seq

end program main