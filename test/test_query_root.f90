module test_query_fixed_reps
  use modq_test
  use modq_query_set
  use modq_result_set
  use modq_execute
  implicit none

  character(len=*), parameter :: FilePath = ""

  contains
    subroutine test__get_root_data

      ! Open the file

      ! Get data using old interface
      integer :: ireadmg, ireadsb
      character(8) :: subset
      integer(kind=8) :: my_idate

      do while (ireadmg(file_unit, subset, my_idate) == 0)
        msg_num = msg_num + 1

        if (present(next)) then
          if (msg_num > next) then
            exit
          end if
        end if

        do while (ireadsb(file_unit) == 0)
          call query(file_unit, String(subset), query_set, result_set)
        end do
      end do

    end subroutine

end module test_query_fixed_reps


program main
  use test_query_fixed_reps
  implicit none

  call test__get_root_data

end program main