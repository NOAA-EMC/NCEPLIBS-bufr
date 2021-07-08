module modq_execute
  use modq_string
  use modq_query
  use modq_query_set
  use modq_result_set
  implicit none

  contains
    type(ResultSet) function execute(file_unit, query_set, next) result(result_set)
      integer, intent(in) :: file_unit
      type(QuerySet), intent(in) :: query_set
      integer, optional, intent(in) ::  next

      integer :: num_msgs

      if (.not. present(next)) then
        num_msgs = 0
      else
        num_msgs = next
      end if

      result_set = ResultSet()
      call execute_(file_unit, query_set, result_set, num_msgs)
    end function execute


    subroutine execute_(file_unit, query_set, result_set, next)
      integer, intent(in) :: file_unit
      type(QuerySet), intent(in) :: query_set
      type(ResultSet), intent(inout) :: result_set
      integer, intent(in) ::  next

      integer :: ireadmg, ireadsb
      character(8) :: subset
      integer(kind=8) :: my_idate
      integer :: msg_num

      msg_num = 0
      do while (ireadmg(file_unit, subset, my_idate) == 0)
        msg_num = msg_num + 1

        if (next > 0) then
          if (msg_num > next) then
            exit
          end if
        end if

        do while (ireadsb(file_unit) == 0)
          call query(file_unit, String(subset), query_set, result_set)
        end do
      end do
    end subroutine execute_


    integer function count_msgs(file_unit) result(total)
      integer, intent(in) :: file_unit

      integer :: ireadmg
      character(8) :: subset
      integer(kind=8) :: my_idate

      rewind(file_unit)

      total = 0
      do while (ireadmg(file_unit, subset, my_idate) == 0)
        total = total + 1
      end do

      rewind(file_unit)
    end function count_msgs

end module modq_execute