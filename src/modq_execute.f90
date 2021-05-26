module modq_execute
  use modq_query
  use modq_query_set
  use modq_result_set
  implicit none



  contains
    type(ResultSet) function execute(file_unit, query_set, next) result(result_set)
      integer, intent(in) :: file_unit
      type(QuerySet), intent(in) :: query_set
      integer, optional, intent(in) ::  next

      integer :: ireadmg, ireadsb
      character(8) :: subset
      integer(kind=8) :: my_idate
      integer(kind=8) :: iret

      integer :: msg_num

      msg_num = 1
      result_set = ResultSet()
      do while (ireadmg(file_unit, subset, my_idate) == 0)
        do while (ireadsb(file_unit) == 0)
          call query(file_unit, query_set, result_set)
        end do

        msg_num = msg_num + 1
        if (msg_num > next) then
          exit
        end if
      end do
    end function execute

end module modq_execute