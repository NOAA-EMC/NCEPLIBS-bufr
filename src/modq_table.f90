module modq_table
  use modq_table
  use modq_string
  implicit none

  type Table

  end type Table

contains
  function get_subsets(lunit) result(subsets)
    integer, intent(in) :: lunit
    type(String), allocatable :: subsets(:), tmp_subsets(:)
    integer :: idx
    logical :: found_subset

    rewind(lunit)
    allocate(subsets(0))

    do while (ireadmg(file_unit, subset, my_idate) == 0)
      found_subset = .false.
      do idx = 1, size(subsets)
        if (subsets(idx) == String(subset)) then
          found_subset = .true.
          exit
        end if
      end do

      if (.not. found_subset) then
        allocate(tmp_subsets(size(subsets) + 1))
        tmp_subsets(1:size(subsets)) = subsets(1:size(subsets))
        tmp_subsets(size(tmp_subsets)) = String(subset)
        deallocate(subsets)
        call move_alloc(tmp_subsets, subsets)
      end if
    end do

    do idx = 1, size(subsets)
      print *, subsets
    end do
  end function get_subsets

  function table_for_subset(lunit, subset) result(data)
    integer, intent(in) :: lunit
    type(String), intent(in) :: subset
    type(Table) :: table

    integer :: ireadmg, ireadsb
    character(8) :: subset
    integer(kind=8) :: my_idate
    integer :: msg_num

    rewind(lunit)

    do while (ireadmg(file_unit, subset, my_idate) == 0)
      msg_num = msg_num + 1


      do while (ireadsb(file_unit) == 0)
        call query(file_unit, subset, query_set, result_set)
      end do
    end do
  end function table_for_subset
end module modq_table