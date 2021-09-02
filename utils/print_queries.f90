module mod_print_queries
  use modq_string
  use modq_table
  implicit none

contains
  subroutine print_help
    print *, "Usage: ", "./print_queries.x [-s <subset>] [-h] <input_file>"
    print *, "Description: "
    print *, "  Lists all the queries possible on a BUFR file per subset."
    print *, "Arguments: "
    print *, "  -h          (Optional) Print out the help message."
    print *, "  -s <subset> (Optional) Print paths only for this subset."
    print *, "  input_file  Path to the BUFR file."
    print *, "Examples: "
    print *, "  ./print_queries.x ../data/bufr_satwnd_old_format.bufr"
    print *, "  ./print_queries.x -s NC005066 ../data/bufr_satwnd_old_format.bufr"
  end subroutine print_help

  subroutine print_paths(q_paths)
    type(String), allocatable, intent(in) :: q_paths(:)
    integer :: idx

    do idx = 1, size(q_paths)
      print *, "  " , q_paths(idx)%chars()
    end do
  end subroutine print_paths

  subroutine print_queries(input_file, subset)
    character(len=*), intent(in) :: input_file
    character(len=*), intent(in) :: subset

    integer, parameter :: FileUnit = 12

    type(String), allocatable :: q_paths(:)
    type(String), allocatable :: subsets(:)
    integer :: subset_idx
    logical :: file_opened = .false.


    open(FileUnit, file=trim(input_file))
    call openbf(FileUnit, "IN", FileUnit)

    if (trim(subset) /= "") then
      q_paths = all_queries(FileUnit, String(subset))

      print *, "Possible queries for subset: ", subset
      call print_paths(q_paths)
    else
      subsets = all_subsets(FileUnit)

      if (size(subsets) == 0) then
        call bort("No BUFR subsets found in " // input_file)
      end if

      print *, "Available subsets:"
      do subset_idx = 1, size(subsets)
        print *, "  ", subsets(subset_idx)%chars()
      end do
      print *, "Total number of subsets found:", size(subsets)
      print *, ""

      do subset_idx = 1, size(subsets)
        ! Reset the file
        call closbf(FileUnit)
        close(FileUnit)
        open(FileUnit, file=trim(input_file))
        call openbf(FileUnit, "IN", FileUnit)

        q_paths = all_queries(FileUnit, subsets(subset_idx))

        print *, "Possible queries for subset: ", subsets(subset_idx)%chars()
        call print_paths(q_paths)
        print *, ""
      end do
    end if

    call closbf(FileUnit)
    close(FileUnit)
  end subroutine print_queries

end module mod_print_queries


program main
  use mod_print_queries
  implicit none

  character(len=255) :: input_file
  character(len=255) :: subset
  character(len=255) :: arg
  integer :: idx

  input_file = ""
  subset = ""

  idx = 1
  do while (idx <= iargc())
    call getarg(idx, arg)
    if (trim(arg) == "-s") then
      call getarg(idx + 1, subset)
      idx = idx + 2
    else if (trim(arg) == "-h") then
      call print_help
      return
    else
      input_file = arg
      exit
    end if
  end do

  print *, trim(input_file)
  print *, trim(subset)

  if (trim(input_file) == "") then
    call print_help
    call bort("Error: no input file specified")
  end if

  call print_queries(trim(input_file), trim(subset))
end program main
