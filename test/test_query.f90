

program bufr_test
  use mod_query

  integer, parameter :: lunit = 12

  character(8) :: subset
  integer(kind=8) :: idate
  real(kind=8), pointer :: data_ptr(:, :)

    open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
    !    open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gdas.t00z.1bhrs4.tm00.bufr_d")
!    open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gdas.t18z.1bmhs.tm00.bufr_d")
    call openbf(lunit, "IN", lunit)

  do while (ireadmg(lunit, subset, idate) == 0)
    do while (ireadsb(lunit) == 0)
!      call query(lunit, "/CLONH", data_ptr)
      call query(lunit, "/ROSEQ1/ROSEQ2/BNDA[2]", data_ptr)
      print *, data_ptr
    end do
  end do

  call closbf(12)
  close(12)
end program