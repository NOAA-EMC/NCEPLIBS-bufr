

program bufr_test
  use mod_query

  character(8) :: subset
  integer(kind=8) :: idate
  integer(kind=8) :: iret
  integer, parameter :: lunit = 12
  real, pointer :: data_ptr

    open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gnssro_kompsat5_20180415_00Z.bufr")
    !    open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gdas.t00z.1bhrs4.tm00.bufr_d")
!    open(lunit, file="/home/rmclaren/Work/ioda-bundle/ioda_converters/test/testinput/gdas.t18z.1bmhs.tm00.bufr_d")
    call openbf(lunit, "IN", lunit)

  do while (ireadmg(lunit, subset, idate) == 0)
    do while (ireadsb(lunit) == 0)
      call query(lunit, "/ROSEQ1/ROSEQ2/BNDA", data_ptr)
    end do
  end do

  call closbf(12)
  close(12)
end program