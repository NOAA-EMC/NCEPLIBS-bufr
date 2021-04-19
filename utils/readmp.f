C> @file
C> @brief Read BUFR file containing embedded DX BUFR tables,
C> and print each report one at a time

      program readmp
      character*8 subset
      character*1 go     
      call openbf(20,'IN',20)
      do while(ireadmg(20,subset,idate).eq.0)
      do while(ireadsb(20).eq.0)
      print*,'message date=',i4dy(idate)
      call ufdump(20,6)
      read(5,'(a)') go
      if(go.eq.'q') stop
      enddo
      enddo
      stop
      end
