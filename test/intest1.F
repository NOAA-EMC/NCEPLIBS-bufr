C       This is a test for NCEPLIBS-bufr.
C
C       Reads test file 'testfiles/IN_1' using CRBMG with
C       OPENBF IO = 'SEC3'.
C
C       Jeff Ator
        program intest1

        integer mxbf, mxbfd4, mxds3, nds3, nbyt, ierme, imgdt
        integer ierndv, iernds
        parameter (mxbf = 20000)
        parameter (mxbfd4 = mxbf/4)
        parameter (mxds3 = 20)
        parameter (mxr8pm = 10)
        parameter (mxr8lv = 255)
        real*8 r8arr(mxr8pm, mxr8lv)
        integer ibfmg(mxbfd4)
        character smidstg*9, softvstg*12, cmgtag*8,
     +                  bfmg(mxbf), cds3(mxds3)*6, tagpr*8,
     +                  celem(2)*60, cunit(2)*22
        character*20 filnam / 'testfiles/IN_1' /
        character filost / 'r' /
        equivalence (bfmg(1), ibfmg(1))

        print *, 'Testing reading IN_1, CRBMG with OPENBF IO = SEC3'

C*      Open the test file.
        call cobfl(filnam, filost)

c       specify format of section 1 date/time when reading.
        call datelen(10)

c       specify the use of section 3 decoding.
        open (unit = 11, file = '/dev/null')
        call openbf(11, 'SEC3', 11)

C       Specify location of master BUFR tables.
        call mtinfo('../tables', 90, 91)

C       Read a BUFR message from the test file into a memory array.
        call crbmg(bfmg, mxbf, nbyt, ierr)
        if (ierr .ne. 0) stop 2

C       Read and check some values from Section 1.
        if ((iupbs01(ibfmg, 'MTYP') .ne. 2) .or.
     +           (iupbs01(ibfmg, 'MTV') .ne. 14) .or.
     +           (iupbs01(ibfmg, 'LENM') .ne. 4169)) stop 3

C       Read and check some values from Section 3.
        if ((iupbs3(ibfmg, 'NSUB') .ne. 1) .or.
     +           (iupbs3(ibfmg, 'ICMP') .ne. 0)) stop 4

C       Read and check some data descriptors from Section 3.
        call upds3(ibfmg, mxds3, cds3, nds3)
        IF (nds3 .ne. 8 .or. cds3(1) .ne. '309052' .or. cds3(5) .ne.
     +      '002095') stop 5

C       Pass the BUFR message from the memory array into the library.
        call readerme(ibfmg, 11, cmgtag, imgdt, ierme)
        if (ierme .ne. 0 .or. cmgtag .ne. 'MSTTB001') stop 5

C       Get and check the element names and units associated with
C       some Table B mnemonics.
        call nemdefs(11, 'VSIGX', celem(1), cunit(1), ierndv)
        call nemdefs(11, 'SMID', celem(2), cunit(2), iernds)
        if (ierndv .ne. 0 .or. iernds .ne. 0 .or. celem(1)(1:40) .ne.
     +             'Extended vertical sounding significance ' .or.
     +          celem(2)(1:39) .ne.
     +             'Ship or mobile land station identifier ' .or.
     +          cunit(1)(1:12) .ne. 'FLAG TABLE  ' .or.
     +          cunit(2)(1:10) .ne. 'CCITT IA5 ') stop 6

C       Read and check the Section 1 date-time.
        if (imgdt .ne. 2012093012) stop 7

C       Read a data subset from the BUFR message.
        if (ireadsb(11) .ne. 0 ) stop 8

C       Get and check the parent of a Table B mnemonic.
        call gettagpr(11, 'PRLC', 192, tagpr, iertgp)
        if (iertgp .ne. 0 .or. tagpr .ne. 'WSPLRAOB') stop 9

C       Read and check some data values.
        call ufbint(11, r8arr, MXR8PM, MXR8LV,
     +                        nr8lv, 'CLONH A4ME HSMSL QCEVR')
        IF (  ( nr8lv .eq. 1 ) .and.
     +                  ( NINT(r8arr(1,1)*100000) .eq. 10388797 ).and.
     +                  ( NINT(r8arr(2,1)) .eq. 7 ) .and.
     +                  ( NINT(r8arr(3,1)) .eq. 14 ) .and.
     +                  ( IBFMS(r8arr(4,1)) .eq. 1 ) ) THEN
           print *, '        UFBINT -> OK'
           print *, '         IBFMS -> OK'
        ELSE
           print *, '        UFBINT -> FAILED!!'
           print *, '         IBFMS -> FAILED!!'
        ENDIF

C       Read and check a sequence of data values.
        CALL UFBSEQ ( 11, r8arr, MXR8PM, MXR8LV,
     +                        nr8lv, 'TDWPRAOB' )
        IF (  ( nr8lv .eq. 191 ) .and.
     +                  ( NINT(r8arr(8,3)*100) .eq. 29416 ) .and.
     +                  ( NINT(r8arr(10,11)*10) .eq. 55 ) .and.
     +                  ( NINT(r8arr(2,12)) .eq. 2048 ) .and.
     +                  ( NINT(r8arr(5,67)*100000) .eq. -1167 ) .and.
     +                  ( NINT(r8arr(1,186)) .eq. 2523 ) ) THEN
           print *, '        UFBSEQ -> OK'
        ELSE
           print *, '        UFBSEQ -> FAILED!!'
        ENDIF

C       Read and check some long character strings.
        CALL READLC(11, smidstg, 'SMID' )
        CALL READLC(11, softvstg, 'SOFTV' )
        IF ( ( smidstg(7:9) .eq. 'UAO' ) .and.
     +               ( softvstg(5:12) .eq. '5.8.5.10' ) ) THEN
           print *, '        READLC -> OK'
        ELSE
           print *, '        READLC -> FAILED!!'
        ENDIF


C       Close the test file.
        call ccbfl()

        print *, 'SUCCESS!'
        end program
