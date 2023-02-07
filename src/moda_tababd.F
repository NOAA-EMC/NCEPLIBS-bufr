C> @file
C> @brief Declare arrays for internal storage of DX BUFR tables.

C> This module contains array and variable declarations used to
C> store DX BUFR tables internally for multiple I/O streams.
C>
C> @author J. Ator
C> @date 2014-12-10

        MODULE MODA_TABABD

C>        Number of Table A entries for each internal I/O stream
C>        (up to a maximum of MAXTBA, whose value is stored in
C>        array element 0).
          INTEGER, ALLOCATABLE :: NTBA(:)
C>
C>        Number of Table B entries for each internal I/O stream
C>        (up to a maximum of MAXTBB, whose value is stored in
C>        array element 0).
          INTEGER, ALLOCATABLE :: NTBB(:)
C>
C>        Number of Table D entries for each internal I/O stream
C>        (up to a maximum of MAXTBD, whose value is stored in
C>        array element 0).
          INTEGER, ALLOCATABLE :: NTBD(:)
C>
C>        Entries within jump/link table corresponding to taba.
          INTEGER, ALLOCATABLE :: MTAB(:,:)
C>
C>        Message types (in array element 1) and subtypes (in array
C>        element 2) corresponding to taba.
          INTEGER, ALLOCATABLE :: IDNA(:,:,:)
C>
C>        Bit-wise representations of the FXY values corresponding
C>        to tabb.
          INTEGER, ALLOCATABLE :: IDNB(:,:)
C>
C>        Bit-wise representations of the FXY values corresponding
C>        to tabd.
          INTEGER, ALLOCATABLE :: IDND(:,:)
C>
C>        Table A entries for each internal I/O stream.
          CHARACTER*128, ALLOCATABLE :: TABA(:,:)
C>
C>        Table B entries for each internal I/O stream.
          CHARACTER*128, ALLOCATABLE :: TABB(:,:)
C>
C>        Table D entries for each internal I/O stream.
          CHARACTER*600, ALLOCATABLE :: TABD(:,:)

        END MODULE
