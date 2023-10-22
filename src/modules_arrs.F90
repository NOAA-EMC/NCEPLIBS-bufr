!> @file
!> @brief Declare module arrays.
!>
!> @author J. Ator @date 2023-02-10

!> Declare arrays and variables used to store
!> BUFR messages internally for multiple file IDs.
!>
!> @author J. Ator @date 2014-12-10
module moda_bitbuf
  !> Maximum length of an output BUFR message.
  integer :: MAXBYT
  !> Bit pointer within IBAY.
  integer :: IBIT
  !> Current data subset.
  integer, allocatable :: IBAY(:)
  !> Length (in bytes) of current BUFR message for each file ID.
  integer, allocatable :: MBYT(:)
  !> Current BUFR message for each internal file ID.
  integer, allocatable :: MBAY(:,:)
end module moda_bitbuf

!> Declare arrays and variables used to store
!> bitmaps internally within a data subset definition.
!>
!> Data values within this module are stored by subprograms strbtm(),
!> igetrfel(), makestab() and tabsub().
!>
!> @author J. Ator @date 2016-05-27
module moda_bitmaps
  !> Number of stored bitmaps for the current data subset (up to a
  !> maximum of MXBTM).
  integer :: NBTM
  !> Number of Table A mnemonics in jump/link table (up to a maximum of
  !> MXTAMC) which contain at least one Table C operator with an XX
  !> value of 21 or greater in their data subset definition; only Table
  !> C operators with an XX value of 21 or greater are tracked within
  !> this module, since all others are automatically processed within
  !> subroutines tabsub() and tabent().
  integer :: NTAMC
  !> Most recent jump/link table entry that was processed by function
  !> igetrfel() and whose corresponding value type was either numeric
  !> or CCITT IA5.
  integer :: LSTNOD
  !> Current count of consecutive occurrences of lstnod.
  integer :: LSTNODCT
  !> true if a bitmap is in the process of being read for the current
  !> data subset; false otherwise.
  logical :: LINBTM
  !> Entries within jump/link table which contain Table A mnemonics.
  integer, allocatable :: INODTAMC(:)
  !> Number of Table C operators (with an XX value of 21 or greater)
  !> within the data subset definition of the corresponding Table A
  !> mnemonic in inodtamc.
  integer, allocatable :: NTCO(:)
  !> Table C operators corresponding to inodtco.
  character*6, allocatable :: CTCO(:,:)
  !> Entries within jump/link table which contain Table C operators.
  integer, allocatable :: INODTCO(:,:)
  !> Number of "set" entries (set to a value of 0) in the bitmap.
  integer, allocatable :: NBTMSE(:)
  !> Ordinal position in data subset definition corresponding to the
  !> first entry of the bitmap.
  integer, allocatable :: ISTBTM(:)
  !> Size of bitmap (total number of entries, whether "set" (set to a
  !> value of 0) or not).
  integer, allocatable :: ISZBTM(:)
  !> Ordinal positions in bitmap of bits that were "set" (set to a
  !> value of 0); these ordinal positions can range in value from 1 to
  !> iszbtm for each stored bitmap.
  integer, allocatable :: IBTMSE(:,:)
end module moda_bitmaps

!> Declare arrays used to store, for each output file ID,
!> a copy of the BUFR message that was most recently written
!> to that ID, for possible later retrieval via subroutine
!> writsa().
!>
!> @author J. Ator @date 2014-12-10
module moda_bufrmg
  !> Length (in integers) of BUFR message most recently written to each
  !> output file ID.
  integer, allocatable :: MSGLEN(:)
  !> BUFR message most recently written to each output file ID.
  integer, allocatable :: MSGTXT(:,:)
end module moda_bufrmg

!> Declare arrays and variables needed to store the
!> current position within a BUFR file. This allows a rewind back to
!> the first message within the file, while retaining the capability
!> to return to the original file position at a later point within the
!> application program.
!>
!> The current position can be stored for only one BUFR file at a time.
!>
!> @author J. Ator @date 2014-12-10
module moda_bufrsr
  !> File ID of BUFR file.
  integer :: JUNN
  !> File status indicator of BUFR file.
  integer :: JILL
  !> Message status indicator of BUFR file.
  integer :: JIMM
  !> Bit pointer within BUFR message.
  integer :: JBIT
  !> Length (in bytes) of BUFR message.
  integer :: JBYT
  !> Sequential number of BUFR message, counting from the beginning of
  !> the file.
  integer :: JMSG
  !> Sequential number of BUFR data subset, counting from the beginning
  !> of the current BUFR message.
  integer :: JSUB
  !> Bit-wise (integer) representation of FXY value associated with
  !> Table A mnemonic for BUFR message.
  integer :: KSUB
  !> Positional index of Table A mnemonic within internal Table A.
  integer :: JNOD
  !> Section 1 date-time of BUFR message.
  integer :: JDAT
  !> Indicator of stack status when entering subroutine rewnbf().
  integer, allocatable :: JSR(:)
  !> BUFR message.
  integer, allocatable :: JBAY(:)
end module moda_bufrsr

!> Declare arrays and variables needed for the
!> storage of data values needed when writing compressed data subsets
!> to a BUFR message for output.
!>
!> These values can only be stored for one
!> compressed BUFR message at any given time.
!>
!> @author J. Woollen @date 2002-05-14
module moda_comprs
  !> Number of data subsets in message.
  integer :: NCOL
  !> Increment used when compressing non-character data values.
  integer(8) :: INCR
  !> Non-character data values for all data subsets in message.
  integer(8), allocatable :: MATX(:,:)
  !> Character data values for all data subsets in message.
  character*(:), allocatable :: CATX(:,:)
end module moda_comprs

!> Declare arrays and variable needed for the
!> storage of data values needed when writing compressed data subsets
!> to a BUFR message for output.
!>
!> These values can only be stored for one
!> compressed BUFR message at any given time.
!>
!> @author J. Woollen @date 2002-05-14
module moda_comprx
  !> Number of data values for each data subset in message.
  integer :: NROW
  !> File ID for output file.
  integer :: LUNC
  !> Number of bytes required to store Sections 0, 1, 2, and 3 of message.
  integer :: KBYT
  !> Flush flag. Set to .true. if a subroutine call was made to force
  !> the writing of the message to the corresponding output file, even
  !> if there may still be room in the message for additional data
  !> subsets. Otherwise set to .false.
  logical :: FLUSH
  !> Write-out flag. Set to .true. if the message needs to be written
  !> to the corresponding output file. Otherwise set to .false.
  logical :: WRIT1
  !> Minimum of each data value across all data subsets in message.
  integer(8), allocatable :: KMIN(:)
  !> Maximum of each data value across all data subsets in message.
  integer(8), allocatable :: KMAX(:)
  !> "Missing" values flag. Set to .true. if at least one occurrence
  !> of this data value is "missing" within any data subset of the
  !> message. Otherwise set to .false.
  logical, allocatable :: KMIS(:)
  !> Number of bits needed to hold the increments for this data value
  !> within each data subset of the message.
  integer, allocatable :: KBIT(:)
  !> Type of each data value:
  !> - 1 Delayed descriptor replication factor.
  !> - 2 Other non-character data.
  !> - 3 Character data.
  integer, allocatable :: ITYP(:)
  !> Bit width of underlying data descriptor as defined within Table B
  !> for each data value.
  integer, allocatable :: IWID(:)
  !> Character data value, if corresponding ityp value is set to 3.
  character*(:), allocatable :: CSTR(:)
end module moda_comprx

!> Declare arrays and variables for the
!> internal Table A mnemonic cache that is used for Section 3 decoding
!> of BUFR messages.
!>
!> Data values within this module are stored by subroutine reads3().
!>
!> @author J. Ator
!> @date 2012-03-02
module moda_dscach
  use modv_maxnc
  use modv_mxcnem
  !> Number of entries in the internal Table A mnemonic cache (up to a
  !> maximum of MXCNEM).
  integer :: NCNEM
  !> Table A mnemonics.
  character*8 :: CNEM(MXCNEM)
  !> Number of child descriptors for the corresponding Table A mnemonic
  !> in cnem.
  integer :: NDC(MXCNEM)
  !> Bit-wise representations of the child descriptors for the
  !> corresponding Table A mnemonic in cnem.
  integer :: IDCACH(MXCNEM,MAXNC)
end module moda_dscach

!> Declare arrays and variables needed to
!> store long character strings (greater than 8 bytes) via subroutine
!> hold4wlc(). These strings can be held for writing into an
!> uncompressed BUFR data subset via future internal calls to
!> subroutine writlc().
!>
!> @author J. Ator @date 2014-02-05
module moda_h4wlc
  use modv_mxh4wlc
  !> Number of long character strings being stored.
  integer :: NH4WLC
  !> File ID for associated output file.
  integer, allocatable :: LUH4WLC(:)
  !> Table B mnemonics associated with long character strings.
  character*14, allocatable :: STH4WLC(:)
  !> Long character strings.
  character*120, allocatable :: CHH4WLC(:)
end module moda_h4wlc

!> Declare an array used by subroutine
!> readerme() to read in a new DX dictionary table as a consecutive
!> set of one or more DX BUFR tables messages.
!>
!> @author J. Ator @date 2009-03-23
module moda_idrdm
  !> DX BUFR tables message count for each file ID.
  !> Set to a value of zero unless a new DX dictionary table is in the
  !> process of being read in by subroutine readerme() for the
  !> associated logical unit, and in which case it keeps track of how
  !> many such messages have been read in so far.
  integer, allocatable :: IDRDM(:)
end module moda_idrdm

!> Declare an array used to pack or
!> unpack all of the values of a BUFR data subset. This array is used
!> when packing a data subset for output via subroutine wrtree(), or
!> when unpacking an input data subset via subroutine rdtree().
!>
!> @author J. Woollen @date 1994-01-06
module moda_ival
  !> BUFR data subset values.
  integer(8), allocatable :: IVAL(:)
end module moda_ival

!> Declare arrays which provide working space in several
!> subprograms (usrtpl() and ufbcup()) which manipulate the contents
!> of internal subset buffers where the contents of an evolving
!> BUFR message are accumulated and stored under user control prior to
!> being written out.
!>
!> @author J. Woollen @date 1994-01-06
module moda_ivttmp
  !> tag array elements for new sections of a growing subset buffer.
  character*10, allocatable :: TTMP(:)
  !> inv array elements for new sections of a growing subset buffer.
  integer, allocatable :: ITMP(:)
  !> val array elements for new sections of a growing subset buffer.
  real*8, allocatable :: VTMP(:)
end module moda_ivttmp

!> Declare an array used by subroutine
!> makestab() to keep track of which logical units share DX BUFR table
!> information.
!>
!> @author J. Woollen @date 1994-01-06
module moda_lushr
  !> Tracking index for each file ID. Set to a value
  !> of zero if the corresponding logical unit does not share DX BUFR
  !> table information with any other logical unit. Otherwise set to a
  !> non-zero value within subroutine makestab().
  integer, allocatable :: LUS(:)
end module moda_lushr

!> Declare an array used by various
!> subroutines and functions to hold a temporary working copy of a
!> BUFR message.
!>
!> @author J. Woollen @date 1994-01-06
module moda_mgwa
  !> Temporary working copy of BUFR message.
  integer, allocatable :: MGWA(:)
end module moda_mgwa

!> Declare an array used by various
!> subroutines and functions to hold a temporary working copy of a
!> BUFR message.
!>
!> @author J. Woollen @date 1994-01-06
module moda_mgwb
  !> Temporary working copy of BUFR message.
  integer, allocatable :: MGWB(:)
end module moda_mgwb

!> Declare arrays used to store
!> information about the current BUFR message that is in the process
!> of being read from or written to the logical unit associated with
!> each file ID.
!>
!> @author J. Woollen @date 1994-01-06
module moda_msgcwd
  !> Current message pointer within logical unit.
  integer, allocatable :: NMSG(:)
  !> Current subset pointer within message.
  integer, allocatable :: NSUB(:)
  !> Total number of data subsets in message.
  integer, allocatable :: MSUB(:)
  !> Table A mnemonic for type of BUFR message.
  integer, allocatable :: INODE(:)
  !> Section 1 date-time of message.
  integer, allocatable :: IDATE(:)
end module moda_msgcwd

!> Declare an array used to keep track
!> of which logical units should not have any empty (zero data subset)
!> BUFR messages written to them. This setting is turned on via a
!> call to subroutine closmg() with LUNIN < 0 for the logical unit in
!> question, and once set it remains in effect for the remainder of
!> the life of the application program for that particular logical
!> unit.
!>
!> @author D. Keyser @date 2005-05-26
module moda_msglim
  !> Tracking index for each file ID. Initialized to a value
  !> of 3 within subroutine bfrini(), and then reset to a value of 0
  !> within subroutine closmg() if the corresponding logical unit
  !> should not have any empty (zero data subset) BUFR messages written
  !> to it.
  integer, allocatable :: MSGLIM(:)
end module moda_msglim

!> Declare arrays and variables used to store
!> the contents of one or more BUFR files within internal memory.
!>
!> Data values within this module are stored by subroutines ufbmem() and cpdxmm().
!>
!> @author J. Ator @date 2014-12-10
module moda_msgmem
  !> Fortran logical unit number for use in accessing contents of BUFR
  !> files within internal memory.
  integer :: MUNIT
  !> Number of array elements filled within msgs (up to a maximum of MAXMEM).
  integer :: MLAST
  !> Number of array elements filled within mdx (up to a maximum of MXDXW).
  integer :: LDXM
  !> Number of DX BUFR table messages stored within mdx (up to a
  !> maximum of MXDXM).
  integer :: NDXM
  !> Number of DX BUFR table that is currently in scope, depending on
  !> which BUFR message within msgs is currently in scope from the most
  !> recent call to subroutine rdmemm() or readmm().
  integer :: LDXTS
  !> Number of DX BUFR tables represented by the messages within mdx
  !> (up to a maximum of MXDXTS).
  integer :: NDXTS
  !> Maximum number of DX BUFR table messages that can be stored within mdx.
  integer :: MXDXM
  !> Maximum number of entries that can be stored within mdx.
  integer :: MXDXW
  !> Pointers to the beginning of each message within msgs (up to a
  !> maximum of MAXMSG, and where array element 0 contains the actual
  !> number of messages stored within msgs).
  integer, allocatable :: MSGP(:)
  !> BUFR messages read from one or more BUFR files.
  integer, allocatable :: MSGS(:)
  !> DX BUFR table messages read from one or more BUFR files, for use
  !> in decoding the messages in msgs.
  integer, allocatable :: MDX(:)
  !> Pointers to the beginning of each message within mdx.
  integer, allocatable :: IPDXM(:)
  !> Pointers to the beginning of each DX BUFR table within mdx.
  integer, allocatable :: IFDXTS(:)
  !> Number of consecutive messages within mdx which constitute each DX
  !> BUFR table, beginning with the corresponding ifdxts.
  integer, allocatable :: ICDXTS(:)
  !> Pointers to first message within msgs for which each DX BUFR table applies.
  integer, allocatable :: IPMSGS(:)
end module moda_msgmem

!> Declare arrays and variables used to store
!> master Table B and Table D entries within internal memory.
!>
!> Data values within this module are stored by subroutine ireadmt().
!>
!> @author J. Ator @date 2014-12-10
module moda_mstabs
  !> Number of master Table B entries (up to a maximum of MXMTBB).
  integer :: NMTB
  !> Number of master Table D entries (up to a maximum of MXMTBD).
  integer :: NMTD
  !> Bit-wise representations of FXY numbers for master Table B.
  integer, allocatable :: IBFXYN(:)
  !> Scale factors corresponding to ibfxyn.
  character, allocatable :: CBSCL(:,:)
  !> Reference values corresponding to ibfxyn.
  character, allocatable :: CBSREF(:,:)
  !> Bit widths corresponding to ibfxyn.
  character, allocatable :: CBBW(:,:)
  !> Units corresponding to ibfxyn.
  character, allocatable :: CBUNIT(:,:)
  !> Mnemonics corresponding to ibfxyn.
  character, allocatable :: CBMNEM(:,:)
  !> Element names corresponding to ibfxyn.
  character, allocatable :: CBELEM(:,:)
  !> Bit-wise representations of FXY numbers for master Table D.
  integer, allocatable :: IDFXYN(:)
  !> Sequence names corresponding to idfxyn.
  character, allocatable :: CDSEQ(:,:)
  !> Mnemonics corresponding to idfxyn.
  character, allocatable :: CDMNEM(:,:)
  !> Numbers of child descriptors corresponding to idfxyn.
  integer, allocatable :: NDELEM(:)
  !> Bit-wise representations of child descriptors corresponding to idfxyn.
  integer, allocatable :: IDEFXY(:)
end module moda_mstabs

!> Declare arrays used by various
!> subroutines to hold information about Table D sequences. The values in
!> these arrays are set via an internal call to subroutine nemtbd().
!>
!> @author J. Woollen @date 1994-01-06
module moda_nmikrp
  !> Child mnemonics within Table D sequences.
  character*8, allocatable :: NEM(:,:)
  !> Replication indicators corresponding to nem:
  !> - 5, if corresponding nem is a Table D mnemonic using 1-bit delayed replication
  !> - 4, if corresponding nem is a Table D mnemonic using 8-bit delayed (stack) replication
  !> - 3, if corresponding nem is a Table D mnemonic using 8-bit delayed replication
  !> - 2, if corresponding nem is a Table D mnemonic using 16-bit delayed replication
  !> - 1, if corresponding nem is a Table D mnemonic using regular (non-delayed) replication
  !> - 0, otherwise
  integer, allocatable :: IRP(:,:)
  !> Replication counts corresponding to nem:
  !> - Number of replications, if corresponding nem is a Table D
  !>   mnemonic using regular (non-delayed) replication
  !> - 0, otherwise
  integer, allocatable :: KRP(:,:)
end module moda_nmikrp

!> Declare arrays and variables for use with
!> any 2-03-YYY (change reference value) operators present within the
!> internal jump/link table.
!>
!> Data values within this module are stored by subroutine tabsub().
!>
!> @author J. Ator @date 2012-03-02
module moda_nrv203
  !> Number of entries in the jump/link table which contain new
  !> reference values (up to a maximum of MXNRV).
  integer :: NNRV
  !> Number of bits in Section 4 occupied by each new reference value
  !> for the current 2-03-YYY operator in scope; set to 0 if no such
  !> operator is currently in scope.
  integer :: IBTNRV
  !> A number between 1 and nnrv, denoting the first entry within the
  !> module arrays which applies to the current data subset in scope;
  !> set to 0 if no 2-03-YYY operators have been applied to the current
  !> data subset in scope.
  integer :: IPFNRV
  !> Table B mnemonic to which the corresponding new reference value in
  !> nrv applies.
  character*8, allocatable :: TAGNRV(:)
  !> Entries within jump/link table which contain new reference values.
  integer, allocatable :: INODNRV(:)
  !> New reference values corresponding to inodnrv.
  integer*8, allocatable :: NRV(:)
  !> Start of entry range in jump/link table, within which the
  !> corresponding new reference value in nrv will be applied to all
  !> occurrences of the corresponding Table B mnemonic in tagnrv.
  integer, allocatable :: ISNRV(:)
  !> End of entry range in jump/link table, within which the
  !> corresponding new reference value in nrv will be applied to all
  !> occurrences of the corresponding Table B mnemonic in tagnrv.
  integer, allocatable :: IENRV(:)
end module moda_nrv203

!> Declare an array used to store a switch for each file ID,
!> indicating whether any BUFR
!> messages should actually be written to the corresponding logical
!> unit.
!>
!> The default value is "on", indicating that BUFR messages should be
!> written to the corresponding logical unit, but it can be switched
!> to a value of "off" if it is connected to the library via a call to
!> subroutine openbf() with IO = 'NUL', and in which case no output
!> will actually be written to the corresponding logical unit. This
!> can be useful if, for example, an application program is planning
!> to use subroutine writsa() to return output BUFR messages via a
!> memory array and doesn't want to also have them automatically
!> written out to a file.
!>
!> @author J. Woollen @date 2003-11-04
module moda_nulbfr
  !> Output switch for each file ID:
  !> - 0 BUFR messages will be written to corresponding logical unit (default)
  !> - 1 no BUFR messages will be written to corresponding logical unit
  integer, allocatable :: NULL(:)
end module moda_nulbfr

!> Declare arrays and variables used to store
!> master Table B and Table D entries within internal memory.
!>
!> Data values within this module are stored by subroutine ireadmt().
!>
!> @author J. Ator @date 2014-12-10
module moda_rdmtb
  !> Bit-wise representations of child descriptors of Table D sequences.
  integer, allocatable :: IEFXYN(:,:)
  !> Descriptor codes for Table B elements.
  character*4, allocatable :: CMDSCB(:)
  !> Descriptor codes for Table D sequences.
  character*4, allocatable :: CMDSCD(:)
  !> Element names corresponding to iefxyn.
  character*120, allocatable :: CEELEM(:,:)
end module moda_rdmtb

!> Declare arrays and variables needed to
!> store information about long character strings (greater than 8
!> bytes) when reading them from input data subsets in compressed BUFR
!> messages via subroutines rdcmps() and readlc().
!>
!> @author J. Woollen @date 2009-03-23
module moda_rlccmn
  !> Number of long character strings in data subset.
  integer :: NRST
  !> Lengths (in bytes) of long character strings.
  integer, allocatable :: IRNCH(:)
  !> Pointers in data subset to first bits of long character strings.
  integer, allocatable :: IRBIT(:)
  !> Table B mnemonics associated with long character strings.
  character*10, allocatable :: CRTAG(:)
end module moda_rlccmn

!> Declare arrays and variables used to store
!> custom values for certain mnemonics within Sections 0 and 1 of all
!> future output BUFR messages written to all Fortran logical units.
!> These values are specified via one or more calls to subroutine
!> pkvs01(). Otherwise, default values for these mnemonics are used as
!> defined within subroutines msgini(), cmsgini(), or dxmini().
!>
!> @author J. Ator @date 2015-03-03
module moda_s01cm
  !> Custom values for use within Sections 0 and 1 of all future output BUFR messages written to all Fortran logical units.
  integer, allocatable :: IVMNEM(:)
  !> Section 0 and 1 mnemonics corresponding to ivmnem.
  character*8, allocatable :: CMNEM(:)
  !> Number of custom values stored.
  integer :: NS01V = 0
end module moda_s01cm

!> Declare an array used to store a switch
!> for each file ID, indicating whether BUFR
!> messages read from the corresponding logical unit should be decoded
!> according to the contents of Section 3 and using master BUFR
!> tables, rather than using DX BUFR tables.
!>
!> The default value is "off", indicating that BUFR messages read from
!> the corresponding logical unit should be decoded using DX BUFR
!> tables, but it can be switched to a value of "on" if the
!> corresponding logical unit is connected to the library via a call
!> to subroutine openbf() with IO = 'SEC3', and in which case each
!> BUFR message will be decoded according to the contents of Section 3
!> and using master BUFR tables.
!>
!> @author J. Ator @date 2009-03-23
module moda_sc3bfr
  !> Section 3 switch for each file ID:
  !> - 0 BUFR messages read from corresponding logical unit will be decoded using DX BUFR tables (default)
  !> - 1 BUFR messages read from corresponding logical unit will be decoded using master BUFR tables
  integer, allocatable :: ISC3(:)
  !> Table A mnemonic most recently read from each file ID, if isc3 = 1 for that stream
  character*8, allocatable :: TAMNEM(:)
end module moda_sc3bfr

!> Declare arrays used to store file and
!> message status indicators for all logical units that have been
!> connected to the library via previous calls to subroutine openbf().
!>
!> @author J. Woollen @date 1994-01-06
module moda_stbfr
  !> File status indicators. Every connected file ID has a
  !> non-zero value in this array:
  !> - if the value is positive, then the logical unit number of this same value is connected for output
  !>   (i.e. writing/encoding) BUFR
  !> - if the value is negative, then the logical unit number of the absolute value of this same value is
  !>   connected for input (i.e. reading/decoding) BUFR
  integer, allocatable :: IOLUN(:)
  !> Message status indicator corresponding to iolun, denoting whether a BUFR message is currently open within the internal
  !> arrays for the corresponding logical unit:
  !> - 0 no
  !> - 1 yes
  integer, allocatable :: IOMSG(:)
end module moda_stbfr

!> Declare an array used to store a status
!> code for each file ID if an error or other
!> abnormal result occurs while processing a BUFR message within the
!> associated logical unit. The status code can be retrieved at any
!> time by the application program via a call to subroutine igetsc().
!>
!> @author J. Ator @date 2010-05-11
module moda_stcode
  !> Abnormal status codes.
  !> - 0 all is normal; no error occurred
  !> - 1 replication factor overflow in subroutine usrtpl()
  integer, allocatable :: ISCODES(:)
end module moda_stcode

!> Declare arrays and variables used to store
!> DX BUFR tables internally for multiple file IDs.
!>
!> @author J. Ator @date 2014-12-10
module moda_tababd
  !> Number of Table A entries for each file ID (up to a
  !> maximum of MAXTBA, whose value is stored in array element 0).
  integer, allocatable :: NTBA(:)
  !> Number of Table B entries for each file ID (up to a
  !> maximum of MAXTBB, whose value is stored in array element 0).
  integer, allocatable :: NTBB(:)
  !> Number of Table D entries for each file ID (up to a
  !> maximum of MAXTBD, whose value is stored in array element 0).
  integer, allocatable :: NTBD(:)
  !> Entries within jump/link table corresponding to taba.
  integer, allocatable :: MTAB(:,:)
  !> Message types (in array element 1) and subtypes (in array element
  !> 2) corresponding to taba.
  integer, allocatable :: IDNA(:,:,:)
  !> Bit-wise representations of the FXY values corresponding to tabb.
  Integer, allocatable :: IDNB(:,:)
  !> Bit-wise representations of the FXY values corresponding to tabd.
  integer, allocatable :: IDND(:,:)
  !> Table A entries for each file ID.
  character*128, allocatable :: TABA(:,:)
  !> Table B entries for each file ID.
  character*128, allocatable :: TABB(:,:)
  !> Table D entries for each file ID.
  character*600, allocatable :: TABD(:,:)
end module moda_tababd

!> Declare arrays and variables used to store
!> the internal jump/link table.
!>
!> Data values within this module are stored by subroutines
!> makestab(), tabsub() and tabent().
!>
!> @author J. Ator @date 2014-12-10
module moda_tables
  !> Maximum number of entries in the jump/link table; equivalent to MAXJL.
  integer :: MAXTAB
  !> Number of entries in the jump/link table.
  integer :: NTAB
  !> Mnemonics in the jump/link table.
  character*10, allocatable :: TAG(:)
  !> Type indicators corresponding to tag:
  !> - "SUB", if corresponding tag entry is a Table A mnemonic
  !> - "SEQ", if corresponding tag entry is a Table D mnemonic using either short (1-bit) delayed replication, regular
  !>   (non-delayed) replication, or no replication at all
  !> - "RPC", if corresponding tag entry is a Table D mnemonic using either medium (8-bit) or long (16-bit) delayed
  !>   replication
  !> - "RPS", if corresponding tag entry is a Table D mnemonic using medium (8-bit) delayed replication in a stack
  !> - "DRB", if corresponding tag entry denotes the short (1-bit) delayed replication of a Table D mnemonic
  !> - "DRP", if corresponding tag entry denotes the medium (8-bit) or long (16-bit) delayed replication of a Table D
  !>   mnemonic
  !> - "DRS", if corresponding tag entry denotes the medium (8-bit) delayed replication of a Table D mnemonic in a stack
  !> - "REP", if corresponding tag entry denotes the regular (non-delayed) replication of a Table D mnemonic
  !> - "CHR", if corresponding tag entry is a Table B mnemonic with units of CCITT IA5
  !> - "NUM", if corresponding tag entry is a Table B mnemonic with any units other than CCITT IA5
  character*3, allocatable :: TYP(:)
  !> Temporary storage used in calculating delayed replication counts.
  integer, allocatable :: KNT(:)
  !> Jump forward indices corresponding to tag and typ:
  !> - 0, if corresponding typ entry is "CHR" or "NUM"
  !> - Jump/link table entry for Table D mnemonic whose replication is denoted by corresponding tag entry, if corresponding
  !>   typ entry is "DRB", "DRP" or "REP"
  !> - Jump/link table entry for Table B or D mnemonic which is the first sequential child descriptor of the corresponding
  !>   tag entry, otherwise
  integer, allocatable :: JUMP(:)
  !> Link indices corresponding to tag, typ and jmpb:
  !> - 0, if corresponding typ entry is "SUB" or "RPC", or if corresponding typ entry is "SEQ" and corresponding tag entry
  !>   uses either short (1-bit) or regular (non-delayed) replication, or if corresponding tag entry is the last sequential
  !>   child descriptor of the Table A or D mnemonic referenced by corresponding jmpb entry
  !> - Jump/link table entry for Table B or D mnemonic which follows the corresponding tag entry as the next sequential
  !>   child descriptor of the Table A or D mnemonic referenced by corresponding jmpb entry, otherwise
  integer, allocatable :: LINK(:)
  !> Jump backward indices corresponding to tag and typ:
  !> - 0, if corresponding typ entry is "SUB"
  !> - Jump/link table entry denoting the replication of corresponding tag entry, if corresponding typ entry is "RPC", or
  !>   if corresponding typ entry is "SEQ" and corresponding tag entry uses either short (1-bit) or regular (non-delayed)
  !>   replication
  !> - Jump/link table entry for Table A or D mnemonic of which corresponding tag entry is a child descriptor, otherwise
  integer, allocatable :: JMPB(:)
  !> Bit widths corresponding to tag and typ:
  !> - Bit width of corresponding tag entry, if corresponding typ entry is "CHR", "NUM", "DRB" or "DRP"
  !> - 0, otherwise
  integer, allocatable :: IBT(:)
  !> Reference values corresponding to tag and typ:
  !> - Reference value of corresponding tag entry, if corresponding typ entry is "NUM"
  !> - Number of regular (non-delayed) replications of Table D mnemonic referenced by corresponding jump entry, if
  !>   corresponding typ entry is "REP"
  !> - 0, otherwise
  integer, allocatable :: IRF(:)
  !> Scale factors corresponding to tag and typ:
  !> - Scale factor of corresponding tag entry, if corresponding typ entry is "NUM"
  !> - Jump/link table entry for Table B or Table D mnemonic which is the last sequential child descriptor of the
  !>   corresponding tag entry, if the corresponding typ entry is "SUB"
  !> - 0, otherwise
  integer, allocatable :: ISC(:)
  !> Integer type values corresponding to typ:
  !> - 1, if corresponding typ entry is "DRS", "DRP" or "DRB"
  !> - 2, if corresponding typ entry is "NUM"
  !> - 3, if corresponding typ entry is "CHR"
  !> - 0, otherwise
  integer, allocatable :: ITP(:)
  !> Initialized data values corresponding to typ:
  !> - Current placeholder value for "missing" data, if corresponding typ entry is "REP", "NUM" or "CHR"
  !> - 0, otherwise
  real*8, allocatable :: VALI(:)
  !> Initialized replication counts corresponding to typ and jump:
  !> - 0, if corresponding typ entry is "RPC", "RPS" or "DRB"
  !> - Number of regular (non-delayed) replications of Table D mnemonic referenced by corresponding jump entry, if
  !>   corresponding typ entry is "REP"
  !> - 1, otherwise
  integer, allocatable :: KNTI(:)
  !> Temporary storage used in expanding sequences.
  integer, allocatable :: ISEQ(:,:)
  !> Temporary storage used in expanding sequences.
  integer, allocatable :: JSEQ(:)
end module moda_tables

!> Declare an array used to store, for each file ID,
!> the logical unit number corresponding to a
!> separate file ID whenever BUFR data subsets are being
!> copied from the latter to the former via subroutine ufbcpy(). In
!> such cases, this stored logical unit number is later accessed
!> within subroutine wrtree() to enable the copying of long character
!> strings (greater than 8 bytes) between the two logical units.
!>
!> @author J. Woollen @date 2009-08-11
module moda_ufbcpl
  !> Logical unit numbers used to copy long character strings between
  !> BUFR data subsets.
  integer, allocatable :: LUNCPY(:)
end module moda_ufbcpl

!> Declare an array used to store, for each file ID
!> from which a BUFR message is currently being read
!> as input, a flag indicating how to unpack the data subsets from the
!> message.
!>
!> @author J. Woollen @date 1994-01-06
module moda_unptyp
  !> Flag indicating how to unpack data subsets from BUFR message:
  !> - 0 message contains data subset byte counters and other non-standard enhancements
  !> - 1 message is fully standard and contains no non-standard enhancements
  !> - 2 message is compressed
  integer, allocatable :: MSGUNP(:)
end module moda_unptyp

!> Declare arrays for internal storage of
!> pointers to BUFR data subset values. These are used when unpacking
!> an input data subset via subroutines rcstpl() and rdtree().
!>
!> @author J. Woollen @date 1994-01-06
module moda_usrbit
  !> Length (in bits) of each packed data value in data subset.
  integer, allocatable :: NBIT(:)
  !> Pointer in data subset to first bit of each packed data value.
  integer, allocatable :: MBIT(:)
end module moda_usrbit

!> Declare arrays used to store data
!> values and associated metadata for the current BUFR data subset in
!> scope for each file ID.
!>
!> @author J. Woollen @date 1994-01-06
module moda_usrint
  !> Number of data values in BUFR data subset.
  integer, allocatable :: NVAL(:)
  !> Inventory pointer which links each data value to its corresponding node in the internal jump/link table.
  integer, target, allocatable :: INV(:,:)
  !> Referenced data value, for data values which refer to a previous data value in the BUFR data subset via an internal
  !> bitmap.
  integer, allocatable :: NRFELM(:,:)
  !> Data values.
  real*8, target, allocatable :: VAL(:,:)
end module moda_usrint

!> Declare arrays used in subroutine rcstpl() to store
!> subset segments that are being copied from a subset template into
!> internal subset arrays.
!>
!> @author J. Woollen 1994-01-06
module moda_usrtmp
  parameter ( MAXRCR = 100 )
  !> inv array elements for new sections of a growing subset buffer.
  integer, allocatable :: IUTMP(:,:)
  !> val array elements for new sections of a growing subset buffer.
  real*8, allocatable :: VUTMP(:,:)
end module moda_usrtmp

!> Declare an array used to track, for each file ID,
!> whether the DX BUFR table associated with the
!> corresponding logical unit has changed during the life of the
!> application program.
!>
!> This information is stored and tracked within subroutine
!> makestab(), which checks each time it is called to see if the DX
!> BUFR table has changed for any file ID since the last time
!> it was called.
!>
!> @author J. Woollen @date 2009-03-18
module moda_xtab
  !> Tracking index for each file ID. Set to
  !> .true. if the DX BUFR table for the corresponding logical unit has
  !> changed since the previous call to subroutine makestab(); set to
  !> .false. otherwise.
  logical, allocatable :: XTAB(:)
end module moda_xtab
