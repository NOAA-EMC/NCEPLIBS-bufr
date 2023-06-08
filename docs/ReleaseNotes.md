
# Release Notes
@brief Detailed description of changes included within each new release.

## Version 12.0.0 - June 8, 2023

* The library has been consolidated into a single build using 4-byte
integers.  It can still be linked to Fortran application codes which are compiled
using 8-byte integers; however, such codes must now include a call to new
subroutine setim8b() with a value of .true. before making any calls to
any other library routines.  Furthermore, since all library functions
which return integer values (e.g. iupbs01(), isetprm(), ibfms()) will now
return 4-byte integer values, Fortran application codes which are compiled
using 8-byte integers must now also explicitly declare such functions
as 4-byte integers before calling them.
[[Issue #78](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/78)]

* The interface between the C and Fortran components of the library has
been modernized.  Accordingly, Fortran application codes must now contain a
'<b>use bufr_interface</b>' statement to directly call any C functions within
the library, and C application codes must now contain an
'<b>#&#8203;include "bufr_interface.h"</b>' statement to directly call any Fortran
or C functions within the library.
[[Issue #79](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/79)]

* Subroutines ufbqcd() and ufbqcp() have been modified to pass call
arguments containing event program codes as integers, rather than
continuing to pass them as real numbers.
[[Issue #78](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/78)]

* An extension was added to support the query interface for C++ IODA
converters.
[[Issue #451](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/451)]

* Support for legacy EBCDIC platforms has been removed.
[[Issue #266](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/266)]

* The library has been patched to exit gracefully from subroutine readsb() if
an erroneous delayed replication factor is read within subroutine usrtpl().
[[Issue #495](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/495)]

* The library has been cleaned up to eliminate a number of compiler warnings.
[[Issue #300](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/300)]

* Documentation has been improved throughout the library, including the
use of Doxygen-style docblocks for all program files.
[[Issue #246](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/246)]


## Version 11.7.1 - August 26, 2022

* More extensions were added to support the query interface for C++ IODA
converters.
[[Issue #225](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/225)]

* Several internal routines were modified to allow the encoding and
decoding of values larger than 32 bits.
[[Issue #195](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/195)]


## Version 11.7.0 - May 19, 2022

* Extensions were added to support a new query interface for C++ IODA
converters.
[[Issue #198](https://github.com/NOAA-EMC/NCEPLIBS-bufr/pull/198)]

* Updates and bug fixes were made to several utility programs within
the library package.
[[Issue #141](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/141)]

* A fix was made to prevent a line truncation error which occurs with
certain compilers when the value of the MASTER_TABLE_DIR macro exceeds a
certain length.
[[Issue #182](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/182)]


## Version 11.6.0 - November 10, 2021

* All of the library builds now use dynamic allocation, so the "_DA" suffix
has now been correspondingly removed from the names of the library builds.
[[Issue #77](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/77)]

* The default BUFR master table version number for output BUFR messages was
changed from 29 to 36 within subroutines cmsgini(), dxmini() and msgini().
[[Issue #142](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/142)]

* The maximum length of a [path/]filename that can be passed into function
cobfl() was increased to 200 characters.
[[Issue #174](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/174)]

* Subroutine ufbpos() was modified to allow it to work for all types of
BUFR messages, including compressed messages.
[[Issue #170](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/170)]

* The library was modified to read up to 24 characters from the units field
of BUFR master Table B files.  Furthermore, in accordance with updated
guidance from WMO, all fields within all BUFR master table files are now
restricted to the CCITTIA5 character set.
[[Issue #140](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/140)]

* Subroutine jstchr and function rjust have been removed from the library,
and their usage has been replaced with the Fortran intrinsic functions
adjustl and adjustr.
[[Issue #165](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/165)]

* Several prototypes were added to the bufrlib.h header file to enable
further interoperability with C application programs.  In addition, several
existing prototypes were modified to use size_t when passing string lengths
between Fortran and C.
[[Issue #164](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/164)]


## Version 11.5.0 - April 26, 2021

* Subroutine ufbseq() was modified for cases where the number of available
levels exceeds the amount of user-provided array space when reading from an
input file.  Previously, the software would abort in such cases without
returning any data whatsoever, but with the change it will now print a
diagnostic and return with the number of levels equal to the amount of array
space that was provided. [[Issue #52](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/52)]

* Subroutine ufdump() was modified to improve the logic for sequence tracking,
and to increase the format width for integer code figure and bit number values
when printing associated meaning strings.
[[Issue #55](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/55)]

* Subroutine ufbdmp() was modified to fix a bug when checking for the "missing"
value in long character strings (i.e. longer than 8 bytes) and to enable
printing of up to 120 characters for such strings.
[[Issue #55](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/55)]

* Subroutine readlc() was modified to return a "missing" character string if
the requested mnemonic isn't found in the subset.  Previously, the subroutine
would return a string of all blank characters in such cases.
[[Issue #53](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/53)]

* Subroutine writlc() was modified to print a warning message if the requested
mnemonic isn't found in the subset definition.  The library would previously
abort in such cases. [[Issue #53](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/53)]

* The default directory location of the BUFR master tables in subroutine bfrini()
was changed from a fixed WCOSS-specific path.  It is now set to the defined
value of the MASTER_TABLE_DIR macro when the library was built.
[[Issue #123](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/123)]

* Subroutine writsa() was modified to allow the return of up to two BUFR
messages during the same call to the subroutine, in the rare instances where
more than one BUFR message could become available during such a call.
[[Issue #54](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/54)]

* Several internal Fortran variable declarations were modified for
compatibility with GNU v10+ compilers.
[[Issue #81](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/81)]

* Subroutines pkx and chrtrn are no longer used within the library and were
never intended to be called from application codes, so they have now been
removed from the library.
[[Issue #107](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/107)]


## Version 11.4.0 - November 20, 2020

* A Python API was added to the library, for use with Python applications.
[[Issue #61](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/61)]


## Version 11.3.2 - July 16, 2020

* A user-friendly sanity check was added to subroutine closbf(), in case an
application program is using a dynamic-allocation build of the library and
calls this subroutine prior to calling subroutine openbf().  The library would
previously abort in such cases, but it will now just print a warning message.
[[Issue #9](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/9)]


## Version 11.3.1 - March 3, 2020

* Subroutine stndrd() was patched to fix an internal calculation for messages
containing only one subset. [[Issue #51](https://github.com/NOAA-EMC/NCEPLIBS-bufr/issues/51)]


## Version 11.3.0 - May 21, 2019

* A bug was fixed in the bitmap processing, so that each Table A mnemonic in
the jump/link table now tracks its own individual set of underlying Table C
operators.  This involved changes to function igetrfel() and subroutines tabsub()
and makestab().

* The build script makebufrlib.sh was modified to remove calls to cpp (the C
preprocessor) for all *.F Fortran files.  Preprocessing for these files is now
done by fpp (the Fortran preprocessor), which is automatically called by the
Fortran compiler for all *.F files.

* Global variable MXRST (the maximum number of long character strings that
can be read from a subset of a compressed BUFR message) has now been added to
the list of array size limits that can be configured using dynamic allocation.

* The build script makebufrlib.sh was modified to stop generating the s_64
("supersize") build.  Such builds are now obsolete, since users can use the
dynamic allocation builds to flexibly define whatever size limits they need
for large BUFR datasets.  If users need to recreate the exact specifications
that were present in the previous s_64 build, they can do so using a dynamic
allocation build and separate calls to subroutine isetprm() to set MXLCC=12,
MAXMEM=75000000, MXCDV=50000, MAXJL=128000, MAXSS=150000, MXMSGL=2500000 and
MXRST=500 within their application program.

* A new capability was added to read and process master code and flag tables.
This feature is activated via an initial call to new subroutine codflg() at any
time after the first call to subroutine openbf().  Once that is done, then for
any BUFR message read into the library via subroutine readmg(), readerme() or
equivalent, and for any mnemonic contained within that message which is defined
as a code or flag table, the user may call new subroutine getcfmng() with that
mnemonic and an associated value (code figure or bit number) as input, and
the subroutine will return the meaning (as a character string) corresponding
to that mnemonic and associated value.

* The default BUFR master table version number was changed from "13" to "29"
within subroutines cmsgini(), dxmini() and msgini().

* The maximum number of delayed replication factors that can be passed in as
input to subroutine drfini() was increased from 200 to 2000.

* Subroutines msgupd() and cpyupd() were modified so that the IPRT verbosity must
be greater than or equal to 1 in order for diagnostic alerts to be printed
whenever BUFR subsets greater than 65535 bytes are written to their own output
messages.

* Function i4dy() was modified to change the window for converting 2-digit years
to 4-digit years.  The old window was 1921-2020, i.e. add 1900 to any 2-digit
year greater than 20; otherwise add 2000.  The new window will be 1941-2040,
i.e. add 1900 to any 2-digit year greater than 40; otherwise add 2000.


## Version 11.2.0 - April 11, 2017

* Support was added for the processing of 2-2X-255, 2-3X-255 and 2-4X-255
marker operators when reading BUFR messages, including when such operators
are included in sequences within a BUFR DX table.

* A new subroutine gettagre() was added which, given an element within a
subset open for reading, determines if the element references another element
within the same subset via an internal bitmap, and if so returns the
referenced element along with its location in the subset.

* A new function igetmxby() was added which returns the maximum size of a BUFR
message that can be written to any output stream by the BUFRLIB software.
This value can then be modified via a subsequent call to subroutine maxout().

* Subroutine nemtbd() was modified to allow the last mnemonic in a sequence to
be a "following value" mnemonic, for cases such as when it is being used as
a coordinate descriptor and set to a value of "missing" to cancel a previous
instance of the same descriptor.

* A new subroutine setvalnb() was added which searches for a specified mnemonic
in a subset definition, then searches forward or backward from that point for
a different mnemonic and sets the associated value as specified by the user.
It can be useful in certain application codes which write BUFR output.

* Subroutines rdtree() and rcstpl() were modified to automatically identify
certain cases where a decoded subset is corrupt and return this information
to the application program via readsb() or ireadsb(), rather than continuing to
try to decode the message which could in turn lead to a segmentation fault.

* The build script makebufrlib.sh was updated to add a missing compiler option
needed for compatibility with the Cray programming environment on the s_64
("supersize") build.

* A bug was fixed in subroutine ufbtab() which could occasionally lead to array
maximums being exceeded and corrupted output when reading BUFR files.


## Version 11.1.0 - April 27, 2016

* Subroutine wrcmps() was modified to fix a bug involving the encoding of long
character strings (via subroutine writlc()) into compressed messages which also
contain delayed replication sequences.

* Subroutine msgupd() was modified to always call subroutine usrtpl(), even for
overlarge subsets which don''t get written to the output stream.  This ensures
that such subsets are properly flushed from internal arrays.


## Version 11.0.2

* The build script makebufrlib.sh was updated for compatibility with the Cray
programming environment, along with module MODA_MSTABS and functions icbfms()
and isize().  All changes remain compatible with other supported environments.


## Version 11.0.1

* Subroutines cpyupd(), ufbmem(), ufbmex() and ufbovr() were patched to include the
proper declaration for IPRT, which is a global variable controlling the
verbosity of diagnostic output.

* Subroutine ufdump() was modified to include level identifiers for event
stacks in the print output.

* Subroutine wrcmps() was modified to fix a bug involving a variable that was
not being saved between successive calls to this subroutine and which in rare
cases could result in the loss of output subsets.


## Version 11.0.0 - April 27, 2015

* A new subroutine rtrcptb() was added which works just like subroutine rtrcpt(),
except that it operates on a BUFR message passed directly to it by a call
argument, rather than on the last BUFR message that was indirectly read during
the previous call to subroutine readmg(), readmm(), readerme() or equivalent.
Subroutine rtrcpt() was correspondingly modified to now directly call rtrcptb(),
to avoid maintaining the same program logic within two different subroutines.

* The maximum number of delayed replication factors that can be passed in as
input to subroutine drfini() was increased from 100 to 200.

* Subroutine ufbseq() was modified to fix a bug involving nesting of delayed
replication sequences.  The subroutine would fail to store data properly in
cases where the inner-nested sequence was not present (i.e. zero replications)
within the first replication of the outer sequence.

* The makebufrlib.sh build script was modified to automatically extract the
version number for the current build from the source of subroutine bvers().
Previously this information had been hardcoded in multiple lines of the script.

* The default directory location of the BUFR master tables in subroutine bfrini()
was changed from "/nwprod/fix" to "/nwprod/decoders/decod_shared/fix".

* A new subroutine nemspecs() was added which returns the scale factor, reference
value and bit width corresponding to a specified occurrence of a given mnemonic
within a subset definition, including accounting for any Table C operators such
as 2-01-YYY, 2-02-YYY, 2-03-YYY, 2-07-YYY, 2-08-YYY, etc. which may be in effect
for that particular occurrence of the mnemonic.

* A new subroutine nemdefs() was added which returns the element definition and
units associated with a given mnemonic.

* Global parameter MXIMB was removed from the BUFRLIB, and the corresponding
logic within subroutine mvb() was simplified to mitigate the need for it.

* Subroutines msgupd(), cpyupd() and copysb() were modified to ensure that any subset
larger than 65530 bytes is written to its own message in the output stream. This
ensures that the subset byte count indicator, which is an NCEP local descriptor
packed into 16 bits prior to each subset in a message, will not need to be
relied upon in order to locate any subsequent subsets within the same message.

* Global parameter MAXSS (the maximum number of data values that can be read
from or written into a subset by the BUFRLIB software) was increased from
80000 to 120000.

* Subroutine RDMSGB, which was rendered obsolete with the addition of the
embedded C-language I/O upgrade in version 10.2.0, has been removed.

* A logical call argument was added to subroutine upc() to let the calling
subprogram specify whether null characters should be converted to blanks.
Previously, all null characters were converted to blanks by default.

* New versions of all of the normal builds of BUFRLIB have been added which
allow certain array sizes to be specified by the user at run time, with the
corresponding arrays dynamically allocated at run time rather than statically
allocated at compile time.  The new builds contain the suffix "_DA" and can
be used by application programs which need flexibility in defining size limits
for certain outlier BUFR datasets.  The size limits that can be modified are
defined within new subroutine isetprm(), which must itself be called prior to the
first call to subroutine openbf() for each new size limit that is to be modified
from its default value.  The corresponding arrays are then dynamically allocated
during the subsequent first call to openbf().  As part of this enhancement,
numerous subprograms within BUFRLIB have been rewritten to share memory using
FORTRAN modules rather than common blocks, and new conditional compilation flags
have been incorporated into these subprograms as well as to the makebufrlib.sh
build script.  This allows the same source code and build script to be used for
both dynamic allocation and static allocation builds.  Note that application
programs which don't have a need to redefine any default array size limits
may continue to use the existing static allocation builds for maximum runtime
efficiency.

* A new subroutine pkx() was added which works just like subroutine pkb(), except
that it properly handles cases where the input value NBITS is greater than the
number of bits in a machine word.

* Support was added for the processing of 2-2X, 2-3X and 2-4X non-marker
operators when reading or writing BUFR messages, including when these operators
are included in sequences within a BUFR DX table.

* Subroutine stseq() was modified to fix a bug involving the application of
associated fields to Table D sequence descriptors.

* Function icbfms() was modified to improve the logic for identifying "missing"
strings encoded as REAL*8 10E10 values prior to version 10.2.0 of the library.


## Version 10.2.5

* Subroutine mesgbf() was modified to ensure that the input BUFR file is
always closed before exiting the subroutine.

* Function cobfl() was modified to allow up to 500 characters in the path of
the filename being opened.

* A declaration typo was fixed in subroutine blocks().

* Global parameter MAXNC (the maximum number of FXY descriptors that can be
written into Section 3 of a BUFR message) was increased from 300 to 600.


## Version 10.2.4

* Configuration files bufrlib.PRM and makebufrlib.sh were updated to
generate a 4_32 build (4-byte REAL, 4-byte INT, 32-bit compilation) on
the IBM CCS for version 10.2.3 of the BUFRLIB.


## Version 10.2.3 - June 21, 2013

* Subroutine rdusdx() was modified to prevent a segfault when trying to read
DX dictionary information from an empty file.


## Version 10.2.2

* Subroutine openbf() was modified to fix a bug which caused a segfault in
certain cases when appending to a BUFR file using the embedded C-language I/O.

* Subroutines readlc() and writlc() were modified to allow the input mnemonic
string to be up to 14 characters when it contains a '#' condition code.


## Version 10.2.1

* A bug was fixed in the embedded C-language I/O to account for the
difference in index numbering between Fortran and C arrays.


## Version 10.2.0 - October 19, 2012

* The makebufrlib.sh script was modified to streamline the endianness check
and make it more portable.

* Subroutine wrtree() was modified to ensure that "missing" character strings
are properly encoded with all bits set to 1.

* A new function icbfms() was added which tests whether decoded character
strings are "missing" by checking if all of the equivalent bits are set to 1.
This was done because, on certain platforms, the BUFRLIB REAL*8 "missing" value
BMISS is not always equivalent to all bits set to 1 when viewed as a character
string, and thus the existing BUFRLIB function ibfms() did not always work
properly in such cases.  However, users can continue to use the existing IBFMS
function in application programs, because the new icbfms() function has now been
incorporated internally within the logic of many BUFRLIB subroutines, in
addition to also being available for direct calling by application programs.

* Subroutines readmg() and readerme() were modified to prevent the BUFRLIB from
internally adjusting to DX (dictionary) table messages when Section 3 decoding
is being used.  Otherwise, contention can occur between the table information
in the DX messages and the table information specified within the Section 3
descriptors.  From now on, whenever Section 3 decoding is used (as specified
by setting IO="SEC3" when opening a file via openbf()), the BUFRLIB will now
treat any DX (dictionary) table message the same as any other message and
decode the actual data (i.e. table) values according to Section 3.

* Subroutine openbf() was modified to allow a new option for input call
argument IO.  If this argument is set to 'INUL', then the BUFRLIB will behave
the same as when IO='IN', except that it will never try to actually read
anything from the file attached to input call argument LUNIT.  This can be
useful for some special cases, such as when the user plans to pass input
messages to the BUFRLIB using subsequent calls to subroutine readerme().

* A new subroutine gettagpr() was added which returns the mnemonic corresponding
to a parent sequence in a subset definition, given the mnemonic corresponding
to a child descriptor within that sequence.  This can be useful in certain
application codes, especially when Section 3 decoding is being used.

* A new function getvalnb() was added which searches for a specified mnemonic
in a subset definition, then searches forward or backward from that point for
a different mnemonic and returns the associated value.  This can be useful in
certain application codes, especially when Section 3 decoding is being used.

* Functionality was added to improve the portability of reading and writing
BUFR messages across different platforms.  All calls to existing FORTRAN
subroutines which read or write BUFR messages from disk (e.g. readmg(), ufbmem(),
ufbtab(), writsb(), wrcmps(), copymg(), etc.) now use embedded C-language I/O to
perform these tasks.  Among other things, this means that any BUFR file can
now be read regardless of whether it has been pre-blocked with FORTRAN
control words using the cwordsh utility.  For writing BUFR files, a new
subroutine setblock() was added which allows users to specify whether output
BUFR messages are to be unblocked (which is the new default), big-endian
blocked, or little-endian blocked.

* A new subroutine setbmiss() was added which allows users to specify a custom
"missing" value for writing to and reading from BUFR files, rather than using
the BUFRLIB default "missing" value of 10E10.  A corresponding function
getbmiss() was also added which returns the current "missing" value in use.


## Version 10.1.0 - June 11, 2012

* Subroutine ufdump() was modified to fix a bug when checking for the "missing"
value in long character strings (i.e. longer than 8 bytes).

* A new subroutine ufbmex() was added for use with certain application
programs.  ufbmex() functions similarly to ufbmem(), but has an additional return
argument containing an array of message types corresponding to the array of
messages that were read into internal memory.

* Subroutines ADDATE, IUPBS1, IUPVS1, LJUST, LSTRPC, LSTRPS, SUBUPD, POSAPN
and PARSEQ, which had been marked as obsolete within a previous version of
BUFRLIB, have now been deleted.

* Several global parameters were increased in "bufrlib.PRM".
Specifically, MAXTBA (the maximum number of Table A entries for a BUFR file)
was increased from 120 to 150, and MXDXTS (the maximum number of dictionary
tables that can be stored for use with BUFR messages in internal memory) was
increased from 10 to 200.

* Subroutine conwin() was modified to fix a bug and remove an obsolete call
argument.

* Subroutine wrcmps() was modified to fix a bug involving embedded tables
within a file of compressed BUFR messages.

* Documentation was improved in many subroutines throughout the library.

* Support has been added for the 2-03-YYY "change reference values" operator.

* Subroutine usrtpl() was modified to fix a bug that was incorrectly using
parameter MAXJL instead of parameter MAXSS when checking for overflow of an
internal template array.

* Subroutine wrdxtb() was modified to prevent it from trying to store more
than 255 Table A, Table B or Table D descriptors in a single DX dictionary
message.  The maximum value was set to 255 since regular 8-bit delayed
replication is used to store descriptor information in these messages.

* Subroutine tabsub() was modified to correctly generate the jump/link table
for subsets where a Table C operator immediately follows a Table D sequence.


## Version 10.0.1

* Subroutine rewnbf() was modified to fix a bug which skipped the first data
message after a file rewind.


## Version 10.0.0 - August 12, 2010

* Subroutines PKVS1, OVRBS1, NMBYT, READIBM, IREADIBM, READFT, IREADFT and
MOVA2I, which had been marked as obsolete within a previous version of
BUFRLIB, have now been deleted.

* A new global parameter MAXSS was defined for use as the maximum number of
data values that can be read from or written into a single data subset by the
BUFRLIB software.  Previously, the separate global parameter MAXJL was used
to define this limit.  MAXJL will now be used solely to define the maximum
number of internal jump/link table entries.

* The size of a string declaration was increased within subroutine rdusdx().

* Subroutine readlc() was modified to enable the extraction of "long" (i.e.
greater than 8 bytes) character strings from compressed messages.  In
addition, it is now possible to access all occurrences of such a string
from within a given data subset, via the use of the new mnemonic condition
character '#'.  Previously, readlc() could only ever access the first
occurrence of any "long" character string from within a data subset.

* Subroutine writlc() was modified to allow the writing of "long" (i.e.
greater than 8 bytes) character strings within compressed messages.  In
addition, it is now possible to write all occurrences of such a string into
a given data subset, via the use of the new mnemonic condition character '#'.
Previously, writlc() could only ever locate and write the first occurrence of
any "long" character string within a data subset.

* Subroutine ufdump() was modified to label each output level for sequences
where the replication count is greater than 1.  In addition, it will now
output all occurrences of "long" (i.e. greater than 8 bytes) character
strings from within a given data subset.

* Subroutine rdcmps() was modified to fix a bug which could cause the overflow
of internal arrays when working with long character strings (i.e. longer
than 8 bytes).

* Subroutine nvnwin() was modified to fix a bug which could cause the overflow
of an internal array during initialization on certain operating systems.

* A new subroutine bvers() was added as a resource for managing and reporting
library version numbers.

* The fuzziness threshold in function ibfms() was increased for improved
accuracy when testing for the BUFRLIB "missing" value.

* A new subroutine iupbs3() was added which unpacks specified values from
Section 3, including subset counts and compression indicators.  The same
logic had been repeated within numerous existing subroutines throughout
BUFRLIB and has now been consolidated into this single subroutine that can
itself be called from wherever it is needed.

* Subroutines readerme(), rdmsgw() and RDMSGB were modified to prevent the
overflow of an internal array for extremely large BUFR messages.

* Subroutine upds3() was modified to pass in a new input argument containing
the dimensioned size of the output array, in order to prevent the subroutine
from possibly overflowing the array.

* Subroutine writsa() was modified to pass in a new input argument containing
the dimensioned size of the output array, in order to prevent the subroutine
from possibly overflowing the array.

* A new capability was added to BUFRLIB to enable the decoding of a BUFR
message according to its data description section (Section 3).  This is
activated by setting IO="SEC3" when opening the file via subroutine openbf().
Master tables containing all possible BUFR descriptors are also required, and
these may be specified via a call to new subroutine mtinfo() or by using default
values specified within subroutine bfrini().  If the default values are used,
then FORTRAN logical unit numbers 98 and 99 will be allocated by the BUFRLIB
for opening and reading the master tables.  This new capability allows BUFR
messages to be decoded without pre-defined DX dictionary files.

* Subroutine readmm() was re-written to directly call subroutine rdmemm()
instead of duplicating all of the code logic in rdmemm().

* Subroutine upb() was re-written to directly call subroutine upbb() instead of
duplicating all of the code logic in upbb().

* Subroutine POSAPN has been marked as obsolete (for future removal from
BUFRLIB).  The same functionality can now be obtained via the use of
subroutine posapx().

* Subroutine wrcmps() was modified to fix a bug involving the writing of
compressed subsets which contain delayed replication.  In certain situations,
the values of two internal variables were not being properly saved between
successive calls to the subroutine.

* Changes were made so that the BUFRLIB will automatically read and adjust
to any DX table (dictionary) messages internal to a file.  Previously, the
software would only ever process such messages at the beginning of a file,
so that all subsequent data messages in that file were required to conform
to these initial dictionary messages, and any subsequent dictionary messages
in the file were simply ignored.  Now, any subsequent dictionary messages
will cause the BUFRLIB to adjust its internal processing tables and treat all
subsequent data messages as conforming to these new dictionary messages, up
through the end of the file or until yet another set of dictionary messages
is encountered.  These changes affect all BUFRLIB subroutines which read BUFR
messages from a file, including readmg(), ireadmg(), readmm(), ireadmm(), rdmemm(),
readns() and ireadns().

* Subroutine ADDATE has been marked as obsolete (for future removal from
BUFRLIB) since it is no longer called by any BUFRLIB routines.  The same
functionality can now be obtained via the use of subroutine W3MOVDAT in the
NCEP W3 library.

* Subroutine SUBUPD has been marked as obsolete (for future removal from
BUFRLIB) since it is no longer called by any BUFRLIB routines and is almost
an exact replica of subroutine msgupd().  The same functionality can now be
obtained via the use of subroutine msgupd().

* A new logical function msgfull() was added which determines whether there is
enough room to store the current data subset within the current BUFR message
for output.  The same logic had been repeated within numerous existing
subroutines throughout BUFRLIB and has now been consolidated into this single
subroutine that can itself be called from wherever it is needed.

* A new capability was added to BUFRLIB to allow it to append a tank receipt
time to Section 1 within all future BUFR messages written to output by
subroutines writsb(), copymg() or equivalent.  The tank receipt time is a local
extension to Section 1; however, its inclusion in a message is still fully
compliant with the WMO BUFR regulations.  This new capability is activated via
an initial call to new subroutine strcpt(), specifying the time to be appended
to Section 1 within all future BUFR messages written to output.  This same
information can then be read back from an input BUFR message via a call to new
subroutine rtrcpt().

* Subroutine numtab() was re-written to directly call subroutine numtbd()
instead of duplicating all of the code logic in numtbd().

* Subroutine nemtba() was re-written to directly call subroutine nemtbax()
instead of duplicating all of the code logic in nemtbax().

* Documentation was improved within numerous subroutines throughout BUFRLIB,
including the addition of docblocks where none previously existed.

* The default BUFR master table version number was changed from "12" to "13"
within subroutines cmsgini(), dxmini() and msgini().

* A new capability was added to allow BUFRLIB print diagnostics and other
runtime messages to be redirected somewhere other than the default FORTRAN
logical unit 6 (i.e. standard output).  This is enabled within an application
program by supplying an in-line version of subroutine errwrt() to override the
new default version of this subroutine provided within the BUFRLIB.  The
default version will continue to write to standard output when included within
a compilation.

* Subroutines cmsgini(), stndrd() and msgwrt() were modified to remove a logical
error which assumed that any message whose data section (Section 4) was
compressed was also fully standard.  In reality, the use of compression only
implies that the data section is fully standard and does not necessarily imply
that the data description section (Section 3) is also fully standard.  BUFRLIB
will now address the standardization of Section 3 solely within subroutine
stndrd(), independent of whether or not the data in Section 4 are compressed.

* Functions LSTRPC and LSTRPS have been marked as obsolete (for future removal
from BUFRLIB).  The same functionality can now be obtained via the use of
function lstjpb().

* Subroutine ufbtab() was modified to fix a bug involving the unpacking of
character strings which are identical within each subset of a single
compressed BUFR message.


## May 28, 2008

* Subroutine BORT_EXIT was modified to fix a faulty ANSI-C declaration.
This had been silently ignored by the IBM CCS compiler but was a portability
issue for other compilers.

* Subroutines RDTREE and WRTREE were modified to fix a bug which, on rare
occasions, caused a segmentation fault due to overflow of internal arrays.
This bug only occurred when working with long character strings (i.e. longer
than 8 bytes) while using a non-optimized compilation of BUFRLIB.

* Subroutine WRITCA, which had been marked as obsolete within a previous
version of BUFRLIB, has now been deleted.

* A new subroutine PARSTR was added which works like existing subroutine
PARSEQ, except that it allows substrings within a string to be separated by
one or more occurrences of any given single character (and not just by one
or more blank characters).  As such, the existing subroutine PARSEQ has
been marked as obsolete (for future removal from BUFRLIB), and many other
subroutines throughout BUFRLIB have been modified to now use the new
subroutine PARSTR.

* Subroutine JSTCHR was modified to add a return argument indicating
whether the input string was empty.  This allows the subroutine to be used
in any context where existing subroutine LJUST was being used, and LJUST
has now been marked as obsolete (for future removal from BUFRLIB).

* Several new subroutines have been added to enable the capability to read
BUFR table information from external ASCII master tables instead of from
pre-defined DX dictionary files.  This is in preparation for the planned
future capability to be able to directly decode a BUFR message according to
its internal data description section.

* The value BMISS (i.e. the BUFR "missing" value), which was defined as a
local data value within many separate subroutines, has now been defined as a
global parameter within the "bufrlib.PRM" include file.  In addition, a new
function IBFMS has been added which safely tests a given value to determine
whether or not it is "missing", and several existing subroutines throughout
BUFRLIB have been modified to now use this new function.

* The determination as to whether the local host machine uses the
"big-endian" or "little-endian" byte-ordering scheme is now determined at
compile time and integrated into BUFRLIB via the use of conditional
compilation statements.  This allows BUFRLIB to run much more efficiently
since it no longer has to constantly re-check the local byte-ordering
scheme at run time.

* Subroutine DXDUMP was modified to correct a bug which caused the
truncation of output reference values longer than 8 digits.

* Several global parameters were increased in "bufrlib.PRM".
Specifically, MXCDV (the maximum number of data values per subset in a
compressed BUFR message) was increased from 2000 to 3000, and MAXMEM (the
maximum number of bytes that can be used to store BUFR messages within
internal memory) was increased from 50Mb to 75Mb within the "supersized"
BUFRLIB.


## February 6, 2007

* Several global parameters were increased in "bufrlib.PRM".  Specifically,
MAXTBA, MAXTBB and MAXTBD (the maximum numbers of internal Table A, B and D
entries, respectively) were increased from 60, 250 and 250 to 120, 500 and 500,
respectively, and MAXJL (the maximum number of internal jump/link table
entries) was increased from 16000 to 20000.

* Subroutine CKTABA was modified to allow "FRtttsss" and "FNtttsss" (where ttt
is the message type and sss is the message subtype) as valid Table A mnemonics
for foreign BUFR messages.  Previously, only "NCtttsss" had been allowed.

* Subroutines GETS1LOC and IUPBS01 were modified to provide two additional
options for unpacking values from Section 1 of a BUFR message.  Specifically,
"CENT" now unpacks the century and "YCEN" now unpacks the year of the century.

* Subroutine PKBS1 was modified to provide several additional options for
directly packing values into Section 1 of a BUFR message.  Specifically,
"YEAR", "MNTH", "DAYS", "HOUR", "CENT" and "YCEN" now pack the message year,
month, day, hour, century and year of century, respectively, and "MTYP" and
"MSBT" now pack the message type and subtype, respectively.

* Subroutine MAXOUT was modified to allow it to be called with a special flag
value of "0", indicating that output BUFR messages should be set to the maximum
allowable record length.  In addition, a sanity check was added to prevent this
record length from being set to a value greater than the maximum allowable.

* For the printing of flag table values, subroutines UFBDMP and UFDUMP were
modified to include an equivalent listing of the bits that were actually set.

* Subroutine UFBPOS was modified to remove an unnecessary (and incorrect!)
initialization statement.  This had been silently ignored by the IBM CCS
compiler but was a portability issue for other compilers.

* Subroutine UFBTAB was modified to add a required declaration for a local
character variable.  This had been silently ignored by the IBM CCS compiler
but was a portability issue for other compilers.

* Subroutine RDUSDX was modified to abort if it encounters a user-defined
BUFR message whose message type is set to 11.  This value is reserved for
internal dictionary messages.


## January 31, 2006

* Documentation was improved and/or clarified within many existing routines
throughout BUFRLIB.

* Global parameter MAXMEM (the maximum number of bytes that can be used to
store BUFR messages internally) was increased from 16Mb to 50Mb, and global
parameter MAXTBA (the maximum number of entries in the internal BUFR Table A)
was increased from 50 to 60.  In addition, all global parameters were moved
into a new global INCLUDE file "bufrlib.PRM", rather than continuing to
hardcode the same parameter values in every individual source file where
they were needed.  This will allow future changes to any of these parameter
values to be made much more easily.

* An additional CCS compilation of BUFRLIB (libbufr_s_64.a) is now being
maintained via the makefile.  This new version is identical to the existing
libbufr_4_64.a compilation, except that several parameter values within
"bufrlib.PRM" are set much larger in order to allow extremely large BUFR
messages (i.e. up to 2.5Mb) to be processed.

* The capability to compress output BUFR messages has now been directly
incorporated into subroutines WRITSB and WRITSA, whereas previously it had
been necessary to instead call the separate subroutines WRITCP and WRITCA,
respectively.  The use of compression can now be easily toggled on or off
(with "off" as the default if left unspecified) via new subroutine CMPMSG.
As such, subroutine WRITCA has now been marked as obsolete (for future removal
from BUFRLIB), since the same functionality can now be obtained by calling the
new subroutine CMPMSG, followed by a call to WRITSA.  In a similar manner,
WRITCP has now been modified to directly call subroutines CMPMSG and WRITSB,
although it is being retained as a distinct subroutine within BUFRLIB (rather
than also being marked as obsolete) out of consideration for the large number
of existing application programs which use it.

* A new function IUPBS01 was added which works like existing function
IUPBS1, except that it uses a descriptive mnemonic rather than a hardcoded
byte number in order to specify the value to be unpacked from Section 0 or
Section 1 of a BUFR message.  This allows the same function call to work on
messages encoded using either BUFR edition 3 or BUFR edition 4 (rather than
having to pass in different byte numbers depending on the edition!), and it
also allows values encoded across multiple bytes (e.g. section lengths,
4-digit years, etc.) to be easily unpacked as well.  As such, the existing
function IUPBS1 has been marked as obsolete (for future removal from BUFRLIB),
and many other subroutines throughout BUFRLIB (e.g. UPDS3, DATEBF, DUMPBF,
STNDRD, CKTABA, NMBYT, MSGWRT, RDBFDX, etc.) have been modified to now use
the new function IUPBS01.  In addition, a new function IUPVS01 was added which
calls IUPBS01 in an in-line fashion, and existing function IUPVS1 (which had
similarly called IUPBS1 in an in-line fashion) has now been marked as obsolete.

* A new subroutine PKVS01 was added which works like existing subroutine
PKVS1, except that it uses a descriptive mnemonic rather than a hardcoded
byte number in order to specify the value to be stored into Section 0 or
Section 1 of all future output BUFR messages.  This allows the same
subroutine call to work on messages encoded using either BUFR edition 3
or BUFR edition 4 (rather than having to pass in different byte numbers
depending on the edition!), and it also allows values encoded across multiple
bytes (e.g. 4-digit years, originating centers and subcenters, etc.) to be
easily overwritten as well.  As such, the existing subroutine PKVS1 has been
marked as obsolete (for future removal from BUFRLIB).  In a similar manner,
a new subroutine PKBS1 was also added to replace existing subroutine OVRBS1,
which has now itself also been marked as obsolete.

* A new subroutine CNVED4 was added which, given a BUFR message encoded using
BUFR edition 3, creates and outputs an equivalent message encoded using BUFR
edition 4.  This subroutine can be called by an application program, or it can
alternatively be activated in an in-line fashion via a call to new subroutine
PKVS01 using the descriptive mnemonic "BEN" (i.e. BUFR edition number) with a
corresponding value of "4".

* Subroutines NEMTAB, NUMTAB, TABENT and TABSUB were modified to support
the Table C operators 2-07-YYY and 2-08-YYY, which are new to BUFR with the
advent of edition 4.

* Subroutines COPYST, WRITST and STANDARD, which had been marked as obsolete
within a previous version of BUFRLIB, have now been deleted.

* The default BUFR master table version number was changed from "4" to "12"
within subroutines CMSGINI, DXMINI and MSGINI.

* A bug was corrected in subroutine STNDRD in order to ensure that byte 4 of
Section 4 is always properly zeroed out.

* A bug was corrected in subroutine PARUTG which was preventing 1-bit delayed
replication factors from being directly read via a call to subroutine UFBINT.

* A bug was corrected in subroutine WRCMPS which was causing a character
compression array to be improperly initialized.  In addition, a local parameter
was increased to allow up to 4000 subsets to be written into a single compressed
BUFR message.

* Subroutine UFBMEM was modified to not abort when there are either too many
messages read in or too many bytes read in (i.e., .gt. array limits passed in),
but rather to just process the limiting number of messages and/or bytes and
print a diagnostic.

* Subroutine CLOSMG was modified to override logic that had always written
out messages 1 and 2 even when they contained zero subsets (it assumed these
contained the dump center and processing time in Section 1).  Now, if the unit
number argument is passed in as a negative number the first time this routine
is called by an application program, ALL empty messages are skipped (i.e.,
assumes that messages 1 and 2 do not contain dump center and processing time).
This remains set for all subsequent calls to CLOSMG for a particular file,
regardless of the sign of the unit number (CLOSMG is called by other BUFRLIB
routines which always pass in a positive unit number).

* A new function IGETDATE was added which unpacks and returns the Section 1
date-time from an input BUFR message, in format of either YYYYMMDDHH or YYMMDDHH
depending on the value requested via the most recent call to subroutine DATELEN.
The same logic had been repeated within numerous existing subroutines throughout
BUFRLIB and has now been consolidated into this single subroutine that can
itself be called from wherever it is needed.

* A new subroutine GETLENS was added which unpacks and returns the individual
section lengths from an input BUFR message.  The same logic had been repeated
within numerous existing subroutines throughout BUFRLIB and has now been
consolidated into this single subroutine that can itself be called from
wherever it is needed.

* A new subroutine RDMSGW was added which reads the next padded BUFR message
from a given BUFR file.  The same logic had been repeated within numerous
existing subroutines throughout BUFRLIB and has now been consolidated into
this single subroutine that can itself be called from wherever it is needed.

* A new function PKFTBV was added which computes and returns the value
equivalent to the setting of a specified bit within a flag table of a
specified width.  In addition, a new subroutine UPFTBV was also added which
functions as the logical inverse, i.e. given a mnemonic and corresponding flag
table value, it computes and returns the equivalent bit settings.

* A new subroutine UFBPOS, which allows a user to directly point at and read
a specified subset from within a specified message in an input BUFR file, was
added to BUFRLIB.  Previously, this logic existed as an in-line subroutine
within a separate application program.

* A new subroutine GETABDB, which returns internal BUFR table information
in a pre-defined ASCII format, was added to BUFRLIB.  Previously, this logic
existed as an in-line subroutine within a separate application program.

* Subroutine READMG was modified to be able to handle BUFR messages which are
not padded out to an 8-byte boundary and for which it had therefore previously
been necessary to instead call the separate subroutine READIBM.  Logic was also
added to allow the option of having READMG behave like the separate subroutine
READFT, so that it will not abort when a read error is encountered but rather
will treat it the same as an end-of-file condition.  This option is activated
by passing in the negative of the usual logical unit number.  In summary, READMG
can now itself properly read from any FORTRAN-blocked file of BUFR messages, and
therefore the existing subroutines READIBM, IREADIBM, READFT and IREADFT have
now all been marked as obsolete (for future removal from BUFRLIB).

* A set of generic C-language functions for reading/writing BUFR messages
from/to generic BUFR files (which may or may not contain FORTRAN-blocking and/or
message padding) was added to BUFRLIB.  These functions (CCBFL, COBFL, CRBMG,
CWBMG and RBYTES) are primarily intended for use by separate application
programs (such as cwordsh), but are themselves being directly incorporated into
BUFRLIB in order to prevent such application programs from having to directly
link to certain COMMON blocks and parameter sizes internal to BUFRLIB.

* Function MOVA2I is marked as obsolete (for future removal from BUFRLIB). It
is present in the W3 Libraries (in C language) and is no longer called by any
BUFR Archive Library routines. A warning message is now printed instructing
users to migrate to MOVA2I in the W3 Libraries.

* Subroutine UFBTAB was modified to work for compressed BUFR messages.  An
option to return only the subset count (when the input unit number is less than
zero) was also added.

* Subroutine COPYSB was modified to now write out a compressed subset/message
if the input subset/message is compressed (before this subroutine could only
write out an uncompressed subset/message regardless of the compression status
of the input subset/message).


## December 21, 2004

* New subroutines ISTDESC, RESTD, WRDESC, CADN30, STDMSG and STNDRD have been
added to provide the capability to expand Section 3 of output BUFR messages
until they are completely "standard" according to the WMO FM-94 regulations.
The logic is activated via an initial call to STDMSG.

* Subroutine XMSGINI has been removed.  It had been included in a previous
BUFRLIB version as an indirect way of "standardizing" compressed messages, but
the same logic is now fully integrated into CMSGINI and is activated via a
separate initial call to new subroutine STDMSG (see 1).

* Subroutine STANDARD has been marked as obsolete (for future removal from
BUFRLIB) in favor of a new subroutine STNDRD which more completely
"standardizes" Section 3.  The old subroutine (i.e. STANDARD) would always just
break down the top-level Table A descriptor by one level, so that, unless this
"one level deep" expansion happened to consist of all standard descriptors, the
resulting BUFR message was still non-standard.  Contrarily, the new logic will
recursively break down successive sequence descriptors for as long as needed
until all appearing in Section 3 are themselves standard or else, at a
minimum, preceded with the 206YYY "bypass" operator (note: this recursive logic
is written using C for portability reasons, since not all FORTRAN 77 compilers
support recursion!).  In addition, STNDRD has other advantages over STANDARD
as well; namely, it contains safety checks which prevent overflow of the
message array that is passed to it, and it also is more directly integrated
into BUFRLIB and can be automatically activated in-line via a separate initial
call to new subroutine STDMSG (see 1).

* Subroutine WRITSA was modified to fix a bug which, in certain situations,
prevented one or more BUFR messages from being returned to the calling program
within the memory arrays.  In addition, a new subroutine WRITCA was added which
functions exactly like WRITSA except that it works on compressed messages.

* Subroutines WRCMPS and RDCMPS were modified to fix a bug in the compression
algorithm which occurred when all subsets in a single message contained
identical character strings.  Separate corrections were also made to each of
these subroutines to fix a few unrelated minor bugs.

* Subroutine UFDUMP was modified to add a fuzziness test for the "missing"
value and to add an interactive, scrolling print capability similar to UFBDMP.

* Subroutine UFBDMP was modified to automatically use READLC when reading
"long" character strings, similar to an existing capability within UFDUMP.

* Documentation was improved and/or clarified in many existing subroutines
throughout BUFRLIB.

* Subroutines COMPRES and READ2C have been removed.  The same functionality
can be obtained by using subroutine WRITCP.

* Subroutines IREADERS, READERS and READTJ have been removed, as they were
nothing more than wrappers for READMG and had been marked as obsolete within
a previous BUFRLIB version.

* Subroutines READERM, IREADERM and IRDERM have been removed.  They had
been superseded functionally by (the more-portable!) subroutine READIBM and
had been marked as obsolete within a previous BUFRLIB version.

* Parameter MXMSGL (the maximum number of bytes in a BUFR message) was
increased from 20K TO 50K bytes in the following subroutines: BFRINI, CKTABA,
CLOSMG, CMSGINI, COPYBF, COPYMG, COPYSB, CPYMEM, CPYUPD, DXMINI, MAXOUT,
MESGBC, MESGBF, MINIMG, MSGINI, MSGUPD, MSGWRT, NMBYT, POSAPN, POSAPX,
RCSTPL, RDBFDX, RDCMPS, RDMEMM, RDMEMS, RDMGSB, RDTREE, READERME, READFT,
READIBM, READLC, READMG, READMM, READSB, REWNBF, SUBUPD, UFBGET, UFBINX,
UFBMEM, UFBTAB, UFBTAM, WRCMPS, WRITDX, WRITLC, WRITSA and WRTREE.  (Note:
this is not included in the Docblock history in these routines.)

* Subroutines READERME, READIBM, DATEBF and DUMPBF were modified to make
the test for the string 'BUFR' portable to EBCDIC machines.

* Subroutine WRTREE was modified to use double-precision arithmetic within
an internal statement function, in order to correct for a truncation problem
that could occur in the case of very large computed values.

* Subroutine COPYST has been marked as obsolete (for future removal from
BUFRLIB).  The same functionality can be obtained by calling new subroutine
STDMSG, followed by a call to COPYMG.

* Subroutine WRITST has been marked as obsolete (for future removal from
BUFRLIB).  The same functionality can be obtained by calling new subroutine
STDMSG, followed by a call to CLOSMG.

* A new option IO="NODX" has been added to subroutine OPENBF.  In this
case, the subroutine behaves exactly as if it had been called with IO="OUT",
except that DX dictionary messages are not written out to logical unit LUNIT.

* Subroutine WRDLEN was modified to keep track of whether it has already
been called by one of the other BUFRLIB subroutines and, if so, to then
immediately return (without proceeding any further) every time it is
subsequently called.

* Subroutines OPENBF, UFBINT, UFBOVR, UFBREP, UFBSEQ, UFBSTP and WRDLEN
were all modified to fix similar portability bugs whereby the values of some
internal variable(s) which keep track of whether the subroutine has already
been called were not being explicitly preserved with a SAVE statement.

* New subroutine PKVS1 was added which calls OVRBS1 in an in-line fashion
and therefore allows easier overwriting of default values in Section 1 of
output BUFR messages.  The new methodology can also overwrite the value of
byte 8 in Section 0 (i.e. BUFR edition number) if desired.

* New function IUPVS1 was added which calls IUPBS1 in an in-line fashion
and therefore allows easy unpacking of Section 1 values from BUFR messages
that have already been read into the internal memory arrays by subroutine
READMG or equivalent.  The new methodology can also unpack the value of
byte 8 in Section 0 (i.e. BUFR edition number) if desired.

* Subroutine ADDATE was modified to fix a bug in calculating the number of
days in February for years which are multiples of 100 but not of 400.

* Subroutine MESGBC was modified to allow the option of operating on a
BUFR message that has already been read into the internal memory arrays by
subroutine READMG or equivalent.

* New subroutine DXDUMP was added which outputs an ASCII-formatted copy of
the information embedded within the DX dictionary messages of a BUFR file.
It is especially useful for learning the contents of archived BUFR files,
and the output is in a format suitable for subsequent input to OPENBF as a
user-defined dictionary tables file.

* Subroutines DATELEN, DATEBF and DUMPBF were all modified to call
subroutine WRDLEN to initialize local machine information (in case it has
not already been called).  These routines do not require this information
but they may now or someday call other routines that do require it.


## November 4, 2003

This is the first "unified" BUFR Archive Library including components from the
regular NCEP production machine version (whose implementation history is
documented to this point), the decoder version (previously on a workstation but
now on the IBM Frost and Snow machines), and a checkout NCEP/EMC grid-to-obs
verification version.  This version is portable to all platforms (as necessary
for WRF), contains docblocks for each routine with a complete program history
log, and outputs more complete diagnostic information when routines terminate
abnormally, unusual things happen or for informational purposes.

The following libraries are now generated on the NCEP IBM Frost and Snow
machines:

* libbufr_4_64.a -- 4-byte reals, 4-byte integers, 64-bit executable compilation
* libbufr_8_64.a -- 8-byte reals, 8-byte integers, 64-bit executable compilation
* libbufr_d_64.a -- 8-byte reals, 4-byte integers, 64-bit executable compilation
* libbufr_4_32.a -- 4-byte reals, 4-byte integers, 32-bit executable compilation

The first three are compiled exactly the same as the three libraries noted in
the previous 05-27-2003 implementation (they are just renamed).  The fourth
library is compiled identically to the previous decoder-specific version on
Frost and Snow (libdecod_bufr_32.a).  It is compiled with optimization level 3
(-O3) for both Fortran and c routines and will be linked into the production
decoder programs in place of libdecod_bufr_32.a.

The following routines have been added to the BUFR archive library:

* Subroutine BORT2 which prints (to STDOUT) two given error strings and
then calls BORT_EXIT (see 7 below) to abort the application program calling
the BUFR Archive Library software.  It is similar to existing subroutine BORT,
except BORT prints only one error string.

* Function IUPBS1 which, given a BUFR message contained within array MBAY,
unpacks and returns the binary integer contained within byte NBYT of
Section 1 of the BUFR message.  This was present in the original decoder-
specific version of the library.

* Subroutine OVRBS1 which, given a BUFR message contained within array MBAY,
packs and stores the value of a binary integer into byte NBYT of Section 1
of the BUFR message, overwriting the value previously stored there.

* Subroutine UPDS3 which, given a BUFR message contained within array MBAY,
unpacks and returns the descriptors contained within Section 3 of the BUFR
message.  This was present in the original decoder-specific version of the
library.

* New library function MOVA2I replaces the W3LIB c-version
previously called by DATEBF and DUMPBF.  It is now called by new subroutines
MESGBC (see 6) and REWNBF (see 11) as well.  This change removes any
dependency upon the W3LIB, since no other BUFR Archive Library routines call
any W3LIB routines.  It was converted to Fortran 77 because the c-version does
not work properly when compiled with 32-bit executable compilation and linked
into a Fortran source copiled with 8-byte real and integer word length.

* Subroutine MESGBC reads past any BUFR table (dictionary) or dummy
(center or dump time in dump files) messages in a BUFR file (if there are
any) and returns the message type for the first report data message found.
It also determines whether or not this first report data message is
compressed BUFR.  This subroutine is identical to MESGBF except MESGBF
only reads past dictionary messages and MESGBF does not return any
information about compression.

* C subroutine BORT_EXIT terminates the application program calling the
BUFR software and returns an implementation-defined non-zero status code to
the executing shell script.  (See 1 and 26.)

* Subroutine RDMGSB opens a BUFR file in logical unit LUNIT for input
operations, then reads a particular subset into internal subset arrays from a
particular BUFR message in a message buffer.  This is based on the subset
number in the message and the message number in the BUFR file.  This was
present in the original verification-specific version of the library.

* Subroutine SUBUPD packs up the current subset within memory and then tries
to add it to the BUFR  message that is currently open within memory for
LUNIT.  If the subset will not fit into the currently open message, then that
message is flushed to LUNIT and a new one is created in order to hold the
current subset.  If the subset is larger than an empty message, the subset is
discarded and a diagnostic is printed.  This subroutine is identical to
existing BUFR Archive Library subroutine MSGUPD except SUBUPD does NOT pad the
packed subset.  This was present in the original verification-specific version
of the library.

* Subroutine UFBINX either opens a BUFR file for input operations (if it is
not already opened as such), or saves its position and rewinds it to the first
data message (if BUFR file already opened), then (via a call to BUFR Archive
Library subroutine UFBINT) reads specified values from internal subset arrays
associated with a particular subset from a particular BUFR message in a message
buffer.  The particular subset and BUFR message are based based on the subset
number in the message and the message number in the BUFR file.  Finally, this
subroutine either closes the BUFR file (if is was opened here) or restores it
to its previous read/write status and position (if is was not opened here).
This was present in the original verification-specific version of the library.

* Subroutine REWNBF which will either: 1) store the current parameters
associated with a BUFR file (read/write pointers, etc.), set the file status
to read, then rewind the BUFR file and position it such that the next BUFR
message read will be the first message in the file containing actual subsets
with data; or 2) restore the BUFR file to the parameters it had prior to 1)
using the information saved in 1).  This allows information to be extracted
from a particular subset in a BUFR file which is in the midst of being read
from or written to by an application program.  This was present in the original
verification-specific version of the library.

* Subroutine UFBIN3 reads specified values from the current BUFR data subset
where the data values correspond to mnemonics which are part of a multiple-
replication "level" sequence within another multiple-replication "event stack"
sequence.  This subroutine is designed to read event information from
"PREPFITS" type BUFR files (currently the only application which reads
PREPFITS files is the verification program GRIDTOBS, where UFBIN3 was
previously an in-line subroutine).  The existing analogous subroutine UFBEVN
should be used to read information from "PREPBUFR" type BUFR files.  This was
present in the original verification-specific version of the library.

* Function NEVN accumulates all data events for a particular data value and
level and returns them to the calling program.  The value of the function
itself is the total number of events found.  {This function should only be
called by UFBIN3 (see 12), which, itself, is called only by verification
application program GRIDTOBS, where it was previously an in-line subroutine.
In general, NEVN does not work properly in other application programs at this
time.}  This was present in the original verification-specific version of the
library.

* Subroutine READLC returns a character data element associated with a
particular subset mnemonic from the internal message buffer.  It is designed
to be used to return character elements greater than the usual length of 8
bytes.  It currently will not work for compressed BUFR mesaages.

* Subroutine WRITLC packs a character data element associated with a
particular subset mnemonic from the internal message buffer.  It is designed
to be used to store character elements greater than the usual length of 8
bytes.

* Subroutine WRITST generates a standardized version of the current BUFR
message in internal memory and writes it to the output file (not sure if
it works properly).

* Subroutine COPYST generates a standardized version of the current BUFR
message read using READMG and writes it intact as a record to the output
file.

* Subroutine COMPRES compresses subsets in BUFR messages previously read
using BUFR Archive Library subroutine READMG or equivalent.  It then
generates a new bufr message consisting of the compressed subsets.  Note
that subsets in the output compressed message may have been read from
different (adjacent) input messages.  Currently the only application program
which calls this subroutine is BUFR_COMPRESS, where COMPRES was previously
an in-line subroutine).

The following routines in the BUFR archive library have been modified:

* Subroutine READ2C reads a subset into compression maxtrix arrays in
preparation for generating compressed BUFR messages.  This had been an in-
line subroutine in the application program BUFR_COMPRESS and is currently
called only by BUFR Archive Library subroutine COMPRES (see 18).

* Subroutine UPBB modified to make certain zero is returned for zero bits
input and to make logic consistent with logic in UPB.  (See also 30 for UPBB.)

* Subroutine UFBTAB modified to not abort when there are too many subsets
coming in (i.e., .gt. array limit passed in), but rather to just process the
limiting number of reports and print a diagnostic.  It is also modified to
call subroutine REWNBF when the BUFR file is already opened (this is taken
from the verification version of UFBTAB and allows specific subset information
to be read from a file in the midst of being read from or written to), before
OPENBF was always called and this would have led to an abort of the
application program.  (See also 29 for UFBTAB.)

* Subroutine CKTABA modified to not abort when the Section 1 message subtype
does not agree with the Section 1 message subtype in the dictionary IF the
message type mnemonic is not of the form "NCtttsss", where ttt is the BUFR type
and sss is the BUFR subtype. This allows program PREPOBS_PREPDATA to specify
different message subtypes for the same message type.  (See also 27 and 43.)

* Subroutine OPENBF modified to accept 'NUL' as the second (I/O) argument.
IO='NUL' prevents the BUFR Archive Library software from actually trying to
access or write to the BUFR file (designed only for use with library
subroutine WRITSA).  This was present in the original decoder-specific version
of the library.

* Subroutine CLOSBF modified to not close the BUFR file if it was opened as
'NUL' by OPENBF (see 23).  This was present in the original decoder-specific
version of the library.

* Subroutine MSGWRT modified to not write to the BUFR file if it was opened
as 'NUL' by OPENBF (see 23).  This was present in the original decoder-
specific version of the library.

* Subroutine BORT modified to call new BUFR Archive Library subroutine
BORT_EXIT (see 7 above) rather than c function EXIT with argument 49 {"CALL
EXIT(49)"}.  Since EXIT is an intrinsic c function, it expects arguments to be
passed by value rather than by reference as in done in Fortran.  This has
caused an unpredictable status code to be passed back to the executing shell
script, in some cases even ZERO!!  This change will ensure an non-zero status
is always returned.

* Suboutines CKTABA, DATEBF, DUMPBF and function I4DY modified such that
date calculations no longer use floating point arithmetic.  This can lead to
round off error and an improper resulting date on some machines (e.g., NCEP
IBM Frost/Snow).  This change increases portability of the BUFR Archive
Library.  (See also 22 and 43 for CKTABA.)

* Parameter MAXMSG (the maximum number of BUFR messages which can be stored
internally) increased from 50000 to 200000 in the following subroutines:
CPYMEM, RDMEMM, RDMEMS, READMM, UFBMEM, UFBMMS, UFBMNS, UFBRMS and UFBTAM.
This may be necessary in the future for BUFR files with many, many messages.

* Parameter MAXJL (the maximum number of Jump/Link table entries) increased
from 15000 to 16000 in the following routines: BFRINI, CONWIN, COPYMG, CPYMEM,
DRFINI, DRSTPL, GETWIN, INCTAB, INVCON, INVMRG, INVTAG, INVWIN, LSTJPB,
LSTRPC, LSTRPS, MAKESTAB, MSGINI, NEWWIN, NVNWIN, NWORDS, NXTWIN, PARUTG,
RCSTPL, RDCMPS, RDTREE, READNS, TABENT, TABSUB, TRYBUMP, UFBCPY, UFBCUP,
UFBDMP, UFBEVN, UFBGET, UFBINT, UFBOVR, UFBREP, UFBRP, UFBRW, UFBSEQ, UFBSP,
UFBSTP, UFBTAB, UFBTAM, UFDUMP, USRTPL, WRCMPS and WRTREE.  This was present
in the original verification-specific version of the library.

* The following routines are modified to make the BUFR Archive Library
big-endian/little-endian independent: IPKM, IUPM, PKB, PKC, UPB and UPBB.  This
was present in the original decoder-specific version of the library and
increases the portability of the BUFR Archive Library.

* Subroutine BFRINI modified to initialize variable JSR as ZERO in new
COMMON block /BUFRSR/.  This was present in the original verification-specific
version of the library.  (See also 29 for BFRINI.)

* Subroutine RCSTPL modified to increase the maximum number of levels of
recursion (parameter MAXRCR) from 50 to 100.  This was present in the original
verification-specific version of the library.  (See also 29 and 43 for RCSTPL.)

* Subroutine WRCMPS modified to save logical variables WRIT1 and FLUSH in
global memory.  This fixed a bug in this subroutine which could lead to
messages being written out before being full.  (See also 29 for WRCMPS.)

* Subroutine RDTREE modified to fix a bug which could only occur when the
last element in a subset is a character.  (See also 29 for RDTREE.)

* Subroutine UFDUMP modified to handle print of character values greater
than 8 bytes.  (See also 29 for UFDUMP.)

* Subroutine UFBEVN modified to save the maximum number of events found for
all data values specified amongst all levels returned as variable MAXEVN in
new COMMON block /UFBN3C/ and to add call to BORT if BUFR file is open for
output.  (See also 29 for UFBEVN.)

* Subroutine NEMOCK modified to expand non-zero return into -1 for length
not 1-8 characters and -2 for invalid characters (return only -1 before for
all problematic cases)

* Subroutine NUMBCK modified to expand non-zero return into -1 for invalid
character in position 1, -2 for invalid characters in positions 2 through 6,
-3 for invalid characters in positions 2 and 3 due to being out of range, and
-4 for invalid characters in positions 4 through 6 due to being out of range
(return only -1 before for all probelmatic cases)

* Subroutine WTSTAT modified to correct a "typo" in test for valid value for
"IM".

* Subroutines ELEMDX, PARSEQ, PARUSR, PARUTG, PKC, RDUSDX, SEQSDX, STRING,
UFBINT, UFBOVR, UFBREP, UFBSTP and VALX modified to call new BUFR Archive
Library subroutine BORT2 (see 1).

* Subroutine MAKESTAB modified to allow for the possibility that a connected
file may not contain any dictionary table info (e.g., an empty file).
Subsequent connected files which are not empty will no longer get tripped up
by this.  (This change avoids the need for an application program to
disconnect any empty files via a call to CLOSBF.)  (See also 29 for MAKESTAB.)

* Subroutine READTJ modified to simply call BUFR Archive Library subroutine
READMG rather than being a clone of it.  At one time it performed different
functions than READMG, but that has not been the case since the 2000-09-19
BUFR Archive Library implementation.

* Subroutines CKTABA, CMSGINI, NUMTAB, PARUSR, PARUTG, RCSTPL, USRTPL,
WRDLEN, WRTREE and XMSGINI modified to correct some minor bugs (uninitialized
variables, etc.) (see subroutine DOCBLOCKS for more information).  (See also
29 for PARUTG, RCSTPL, USRTPL, WRTREE and 32 for RCSTPL.)

* Subroutine UFBDMP modified to add "fuzziness" about 10E10 in test for a
missing value (rather than true equality as before) because some missing values
(e.g., character strings < 8 characters) were not getting stamped out as
"MISSING".  Also added option to print values using format edit descriptor
"F15.6" if input argument LUNIN is < zero.  If LUNIN is > zero edit descriptor
expanded from "G10.3" to "G15.6".  (See also 29 for UFBDMP.)


## May 19, 2003

The following changes have been made in the BUFR Archive Library:

* Subroutine CLOSMG - to correct a problem introduced in the previous
(May 2002) implementation which prevented the dump center time and initiation
time messages from being written out (affected program BUFR_DUMPMD, if it were
recompiled, in the data dumping process)

* Subroutine UFBREP - to work properly for descriptors tied to a pivot
descriptor in delayed replicated sequences (involved disabling the parsing
switch which controlled checking for presence in the same replication group -
UFBREP does not need this check, and it interferes with what UFBREP can do
otherwise)

* Subroutine UFBSEQ - to fix cases where delayed replication is at end of
subset, or when a requested sequence is missing; also corrected the logic
array of exit conditions for the subroutine, previously, in some cases, proper
exits were missed, generating bogus error messages, because of several
miscellaneous bugs which are now removed

* Subroutine UPB - to make certain zero is returned for zero bits input

* The following subroutines are modified to replace calls to Fortran
Insrinsic Function ICHAR with calls to NCEP W3LIB c-function MOVA2I: DATEBF
and DUMPBF.  This change increases portability of the BUFR Archive Library
because MOVA2I copies a bit string from a Character*1 variable to an integer
variable.  It is intended to replace the Fortran Intrinsic Function ICHAR,
which only supports 0 <= ICHAR(A) <= 127 on the IBM SP.  If "A" is greater
than 127 in the collating sequence, ICHAR(A) does not return the expected bit
value.  This function can be used for all values of ICHAR(A) between 0 and 255.
This change increases portability of the BUFR Archive Library and is, in
fact, necessary on the NCEP IBM Frost and Snow machines.

The BUFR Archive Library on Frost and Snow is compiled using optimization
level 4 (-O4) for Fortran routines and optimization level 3 (-O3) for c
routines.  The previous BUFR Archive Library on the IBM-SP's had used -O3 for
the default filenames and -O4 for a second set of filenames with the string
"_O4" appended to the end.

The following libraries are generated on the NCEP IBM Frost and Snow machines:

* libbufr_4.a -- 4-byte reals, 4-byte integers, 64-bit executable compilation
* libbufr_8.a -- 8-byte reals, 8-byte integers, 64-bit executable compilation
* libbufr_d.a -- 8-byte reals, 4-byte integers, 64-bit executable compilation


## May 14, 2002

A number of routines in the BUFR Archive Library have been modified.  These
changes include:

* Entries IREADMM, IREADNS, IREADSB, IREADERS, IREADIBM, IREADFT and
ICOPYSB changed to functions. Entries MRGINV, MINIMG, DATELEN, NENUBD,
NENUAA, JSTNUM, JSTCHR and READERS changed to subroutines (note that READERS
now simply calls READSB since it was an entry point at the top of READSB and
was thus already an alias to it). Converted all entry points to subroutines or
functions in order to increase portability to other platforms (e.g., the NESDIS
CEMSCS machine).

* Entries DXMINA, DXMINB, DXMIND and SUBUPD removed because they are
obsolete.

* Added XMSGINI for capacity to expand section three. XMSGINI has the
capacity to write a fully expanded section three descriptor set into BUFR
messages.  Created specifically for NESDIS so they can send files out without
local sequence descriptors. This "capacity" is not fully functional, it is
currently activated by changing WRCMPS.to call it rather than CMSGINI, which
writes sections 0,1,2,3  for compressed messages in the usual way. XMSGINI
is included because it is useful for particular situations as is (aka
NESDIS), and at some point could be integrated as a more direct form of
STANDARDizing messages for export or whatever.

* Included in-line compression function (subr. CMSGINI, WRITCP, WRCMPS
added).

* Improved RDCMPS and UFBSEQ for generality. Previously RDCMPS and UFBSEQ
would not recognise compressed delayed replication as a legitimate data
structure.

* Removed old CRAY compiler directives in: COPYSB, CPYUPD, DRSTPL, GETWIN,
INVMRG, MVB, NEWWIN, NXTWIN, RCSTPL, READSB, UFBDMP, UFBGET, UFBINT, UFBOVR,
UFBRW, UFBTAB, UFBTAM and USRTPL.

* Added new subroutine UFDUMP which is like UFBDMP, but prints subset element
contents in more detail, omitting the pointers, counters, and other more
esoteric information describing the internal subset structures. Each
subroutine, UFBDMP and UFDUMP, is useful for different diagnostic purposes,
but in general UFDUMP is more useful for just looking at the data elements.

* Corrected error in READSB relating to certain foreign filetypes.

* Added new subroutine DRFINI which initializes delayed replication factors,
and allocates the space in the full word buffer for their contents
explicitly. This is done implictly by UFBINT in a more limited way. DRFINI
enables, for instance, the subsequent use of UFBSEQ to write data directly
into delayed replicated sequences.

* Added new subroutine MAXOUT which allows users to control the record
length of output BUFR messages created.

* Added new subroutine NUMTBD which is used by XMSGINI, in expanding the
section 3 descriptor list.

* Added new subroutine CAPIT which capitalizes a string of characters. This
enables the use of mixed case in the unit section of the ASCII BUFR tables.
An example; a program which generates an ASCII BUFR table from the "Master
Table B", might end up copying some units fields in mixed or lower case. If
the units are 'Code table' or 'Flag table' or certain other unit
designations, the table will be parsed incorrectly, and the data read or
written incorrectly as a result. This makes sure all unit designations are
seen by the parser in upper case to avoid these types of problems.

* Removed subroutine JSTIFY because it was a dummy subroutine with two
entry points for left justifying two different types of character strings.
Part of conversion of entry points to separate subroutines or functions. See
number 1 above.

* Removed subroutine NENUCK because it was a dummy subroutine with two
entry points for checking the BUFR mnemonic table.  Part of conversion of
entry points to separate subroutines or functions. See number 1 above.


## August 15, 2001

* Parameter MAXMEM (the maximum number of bytes required to store all messages
internally) was increased from 8 MBYTES TO 16 MBYTES in the following
subroutines: CPYMEM, RDMEMM, RDMEMS, READMM, UFBMEM, UFBMMS, UFBMNS, UFBRMS and
UFBTAM.

* Subroutine UFBTAM modified to not abort when there are too many subsets coming
in (i.e., .gt. array limit passed in), but rather to just process the limiting
number of reports and print a diagnostic.


## September 19, 2000

A number of routines in the BUFR Archive Library have been modified.  These
changes include:

* Consolidated logic that had been replicated in message decoding subroutines
READMG, READFT, READTJ, READERM, READERME, RDMEMM and READIBM into a single
new subroutine CKTABA (called by these subroutines).  On top of this CKTABA
now recognizes a variety of Section 3 formats, including compression
indicators and "standard" BUFR.  Thus, compressed and standard BUFR messages
can now be read in via these message decoding subroutines.

* The subset decoding subroutine READSB now calls a new subroutine RDCMPS
which allows it to read subsets from compressed BUFR messages.

* Subroutine RDTRER has been removed.  It had been called by READERS to decode
ERS scatterometer data from compressed BUFR messages.  The change in READSB
(see 2) allows READERS to be changed from a subroutine to an entry point at the
top of READSB since it is now essentially an alias to READSB.

* Subroutine UNCMPS has been removed.  It had been called by READERM, READERME
and READIBM to uncompress BUFR messages in foreign (i.e., standard ) BUFR files
(e.g., ERS scatterometer data).  This is a result of change 1 above.

* Added capability to encode and decode data using the operator descriptors
(BUFR Table C) for changing width and changing scale.  Subroutines modified
were: NEMTAB, NEMTBD, NUMTAB and TABSUB

* Enlarged arrays to allow processing messages up to 20000 bytes.  Routines
modified were: BFRINI, CLOSMG, COPYBF, COPYMG, COPYSB, CPYMEM, CPYUPD, IRDERM,
MESGBF, MINIMG, MSGINI, MSGUPD, MSGWRT, POSAPN, POSAPX, RCSTPL, RDBFDX, RDMEMM,
RDMEMS, RDTREE, READERM, READERME, READFT, READIBM, READMG, READMM, READSB,
READTJ, UFBGET, UFBMEM, UFBTAB, UFBTAM, WRITDX, WRITSA and WRTREE.

* Added subroutine UFBSEQ, like UFBINT except processes specific sequences
instead of specific elements.

* Added function NMBYT, returns the number of bytes in a message opened for
input.

The BUFR Archive Library is now compiled using both optimization level 3 (-O3)
and optimization level 4 (-O4).  The previous BUFR Archive Library had used
only -O4.  The -O3 compilation here generates the same archive library names as
before.  Thus, any code that is recompiled from an unchanged makefile will now
link in the appropriate -O3 library, rather than the -O4 library as before.
The new -O4 libraries all have the string "_O4" appended to the end of the
filename.

Any program that must link to the -O4 BUFR Archive Library when compiled will
have to modify its makefile.


## July 13, 1999

*  A number of routines in the BUFR Archive Library have been modified to
increase the number of BUFR files which can be opened at one time from 10 to 32.
This is necessary in order to process multiple BUFR files under the MPI.
The following routines were modified: BFRINI, CHEKSTAB, CLOSMG, CONWIN, COPYMG,
COPYSB, CPBFDX, CPYMEM, CPYUPD, DXINIT, ELEMDX, GETWIN, IFBGET, INVCON, INVMRG,
INVTAG, INVWIN, LSTJPD, LSTRPC, LSTRPS, MAKESTAB, MSGINI, MSGUPD, NEMTAB,
NEMTBA, NEMTBD, NENUCK, NEWWIN, NMSUB, NUMTAB, NVNWIN, NWORDS, NXTWIN, OPENBF,
OPENMB, OPENMG, PARUTG, PKTDD, RCSTPL, RDBFDX, RDMEMM, RDMEMS, RDTREE, RDTRER,
RDUSDX, READERM, READERME, READERS, READFT, READMG, READNS, READSB, READTJ,
STATUS, STRING, TRYBUMP, UFBCNT, UFBCPY, UFBCUP, UFBDMP, UFBEVN, UFBGET,
UFBINT, UFBOVR, UFBREP, UFBRP, UFBRW, UFBTAB, UFBTAM, UNCMPS, UPTDD, USRTPL,
WRITDX, WRTREE, WTSTAT.

* Subroutines READFT, READMG, and READTJ have been modified with semantic
adjustments to ameliorate compiler complaints from LINUX boxes.

* Added the new subroutine READIBM in order to process "foreign" (non-NCEP)
BUFR files which may not be padded.  Unlike the subroutine READERM, which
performs a similar fuction, READIBM works properly on all platforms and should
replace calls to READERM in application programs. (READERM does not work
properly on the NCEP IBM-SP machine.)

* Added the new subroutine NEMTBAX.  It is like subroutine NEMTBA except if
the requested mnemonic is not found, it returns rather than calls BORT.  This
is necessary to support the logic in the new BUFR Archive Library subroutine
READIBM (see 3).

* Added the new subroutine READMM.  It is like subroutine RDMEMM except it
advances the value of the message (record) number by one prior to returning
to the calling program.  This adds another option for application programs
which read BUFR files in random access mode (e.g., PREPOBS_OIQCBUFR).

* Function IREADMG has been modified to contain two new function entries
called IREADMM and IREADIBM.  The IREADIBM function calls the new library
subroutine READIBM (see 3) and the IREADMM function calls the new library
subroutine READMM (see 5).

* RDTRER, READERM, READERME and UNCMPS have been modified to expand the
maximum number of possible descriptors in a subset from 1000 to 3000.

* The maximum number of bytes required to store all messages internally was
increased from 4 Mbytes to 8 Mbytes in the following subroutines: RDMEMM,
UFBMEM, UFBMMS, UFBMNS and UFBRMS.

* The function formerly called VAL$ has been renamed to VALX to remove the
possibility of the "$" symbol causing problems on other platforms.  In turn
subroutine NEMTBB has been modified to call function VALX rather than VAL$.

* New subroutines UFBSTP and UFBSP added (UFBSP is called by UFBSTP).


## December 14, 1998

* Subroutine MSGUPD was updated to bybass the processing of reports that are
longer than the length of a BUFR message.  Prior to this change, the BUFR
Archive Library would issue an abort in the event of this rare, but possible
occurrence which occurred at 12Z on 4 December in the RGL suite.

* In addition, function I4DY was modified to use 20 as the 2-digit year for
windowing to a 4-digit year (00-20 ==> add 2000; 21-99 ==> add 1900).  This
windowing technique was inadvertently changed to 10 in the previous
implementation of the BUFR Archive Library.


## November 24, 1998

* Function I4DY and subroutine MSGWRT were changed as a result of final Y2K
testing of the decoder/ingest system.

* I4DY was changed to conform to the NCEP 2-digit year time-window of 1921-2020.

* MSGWRT was changed to zero out the padding bytes written at the end of
Section 4.


## October 27, 1998

* The BUFR Archive Library is being modified to correct problems caused by
in-lining code with fpp directives.  The following subroutines are being
changed: DATEBF, MVB, RCSTPL, RDMEMS, RDTREE, RDTRER, UFBGET, UFBRW, UFBTAB,
UFBTAM and UPBB.


## August 31, 1998

* BUFR Archive Library subroutine DATEBF, which returns the center date-time for
a BUFR data dump file, is being modified to correct an error which lead to the
year being returned in the second argument as 2-digit year when a 4-digit year
was requested via a prior call to subroutine DATELEN.  The center date
returned in the sixth argument, in the form YYYYMMDDHH, was correct in the
previous version of this subroutine.


## July 8, 1998

The new version of the BUFR Archive Library is Y2K compliant, with additional
changes to support expanded machine independence of the code, and to refine,
correct, or improve some of the routines within.  Although nearly every one of
library routines has some change made (mainly because of the introduction of a
more general error exit subroutine), the changes largely fall into the first
two categories. Three new routines were also added to the BUFR Archive Library
for micellaneous puposes.

<u>Y2K Compliance</u>

Y2K compliance in the BUFR Archive Library is downwardly compatible. That is,
the new library will read non-Y2K BUFR files as the old one does.  However, all
two digit years read are represented internally as four digit years, and any
files written with the new library will be in Y2K format.  A functional
conversion of two digit year inputs assumes the years 21-99 are in the
twentieth century, while years numbered 00-20 are in the twenty-first.  A Y2K
BUFR file is identified by a non-zero value in the 18th byte of the message
section one, the century byte.  At this point users of the library have access
to the full four digit year values read by including a signal in their programs
via a new entry point called DATELEN.  The plan is to have the default set to
return two digits of the year during a transition period.  This allows
implementation of the new BUFR Archive Library into a non-Y2K compliant
environment. The susbsequent list of subroutines have been changed for Y2K
compliance: BFRINI, DATEBF, DUMPBF, MSGINI, OPENMB, OPENMG, RDMEMM, READERME,
READFT, READMG, READTJ.

<u>Machine Independence</u>

Since the last implementation of the BUFR Archive Library, several areas in the
code have been identified which are problematic in some way with regards to
compiling the library on some computers.  Upgrades have been made to the
following list of subroutines to address these: CONWIN, INVCON, PARUSR,
RDTRER, READERM, READERME, TRYBUMP, UFBEVN, UFBGET, UFBINT, UFBRP, UFBRW,
UFBTAB and UNCMPS.

<u>Refinements, Corrections, and Improvements</u>

This is a list of BUFR Archive Library routines which were either in error,
or in need of some improvement: IRDERM, NEMTBB, NENUCK, RDBFDX, RDUSDX, STRCLN,
STRING, TABENT, UNCMPS and WRTREE.

<u>New Error Exit Subroutine</u>

Many of the BUFR Archive Library routines perform internal testing during
operation in order to prevent certain situation from generating mysterious
aborts, or, even worse, giving the wrong answers. The original library utilized
the Cray library routine ABORT to terminate a program when such a situation was
found.  The new library uses a new inernal subroutine, BORT, to accomplish
this.  The list of routines changed for this purpose is as follows: ADN30,
CHEKSTAB, CLOSMG, COPYBF, COPYMG, COPYSB, CPYMEM, CPYUPD, DATEBF, DRSTPL,
DUMPBF, DXMINI, ELEMDX, GETWIN, IDN30, IFBGET, INCTAB, INVMRG, IPKM, IUPM,
JSTIFY, LSTJPB, LSTRPC, LSTRPS, MAKESTAB, MSGINI, MSGUPD, MSGWRT, MVB, NEMTBA,
NEMTBB, NEMTBD, NENUCK, NEWWIN, NMSUB, NVNWIN, NXTWIN, OPENMB, OPENMG, OPENBF,
PAD, PARSEQ, PARUSR, PARUTG, PKC, POSAPN, POSAPX, RCSTPL, RDBFDX, RDMEMM,
RDMEMS, RDUSDX, READDX, READERM, READERME, READERS, READFT, READMG, READNS,
READSB, READTJ, SEQSDX, STANDARD, STATUS, STRING, TABENT, TABSUB, UFBCNT,
UFBCPY, UFBCUP, UFBDMP, UFBEVN, UFBGET, UFBINT, UFBMEM, UFBMMS, UFBMNS,
UFBOVR, UFBQCD, UFBQCP, UFBREP, UFBRMS, UFBTAM, UPTDD, USRTPL, VAL$, WRDLEN,
WRITDX, WRITSA, WRITSB, WTSTAT

<u>New Code Added</u>

I4DY      the two/four digit year conversion function
LJUST     a character left justify function
OPENBT    A dummy entry point which is relevant to users of the READTJ
          subroutine


## April 2, 1998

* BUFR Archive Library subroutine STRCLN, which initializes the mnemonic string
cache in the BUFR interface, is being modified to enlarge the cache from 50
elements to 1000, maximum.

* BUFR Archive Library subroutine STRING manages the mnemonic string cache in
the BUFR interface.  The mnemonic string cache is a performance enhancing
device which saves time when the same mnemonic strings are encountered in a
user program, over and over again (the typical scenario).  It is being
modified to operate a bigger cache, and some optimization of the cache search
algorithm is being made in support of a bigger cache.


## September 3, 1997

* Changes are being made to the BUFR Archive Library to recompile all routines
without the -ez compiler option.  The removal of this debugging option should
speed up the execution of all modules which are linked with BUFR Archive
Library routines.

* In addition, a new subroutine, STANDARD, is being added to the library.  This
subroutine "standardizes" NCEP BUFR messages for transmission.  It was
requested to process hurricane location data.


## July 29, 1997

* Three BUFR Archive Library subroutines were modified to update the current
BUFR version information written into Section 0 of each message:  DXMINI,
MSGINI and MSGWRT.  Version 3 replaces version 2.

* Three additional subroutines were modified to enable them to process GOES
soundings from NESDIS: IRDERM, RDTRER and READERME.


## December 17, 1996

The BUFR Archive Library was modified to make the following changes:

* DUMPBF - Corrected error in dump date reader.

* RDUSDX - Fixed for some MVS compiler's treatment of internal reads.

* RDBFDX - Fixed for some MVS compiler's treatment of internal reads.

* UFBINT - Modified to always initialize "USR" array to missing (10E10) when
           BUFR file is being read.


## December 11, 1996

The following subroutines were modified in the BUFR Archive Library:

* STATUS - Fixed a long standing bug which occurs in unusual situations. Very
           low impact.

* UFBINT - Removed a hard abort for users who try to write non-existing
           mnemonics.

* UFBRW  - Removed a hard abort for users who try to write non-existing
           mnemonics.

* ADDATE - New date arithmetic subroutine added to the library.

* DUMPBF - New dump date reader added to the library.

* MSGINI - Modified to allow inclusion of minutes in writing the message date
           into a BUFR message.

* READTJ - Specific database ingest message reader added to the library which
           can attach different BUFR tables if the message type is not
           recognized in the current ones.  Works with a user subroutine
           called OPENBT which specifies the location(s) of different tables.


## November 25, 1996

Several routines in the BUFR Archive Library are being modified to provide
more machine independence.  The data merging routine is being modified
for radiosonde call signs, a return code is being added to UFBINT when
mnemonics are not found, and READMG is being modified to exit gracefully when
the file is positioned after an end of file


## October 9, 1996

The BUFR Archive Library was modified to include 9 additional routines to
process ERS scatterometer data (IREADERS, RDTRER, READERS, UNCMPS), perform
fault tolerant reading (IREADFT, READFT), and support report part merging
(INVMRG, MRGINV, NWORDS).


## September 9, 1996

The BUFR Archive Library was separated into 121 BUFR interface routines,
which include upgrades and devices for operating the BUFR database.


## June 28, 1995

The BUFR Archive Library was modified to increase the size of internal arrays
in order to handle bigger files.  Coding was also added in order to process
ERS scatterometer data which is input from compressed BUFR messages (new
subroutine READERME).


## January 10, 1995

The BUFR Archive Library was modified slightly to allow for changes in the AVN
and FNL PREPBUFR and Q.C. Processing codes (PREPDATA, CQCBUFR, OIQCBUFR,
SSIANL).


## Original Implementation of BUFR Archive Library - January 6, 1994

Implemented on Cray-YMP as a single monolithic source bufr.f.  Only the AVN
and FNL PREPBUFR processing and q.c. codes used the BUFR Archive Library
initially.  These were: PREPDATA, SYNDATA, CQCBUFR, OIQCBUFR, and SSIANL.
These had actually been implemented with a non-production version of the
library in Jack Woollen's directory September 21, 1993.
