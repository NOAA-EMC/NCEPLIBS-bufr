
## Master BUFR Tables
@brief Description and format of master BUFR tables files for use with the library
@anchor dfbfmstab

This document describes the format and concept of master BUFR tables. These are required by the BUFRLIB software
whenever the IO='SEC3' option is specified during a call to subroutine openbf() for the reading/decoding of a
given file of BUFR messages.  Otherwise, if a different value of IO is specified, then only a
[DX BUFR tables](@ref dfbftab) file is normally required, and master BUFR tables are not needed.

<br>
Whenever master BUFR tables are used, they are read in by the BUFRLIB software as a corresponding set of four
system files, from a directory on the local filesystem as specified within a separate preceding call to
subroutine mtinfo() :

- <i>Standard Table B</i> - an ordered listing of all descriptors corresponding to an official release
(i.e. version number) of the standard, international BUFR Table B
- <i>Local Table B</i> - an ordered listing of all local Table B descriptors supplementing the standard
BUFR Table B, as defined for use within a particular local originating center
- <i>Standard Table D</i> - an ordered listing of all descriptors corresponding to an official release
(i.e. version number) of the standard, international BUFR Table D
- <i>Local Table D</i> - an ordered listing of all local Table D descriptors supplementing the standard
BUFR Table D, as defined for use within a particular local originating center

Actual filenames use the following convention:

<pre>
Standard tables:     bufrtab.Table.<i>X</i>_STD_<i>M</i>_<i>V</i>
Local tables:        bufrtab.Table.<i>X</i>_LOC_<i>M</i>_<i>C</i>_<i>L</i>

        where:       <i>X</i> = Table type ('B' or 'D')
                     <i>M</i> = Master Table number ('0' for WMO, '10' for IOC, etc.)
                     <i>V</i> = Version number of Master Table used
                     <i>C</i> = Originating Center number ('7' for NCEP, etc.)
                     <i>L</i> = Version number of local tables used
</pre>

Whenever any of the [message-reading subroutines](@ref hierarchy) are used to read a new BUFR message
from a Fortran logical unit that was previously opened using subroutine
openbf() with IO='SEC3', the identification section (Section 1) of the message is automatically scanned to
determine the above values for that message. The BUFRLIB software then automatically generates the four necessary
filenames using those values and attempts to open and read each of those files from within the directory that was
specified as CMTDIR during the previous call to subroutine mtinfo(). This table information is then retained
and re-used in memory until a subsequent message is read which has a different value for any one of the
above values, and at which point a new set of tables is then read in from the same directory in order to be
applied to the new message. With this approach, the user can have multiple master table files stored within
the same directory, and the BUFRLIB software will always locate and read the appropriate files depending on
the corresponding values stored within Section 1 of each new message to be decoded. For more details about
the above values, see the discussion on Section 1 within the official
[WMO Manual 306, Volume I.2](@ref manual).

<br>
Now that we've discussed the content and naming conventions for master BUFR table files, let's turn our
attention to the actual format of these files:

<div id="TableB">
### Table B

As described above, two master Table B files (one standard and one local) are required for each BUFR message
that is to be read and decoded. The BUFRLIB software will scan the identification section (Section 1) of
each new message and then open and read the appropriate Table B files from within the directory specified by
CMTDIR during the previous call to subroutine mtinfo(). In particular, a standard Table B file must be
available corresponding to the version number of each BUFR message to be decoded, and the filename must
follow the naming convention described above. In addition, if there are any local descriptors contained
within the messages, then a local Table B file must also be available corresponding to the originating center
and local version numbers encoded within Section 1 of those messages. Otherwise, if the messages to be
decoded contain only standard descriptors (which is normally the case for data exchanged between operational
centers), then the
[default local Table B file from NCEP (originating center 7)](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_LOC_0_7_1)
can be used as a placeholder
since the software will not need to actually read any information from this table. In this way, users are
relieved from having to provide a local Table B file for every originating center whose messages they wish
to decode, and such a table only becomes necessary when the messages themselves actually contain one or more
local descriptors defined by that particular originating center.

<br>

Here now is the format for each master Table B:

<br>

The first line of the file is as follows, where the symbols correspond to those used in the file naming
convention described above. In this way, the software can perform internal consistency checks where needed.
Note that the separator for each field is a "|" character, but otherwise the free use of white space is
allowed within each field:

<pre>
Standard table:     Table B STD | <i>M</i> | <i>V</i>
Local table:        Table B LOC | <i>M</i> | <i>C</i> | <i>L</i>

       where:       <i>M</i> = Master Table number ('0' for WMO, '10' for IOC, etc.)
                    <i>V</i> = Version number of Master Table used
                    <i>C</i> = Originating Center number ('7' for NCEP, etc.)
                    <i>L</i> = Version number of local tables used
</pre>

Following this first header line, the remainder of the file contains a listing of all possible Table B
descriptors corresponding to the specified version number of the table, ordered in ascending order with
respect to the FXY number as shown below. Blank lines as well as comments (i.e. any line beginning with
the character "#") are permitted throughout the file and will be ignored by the software, up until a line
is reached which contains the string "END" in the first three characters, and which is a signal to the
software to stop looking for any new Table B descriptor definitions in the table. Each definition line
has the following format, where the "|" and ";" characters are required separators as shown, but
otherwise the additional use of white space is allowed within each field.

<pre>
 F-XX-YYY | SCALE | REFERENCE | BITS | UNITS  | MNEMONIC ; CODES ; ELEMENT NAME
</pre>

The CODES field can be used for any annotations the user may wish to add for a particular definition line,
or it can be left blank. All other fields should be self-explanatory based on earlier discussions and
should contain a value. Here are some examples:

<pre>
  0-01-018 |   0 |  0 |  40 | CCITT IA5   | SSTN    ; ; Short station or site name
  0-01-041 |   5 | -1073741824 |  31 | m/s | PS00   ;  ; Absolute platform velocity - first component
  0-05-002 |   2 |       -9000 |  15 | Degree(N+,S-)  | CLAT     ;     ; Latitude (coarse accuracy)
  0-07-002 |  -1 |         -40 |  16 | m              | HMSL     ;     ; Height or altitude
</pre>

The above examples show how additional white space can vary from line to line and can be used according
to individual preferences, but of course for overall readability it's usually best to pick one format
and stick to it within a given table file. Here are some sample master Table B files that can be downloaded
and used.  These are also available as part of the standard distribution package for the software, within
the tables subdirectory:

- [Standard Table B for Master Table 0 (WMO), Version 13](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_13)
- [Standard Table B for Master Table 0 (WMO), Version 14](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_14)
- [Standard Table B for Master Table 0 (WMO), Version 15](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_15)
- [Standard Table B for Master Table 0 (WMO), Version 16](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_16)
- [Standard Table B for Master Table 0 (WMO), Version 17](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_17)
- [Standard Table B for Master Table 0 (WMO), Version 18](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_18)
- [Standard Table B for Master Table 0 (WMO), Version 19](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_19)
- [Standard Table B for Master Table 0 (WMO), Version 20](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_20)
- [Standard Table B for Master Table 0 (WMO), Version 21](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_21)
- [Standard Table B for Master Table 0 (WMO), Version 22](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_22)
- [Standard Table B for Master Table 0 (WMO), Version 23](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_23)
- [Standard Table B for Master Table 0 (WMO), Version 24](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_24)
- [Standard Table B for Master Table 0 (WMO), Version 25](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_25)
- [Standard Table B for Master Table 0 (WMO), Version 26](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_26)
- [Standard Table B for Master Table 0 (WMO), Version 27](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_27)
- [Standard Table B for Master Table 0 (WMO), Version 28](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_28)
- [Standard Table B for Master Table 0 (WMO), Version 29](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_29)
- [Standard Table B for Master Table 0 (WMO), Version 30](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_30)
- [Standard Table B for Master Table 0 (WMO), Version 31](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_31)
- [Standard Table B for Master Table 0 (WMO), Version 32](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_32)
- [Standard Table B for Master Table 0 (WMO), Version 33](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_33)
- [Standard Table B for Master Table 0 (WMO), Version 34](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_34)
- [Standard Table B for Master Table 0 (WMO), Version 35](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_35)
- [Standard Table B for Master Table 0 (WMO), Version 36](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_36)
- [Standard Table B for Master Table 0 (WMO), Version 37](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_37)
- [Standard Table B for Master Table 0 (WMO), Version 38](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_STD_0_38)
- [Local Table B for Master Table 0 (WMO), Originating Center 7 (NCEP), Version 1](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableB_LOC_0_7_1)
</div>

<div id="TableD">
### Table D

As was the case for [Table B](#TableB), two master Table D files (one standard and one local) are also
required for each BUFR message that is to be read and decoded, and these files must also exist within
the same directory specified by CMTDIR during the previous call to subroutine mtinfo(). The BUFRLIB
software will then scan the identification section (Section 1) of each BUFR message as it is read, in
order to determine the exact master table files to open and read for that message, and a local Table D
file is necessary whenever local descriptors from the originating center in question are included
within a message; otherwise, the
[default local Table D file from NCEP (originating center 7)](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_LOC_0_7_1)
can be used as a placeholder.

<br>

The format of the first line of each master Table D file is the same as for Table B:

<pre>
Standard table:     Table D STD | <i>M</i> | <i>V</i>
Local table:        Table D LOC | <i>M</i> | <i>C</i> | <i>L</i>

       where:       <i>M</i> = Master Table number ('0' for WMO, '10' for IOC, etc.)
                    <i>V</i> = Version number of Master Table used
                    <i>C</i> = Originating Center number ('7' for NCEP, etc.)
                    <i>L</i> = Version number of local tables used
</pre>

and blank lines and comment lines (beginning with a "#" in column 1) are likewise allowed, but
otherwise the format for a master Table D file differs from that of a master Table B file since
entries now span multiple file lines as follows:

<pre>
 F-XX-YYY | MNEMONIC  ; CODES ; SEQUENCE_NAME
          | F-XX-YYY > | ELEMENT_NAME  
          | F-XX-YYY   | ELEMENT_NAME 
</pre>

As shown, the first line of each entry contains the FXY number of the Table D sequence, along with the
corresponding sequence name, mnemonic and annotation codes (if any), and then each successive line
contains a single element of that sequence. Each successive element is listed one per line, and note
the ">" character after the FXY number within each element line up to, but not including, the last
element of the sequence. The BUFRLIB software uses the lack of a ">" character to know when it has
reached the last element of the sequence. Here are some examples:

<pre>
  3-01-004 | SFCSTNID   ;  ; Surface station identification
           | 0-01-001 > | WMO block number
           | 0-01-002 > | WMO station number
           | 0-01-015 > | Station or site name
           | 0-02-001   | Type of station

  3-01-012 | HHMM       ; ;
           | 0-04-004 >  | Hour
           | 0-04-005    | Minute

  3-01-023 | LTLONC   ;   ;
           | 0-05-002 > | Latitude (coarse accuracy)
           | 0-06-002   | Longitude (coarse accuracy)

  3-01-025 | LTLONCDT   ;  ;
           | 3-01-023 > | Latitude and longitude (coarse accuracy)
           | 0-04-003 > | Day
           | 3-01-012   | Time

  3-01-045 | SATLOVEL   ;     ; Satellite location and velocity
           | 3-01-011 > | Year, month, day
           | 3-01-012 > | Time (hour, minute)
           | 2-01-138 > | Change width to 16 bits
           | 2-02-131 > | Change scale to 3
           | 0-04-006 > | Second
           | 2-01-000 > | Change width back to Table B
           | 2-02-000 > | Change scale back to Table B
           | 3-04-030 > | Location relative to the Earth's centre
           | 3-04-031   | Velocity relative to the Earth's centre

  3-03-050 | WDPLRAOB   ; ; Wind data at a pressure level with radiosonde position
           | 0-04-086 > | Long time period or displacement (since launch time)
           | 0-08-042 > | Extended vertical sounding significance
           | 0-07-004 > | Pressure
           | 0-05-015 > | Latitude displacement since launch site (high accuracy)
           | 0-06-015 > | Longitude displacement since launch site (high accuracy)
           | 0-11-001 > | Wind direction
           | 0-11-002   | Wind speed
</pre>

As with master [Table B](#TableB) files, the entries within a master Table D file must all be sorted in
ascending order with respect to the FXY number, and the software will continue reading from the file
until it reaches a line with the string "END" in the first three characters. Here are some sample master
Table D files that can be downloaded and used.  These are also available as part of the standard
distribution package for the software, within the tables subdirectory:

- [Standard Table D for Master Table 0 (WMO), Version 13](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_13)
- [Standard Table D for Master Table 0 (WMO), Version 14](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_14)
- [Standard Table D for Master Table 0 (WMO), Version 15](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_15)
- [Standard Table D for Master Table 0 (WMO), Version 16](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_16)
- [Standard Table D for Master Table 0 (WMO), Version 17](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_17)
- [Standard Table D for Master Table 0 (WMO), Version 18](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_18)
- [Standard Table D for Master Table 0 (WMO), Version 19](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_19)
- [Standard Table D for Master Table 0 (WMO), Version 20](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_20)
- [Standard Table D for Master Table 0 (WMO), Version 21](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_21)
- [Standard Table D for Master Table 0 (WMO), Version 22](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_22)
- [Standard Table D for Master Table 0 (WMO), Version 23](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_23)
- [Standard Table D for Master Table 0 (WMO), Version 24](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_24)
- [Standard Table D for Master Table 0 (WMO), Version 25](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_25)
- [Standard Table D for Master Table 0 (WMO), Version 26](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_26)
- [Standard Table D for Master Table 0 (WMO), Version 27](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_27)
- [Standard Table D for Master Table 0 (WMO), Version 28](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_28)
- [Standard Table D for Master Table 0 (WMO), Version 29](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_29)
- [Standard Table D for Master Table 0 (WMO), Version 30](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_30)
- [Standard Table D for Master Table 0 (WMO), Version 31](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_31)
- [Standard Table D for Master Table 0 (WMO), Version 32](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_32)
- [Standard Table D for Master Table 0 (WMO), Version 33](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_33)
- [Standard Table D for Master Table 0 (WMO), Version 34](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_34)
- [Standard Table D for Master Table 0 (WMO), Version 35](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_35)
- [Standard Table D for Master Table 0 (WMO), Version 36](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_36)
- [Standard Table D for Master Table 0 (WMO), Version 37](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_37)
- [Standard Table D for Master Table 0 (WMO), Version 38](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_STD_0_38)
- [Local Table D for Master Table 0 (WMO), Originating Center 7 (NCEP), Version 1](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableD_LOC_0_7_1)
</div>

<div id="CodeFlag">
### Code/Flag Tables

Unlike for [Table B](#TableB) and [Table D](#TableD), master Code/Flag tables are optional when
the IO='SEC3' option is specified during a call to subroutine openbf() for the reading/decoding
of BUFR messages.  Instead, they are only required if the user intends to make one or more
calls to subroutine getcfmng(), and in which case a prior call to subroutine codflg() is also
required with the value of CF set to 'Y'.

<br>

Whenever master Code/Flag tables are used, they must exist within the same local filesystem
directory specified by CMTDIR during the most recent call to subroutine mtinfo(). And just like
for Table B and Table D, they must exist as a set of two separate tables files, one containing
all of the standard entries and one containing all of the local entries, and where the
[default local Code/Flag tables file from NCEP (originating center 7)](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.TableF_LOC_0_7_1)
can be used as a placeholder for the local file whenever the BUFR messages to be decoded contain
only standard descriptors.

<br>

The format of the first line of each master Code/Flag table file is the same as for Tables B and D:

<pre>
Standard table:     Table F STD | <i>M</i> | <i>V</i>
Local table:        Table F LOC | <i>M</i> | <i>C</i> | <i>L</i>

       where:       <i>M</i> = Master Table number ('0' for WMO, '10' for IOC, etc.)
                    <i>V</i> = Version number of Master Table used
                    <i>C</i> = Originating Center number ('7' for NCEP, etc.)
                    <i>L</i> = Version number of local tables used
</pre>

and blank lines and comment lines (beginning with a "#" in column 1) are likewise allowed
throughout each table for readability. Entries span multiple lines and follow the format:

<pre>
 F-XX-YYY | MNEMONIC ; CODE
          | F-XX-YYY=VAL
            | VAL > | MEANING
            | VAL   | MEANING
</pre>

for code tables, and:

<pre>
 F-XX-YYY | MNEMONIC ; FLAG
          | F-XX-YYY=BIT
            | BIT > | MEANING
            | BIT   | MEANING
</pre>

for flag tables. As shown, the first line of each entry contains the FXY number and mnemonic
of the associated Table B descriptor, along with the identifier "CODE" or "FLAG" depending on
the table type. Successive lines contain individual entries within the associated table,
grouped by dependency where applicable. For example, for mnemonic GSES (originating sub-center),
whose meanings depend on that of the associated originating center as noted in the discussion
for subroutine getcfmng(), the entry would look something like the following, and where 0-01-031,
0-01-033 and 0-01-035 are the respective FXY numbers for mnemonics GCLONG, OGCE and ORIGC:

<pre>
  0-01-034 | GSES ; CODE
           | 0-01-031,0-01-033,0-01-035=39
              | 0 > | No sub-centre
              | 225 > | Beijing
              | 226 > | Guangzhou
              | 228 > | Urumuqi
           | 0-01-031,0-01-033,0-01-035=80
              | 0 > | No sub-centre
              | 101 > | Albania (NMC)
              | 102 > | National Research Council/Institute of Atmospheric Sciences and Climate (CNR-ISAC)
           | 0-01-031,0-01-033,0-01-035=7
              | 0 > | No sub-centre
              | 1 > | NCEP Reanalysis Project
              | 2 > | NCEP Ensemble Products
              | 3 > | NCEP Central Operations
              | 4 > | Environmental Modeling Center
              | 5 > | Weather Prediction Center
              | 6 > | Ocean Prediction Center
              | 7 > | Climate Prediction Center
              | 8 > | Aviation Weather Center
              | 9 > | Storm Prediction Center
              | 10 > | National Hurricane Center
              | 11 > | NWS Techniques Development Laboratory
              | 12 > | NESDIS Office of Research and Applications
              | 13 > | Federal Aviation Administration
              | 14 > | NWS Meteorological Development Laboratory
              | 15 > | North American Regional Reanalysis Project
              | 16 > | Space Weather Prediction Center
              | 17 > | ESRL Global Systems Division
           | 0-01-031,0-01-033,0-01-035=46
              | 0 > | No sub-centre
              | 10 > | Cachoeira Paulista (INPE)
              | 11 > | Cuiaba (INPE)
              | 12 > | Brasilia (INMET)
              | 13 > | Fortaleza (FUNCEME)
              | 14 > | Natal (Navy Hygrog. Centre)
              | 15 > | Manaus (SIVAM)
              | 16 > | Natal (INPE)
              | 17 > | Boa Vista
              | 25 > | São Paulo University - USP
           | 0-01-031,0-01-033,0-01-035=254
              | 0 > | No sub-centre
              | 10 > | Tromso (Norway)
              | 20 > | Maspalomas (Spain)
              | 30 > | Kangerlussuaq (Greenland)
              | 40 > | Edmonton (Canada)
              | 50 > | Bedford (Canada)
              | 60 > | Gander (Canada)
              | 70 > | Monterey (USA)
              | 80 > | Wallops Island (USA)
              | 90 > | Gilmor Creek (USA)
              | 100 > | Athens (Greece)
              | 120 > | Ewa Beach, Hawaii
              | 125 > | Ford Island, Hawaii
              | 130 > | Miami, Florida
              | 140 > | Lannion (France)
              | 150 > | Svalbard (Norway)
              | 170 > | St Denis (La Réunion)
              | 180 > | Moscow
              | 190 > | Muscat
              | 200 > | Khabarovsk
              | 210   | Novosibirsk
</pre>

However, if the meanings of the code or flag table values for a particular mnemonic do not
depend on those of any other mnemonic, then the dependency lines can be omitted from the
above format, as shown in the following additional examples:

<pre>
  0-02-003 | A4ME ; CODE
              | 0 > | Pressure Instrument associated with wind measuring equipment
              | 1 > | Optical theodolite
              | 2 > | Radio theodolite
              | 3 > | Radar
              | 4 > | VLF-Omega
              | 5 > | Loran C
              | 6 > | Wind profiler
              | 7 > | Satellite navigation
              | 8 > | Radio-acoustic Sounding System (RASS)
              | 9 > | Sodar
              | 10 > | LIDAR
              | 14   | Pressure instrument associated with wind measuring equipment but pressure element failed during ascent

  0-02-008 | TOFSP ; CODE
              | 0 > | Fixed platform
              | 1 > | Mobile offshore drill ship
              | 2 > | Jack-up rig
              | 3 > | Semi-submersible platform
              | 4 > | FPSO (floating production storage and offloading unit)
              | 5   | Light vessel

  0-02-016 | RCONF ; FLAG
              | 1 > | Train regulator
              | 2 > | Light unit
              | 3 > | Parachute
              | 4   | Rooftop release

  0-02-017 | CAHM ; CODE
              | 0 > | No corrections
              | 1 > | Time lag correction provided by the manufacturer
              | 2 > | Solar radiation correction provided by the manufacturer
              | 3 > | Solar radiation and time lag correction provided by the manufacturer
              | 7   | GRUAN solar radiation and time lag correction

  0-02-022 | SDPT ; FLAG
              | 1 > | Processing technique not defined
              | 2 > | Automated statistical regression
              | 3 > | Clear path
              | 4 > | Partly cloudy path
              | 5   | Cloudy path
</pre>

In any case, note that the values for any entry are always listed one per line, and note
the ">" character after the value within each line up to, but not including, the last defined
value/meaning pair for that entry. The BUFRLIB software uses this lack of a ">" character to
know when it has reached the last value/meaning pair for the associated entry. And as was also
the case for master Table B and Table D files, all entries in a master Code/Flag table file
must be in sorted ascending order with respect to the FXY number, and the BUFRLIB software
will continue reading from the file until it reaches a line with the string "END" in the first
three characters. Here are some sample master code/flag table files that can be downloaded and
used.  These are also available as part of the standard
distribution package for the software, within the tables subdirectory:

- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 13](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_13)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 14](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_14)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 15](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_15)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 16](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_16)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 17](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_17)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 18](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_18)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 19](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_19)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 20](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_20)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 21](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_21)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 22](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_22)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 23](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_23)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 24](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_24)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 25](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_25)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 26](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_26)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 27](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_27)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 28](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_28)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 29](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_29)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 30](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_30)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 31](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_31)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 32](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_32)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 33](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_33)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 34](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_34)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 35](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_35)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 36](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_36)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 37](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_37)
- [Standard Code/Flag Tables for Master Table 0 (WMO), Version 38](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_STD_0_38)
- [Local Code/Flag Tables for Master Table 0 (WMO), Originating Center 7 (NCEP), Version 1](https://github.com/NOAA-EMC/NCEPLIBS-bufr/tree/develop/tables/bufrtab.CodeFlag_LOC_0_7_1)
</div>
