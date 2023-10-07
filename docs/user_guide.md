@mainpage

## Documentation for Previous Versions of NCEPLIBS-bufr

* [NCEPLIBS-bufr-12.0.0](https://noaa-emc.github.io/NCEPLIBS-bufr/previous_versions/v12.0.0/index.html)
* [NCEPLIBS-bufr-11.7.1](https://noaa-emc.github.io/NCEPLIBS-bufr/previous_versions/v11.7.1/index.html)
* [NCEPLIBS-bufr-11.7.0](https://noaa-emc.github.io/NCEPLIBS-bufr/previous_versions/v11.7.0/index.html)
* [NCEPLIBS-bufr-11.6.0](https://noaa-emc.github.io/NCEPLIBS-bufr/previous_versions/v11.6.0/index.html)
* [NCEPLIBS-bufr-11.5.0](https://noaa-emc.github.io/NCEPLIBS-bufr/previous_versions/v11.5.0/index.html)

## Documentation for the Python API for NCEPLIBS-bufr

* [The Python API documentation](python/index.html)

## Introduction

This library contains subroutines, functions and other utilities that
can be used to read (decode) and write (encode) data in BUFR, which is
a [WMO](https://public.wmo.int) standard format for the exchange of
meteorological data.

## The BUFR Format

@anchor manual
The BUFR format is officially documented in [WMO Manual 306, Volume I.2](https://library.wmo.int/index.php?lvl=notice_display&id=10684#.X68yu8hKiUn).
The latest edition of BUFR is edition 4, although edition 3 is still
in use in many parts of the world.  A complete BUFR message consists
of the following sections:

| Section Number | Name | Contents |
| -------------- | ---- | -------- |
| 0 | Indicator section | "BUFR", length of message, edition number |
| 1 | Identification section | Originating center and subcenter, data category and subcategory, master table and version number |
| 2 | (Optional) Local Use section | (Optional) free-format additional information of potential interest to users | 
| 3 | Data Description section | Number of data subsets, compression indicator, and list of descriptors defining the content of each data subset |
| 4 | Data section | One or more data subsets, each containing values corresponding to the list of descriptors defined within Section 3 |
| 5 | End section | "7777" |

@anchor wmomstab
BUFR is a table-driven format, meaning that new descriptors can be
encoded and decoded by adding them to tables which are read in by the
software, rather than having to modify the software itself. To that
end, WMO periodically releases new versions of their
[official WMO master BUFR tables](https://community.wmo.int/activity-areas/wmo-codes/manual-codes/latest-version),
to facilitate the continued exchange of meteorological data throughout
the global community.

NCEPLIBS-bufr can read (decode) and write (encode) data in both
edition 3 and edition 4 of BUFR. It consists of more than 300
different subroutines and functions; however, a typical application
program will never directly call more than 10 to 20 of them, and the
rest are lower-level routines that the library uses to accomplish
various underlying tasks, and which can therefore be considered as
"black box" from a user perspective.

Whenever new versions of the official WMO master BUFR tables are
released by WMO (as noted above), they are downloaded and reformatted
as new [NCEPLIBS-bufr master BUFR tables](@ref dfbfmstab) for use with
the software and distributed with the next release of the
library. However, users can also generate their own custom
[NCEPLIBS-bufr DX BUFR tables](@ref dfbftab) for use with the software
as needed.

More details are available throughout the documentation, including
sample utilities which demonstrate how to use many of the various
library subroutines and functions to accomplish different tasks.

## NCEPLIBS-bufr Library Subroutines and Functions

@anchor hierarchy
It's important to understand the following hierarchy:

1. Any BUFR file can consist of one or more BUFR messages.
2. Any BUFR message can consist of one or more BUFR data subsets, which are akin to reports from individual
observational stations at a particular time and location.
3. Any BUFR data subset can consist of one or more BUFR data descriptors and corresponding data values.

This allows many of the most commonly used library subroutines and
functions to be grouped based on which level of the hierarchy they
operate at, and whether for reading/decoding BUFR data or
writing/encoding BUFR data.  In the following table, any routine with
a _f suffix wraps a Fortran routine of the same name and should only
be called from C application programs.  Conversely, any routine with
a _c suffix wraps a C function of the same name and should only be
called from Fortran application programs.

<table border>
<tr>
  <th>For working with:</th>
  <th>BUFR files</th>
  <th>BUFR messages</th>
  <th>BUFR data subsets</th>
  <th>BUFR data values</th>
</tr>
<tr>
  <th>reading/decoding</th>
  <td>openbf() openbf_f() ufbmem() ufbmex() cobfl() cobfl_c()</td>
  <td>readmg() ireadmg() ireadmg_f() readerme() readns() ireadns() rdmemm() readmm() ireadmm() crbmg() crbmg_c() datelen() getlens() iupvs01() iupbs01() iupbs01_f() iupbs3() nmsub() rtrcpt() igetdate()</td>
  <td>readsb() ireadsb() ireadsb_f() readns() ireadns() rdmems() ufbmms() ufbmns() ufbpos()</td>
  <td>ufbint() ufbint_f() ufbrep() ufbrep_f() ufbseq() ufbstp() ufbrms() readlc() getvalnb() setbmiss() getbmiss() ibfms() icbfms()</td>
</tr>
<tr>
  <th>writing/encoding</th>
  <td>openbf() openbf_f() closbf() closbf_f() copybf() cobfl() cobfl_c()</td>
  <td>openmb() openmg() closmg() copymg() cpymem() cwbmg() cwbmg_c() cmpmsg() stdmsg() cnved4() strcpt() setblock() maxout() pkvs01() pkbs1() minimg()</td>
  <td>writsb() writsa() writcp() copysb() icopysb()</td>
  <td>ufbint() ufbint_f() ufbrep() ufbrep_f() ufbseq() ufbstp() writlc() setvalnb() setbmiss()</td>
</tr>
</table>
