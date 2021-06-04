
## User Guide
@brief Comprehensive discussion on how to incorporate the library within user application programs.

@anchor manual
The BUFR format is officially documented in
[WMO Manual 306, Volume I.2](https://library.wmo.int/index.php?lvl=notice_display&id=10684#.X68yu8hKiUn).
The latest edition of BUFR is edition 4, although edition 3 is still in use in many parts of the world.

@anchor wmomstab
BUFR is a table-driven format, meaning
that new parameters can be encoded and decoded by adding them to tables which are read in by the software,
rather than having to modify the software itself.  To that end, WMO periodically releases
new versions of their
[official WMO master BUFR tables](https://community.wmo.int/activity-areas/wmo-codes/manual-codes/latest-version),
to facilitate the continued exchange of meteorological data throughout the global community.

NCEPLIBS-bufr (also often referred to as BUFRLIB) is a software library that can read (decode) and
write (encode) data in both edition 3 and edition 4 of BUFR.  It consists of more than 300 different subroutines and
functions; however, a typical application program will never directly call more than 10 to 20 of them, and
the rest are lower-level routines that the library uses to accomplish various underlying tasks, and which
can therefore be considered as "black box" from a user perspective.

Whenever new versions of the official WMO master BUFR tables are released by WMO (as noted above),
they are downloaded and reformatted as new [NCEPLIBS-bufr master BUFR tables](@ref dfbfmstab) for use with the
software and distributed with the next release of the library.  However, users can also generate their own custom
[NCEPLIBS-bufr DX BUFR tables](@ref dfbftab) for use with the software as needed.

More details are available throughout the documentation, including sample utilities which demonstrate
how to use many of the various library subroutines and functions to accomplish different tasks.

@anchor hierarchy
For now, it's important to understand the following hierarchy:

1. Any BUFR file can consist of one or more BUFR messages.
2. Any BUFR message can consist of one or more BUFR data subsets, which are akin to reports from individual
observational stations at a particular time and location.
3. Any BUFR data subset can consist of one or more BUFR data descriptors and corresponding data values.

This in turn allows many of the most commonly used library subroutines and functions to be grouped accordingly,
based on which level of the hierarchy they operate at, and whether for reading/decoding BUFR data or
writing/encoding BUFR data:

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
  <td>openbf() openbf_f() ufbmem() ufbmex() cobfl()</td>
  <td>readmg() ireadmg() ireadmg_f() readerme() readns() ireadns() rdmemm() readmm() ireadmm() crbmg() datelen() getlens() iupvs01() iupbs01() iupbs3() nmsub() rtrcpt() igetdate()</td>
  <td>readsb() ireadsb() ireadsb_f() readns() ireadns() rdmems() ufbmms() ufbmns()</td>
  <td>ufbint() ufbrep() ufbseq() ufbstp() ufbint_f() ufbrep_f() ufbrms() readlc() getvalnb() setbmiss() getbmiss() ibfms() icbfms()</td>
</tr>
<tr>
  <th>writing/encoding</th>
  <td>openbf() openbf_f() closbf() closbf_f() copybf() cobfl()</td>
  <td>openmb() openmg() closmg() copymg() cpymem() cwbmg() cmpmsg() stdmsg() cnved4() strcpt() setblock() maxout() pkvs01() pkbs1() minimg()</td>
  <td>writsb() writsa() writcp() copysb() icopysb()</td>
  <td>ufbint() ufbrep() ufbseq() ufbstp() ufbint_f() ufbrep_f() writlc() setvalnb() setbmiss()</td>
</tr>
</table>
