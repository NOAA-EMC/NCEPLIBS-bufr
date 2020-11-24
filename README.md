![Status](https://github.com/NOAA-EMC/NCEPLIBS-bufr/workflows/Build%20and%20Test/badge.svg)

## BUFR library

The BUFR library contains Fortran programs and command line utilites
for working with the BUFR format. This is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For full documentation see https://noaa-emc.github.io/NCEPLIBS-bufr/.

## Authors

NCEP/EMC developers.

Code manager: Jeff Ator

## Installation

<pre>
git clone https://github.com/noaa-emc/nceplibs-bufr
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=./install ../nceplibs-bufr
make -j4
ctest
make install
</pre>

Installation of the library and utilities will be under ./install

## What is Built and Installed

The NCEPLIBS-bufr library is built in several different ways. All are
installed.

Three different type size combinations are supported, set with
compiler flags in the CMake build, for Intel and GNU compilers:

- "4" = 4-byte integers and 4-byte reals
- "8" = 8-byte integers and 8-byte reals
- "d" = 4-byte integers and 8-byte reals (sort of a hybrid)

In addition, NCEPLIBS-bufr can build with internally static or
internally dynamic array allocation. The build of the library which
internally uses dynamic allocations has a _DA suffix.

## Disclaimer

The United States Department of Commerce (DOC) GitHub project code is
provided on an "as is" basis and the user assumes responsibility for
its use. DOC has relinquished control of the information and no longer
has responsibility to protect the integrity, confidentiality, or
availability of the information. Any claims against the Department of
Commerce stemming from the use of its GitHub project will be governed
by all applicable Federal law. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of
Commerce. The Department of Commerce seal and logo, or the seal and
logo of a DOC bureau, shall not be used in any manner to imply
endorsement of any commercial product or activity by DOC or the United
States Government.


