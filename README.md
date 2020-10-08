![Status](https://github.com/NOAA-EMC/NCEPLIBS-sp/workflows/Build%20and%20Test/badge.svg)

## BUFR library

The BUFR library contains Fortran programs and command line utilites
for working with the BUFR format. This is part of the
[NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

## Authors

NCEP/EMC developers.

Code manager: Jeff Ator

## Installation

<pre>
git clone https://github.com/noaa-emc/nceplibs-bufr
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=./install -DENABLE_TESTS=ON nceplibs-bufr
make -j4
ctest
make install
</pre>

Installation of the library and utilities will be under ./install

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


