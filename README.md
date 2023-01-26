![Status](https://github.com/NOAA-EMC/NCEPLIBS-bufr/workflows/Build%20and%20Test/badge.svg)

## NCEPLIBS-bufr library

The NCEPLIBS-bufr library contains routines and utilites for working with the
[WMO BUFR](https://library.wmo.int/index.php?lvl=notice_display&id=10684#.Y70OSNLMJH7) format.
It is part of the [NCEPLIBS](https://github.com/NOAA-EMC/NCEPLIBS) project.

For full documentation of the library, see https://noaa-emc.github.io/NCEPLIBS-bufr/.

## Authors

Jack Woollen, Jeff Ator, Dennis Keyser, Stacey Bender, Diane Stokes, Jeff Whitaker, and Ron McLaren.

Code manager: Jeff Ator

## How to Build and Install

Download tarball from
[Releases](https://github.com/NOAA-EMC/NCEPLIBS-bufr/releases) and
unpack.

<pre>
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=path1 -DMASTER_TABLE_DIR=path2 ..
make -j4
ctest
make install
</pre>

Both `path1` and `path2` may be full or relative pathnames
on the system, up to a maximum of 90 characters each.

Installation of the library and utilities will be under `path1`.
Installation of the master BUFR tables will be under `path2`, or
under `path1` if `-DMASTER_TABLE_DIR=path2` is omitted
from the above cmake command.

If Python interoperability is desired, `-DENABLE_PYTHON=ON` can also
be added to the above cmake command.  However, version 3 of Python
must be installed and available on the system.

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
