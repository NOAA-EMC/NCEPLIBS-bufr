#!/bin/sh

#  ------------------------------------------------------------------------
#  This script generates BUFR messages corresponding to a given user DX
#  (BUFR dictionary) table and appends them to a given BUFR data file.
#  It is primarily intended for use when implementing DX table changes
#  to a BUFR tankfile in the NCEP observational database.
#
#  NOTE: The script is set up to run in the Bourne shell. If you are a
#  C-shell user, enter 'sh ./apxdx.sh'.
#  ------------------------------------------------------------------------
#  Usage: apxdx.sh <BUFRfile> <DXtable>
#
#  where:
#  <BUFRfile>  [path/]filename of BUFR data file to be appended
#  <DXtable>   [path/]filename of DX dictionary table
#  ------------------------------------------------------------------------

[ $# -ne 2 ] && { echo; echo "Usage: $0 <BUFRfile> <DXtable>";
                  echo;
                  echo "where:";
                  echo "  <BUFRfile> = [path/]filename of BUFR data file";
                  echo "  <DXtable>  = [path/]filename of DX table";
                  echo; exit 99; }

utilexec=${utilexec:-/nwprod/util/exec}

cat<<eof|$utilexec/apxdx
$1
$2
eof
err=$?
exit $err
