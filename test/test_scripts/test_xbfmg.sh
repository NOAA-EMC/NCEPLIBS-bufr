#!/bin/bash
# This is a test for NCEPLIBS-bufr.
# test the xbfmg utility.
# jack-woollen 2022-02-18
set -eu

cmd=$1
infiledir=$2
reffiledir=$3
xbcase=$4

${cmd} ${infiledir}/${xbcase}   # outfiles will be written to current working directory
rc=$?
[[ ${rc} -ne 0 ]] && exit ${rc}

reffiles=`ls -1 ${reffiledir}/${xbcase}.xbfmg.out.*`   # get complete list of reference files
for file in ${reffiles}; do  # compare each reference file to corresponding outfile
  filenum=`echo ${file} | cut -f4 -d.`
  outfile=${xbcase}.xbfmg.out.${filenum}
  [[ -s ${outfile} ]] || exit -1   # make sure this outfile was generated
  echo "comparing ... ${outfile}"
  cmp -s ${outfile} ${file}
  rc=$?
  [[ ${rc} -ne 0 ]] && exit ${rc}
done

exit 0  # if we made it here, we're good!
