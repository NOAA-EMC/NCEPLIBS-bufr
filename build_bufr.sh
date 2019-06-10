#!/bin/sh

 (( $# == 0 )) && {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [prefix=<installpath>] [[local]install[only]]" >&2
   exit 1
 }

 sys=${1,,}
 [[ $sys == wcoss || $sys == dell || $sys == cray ||\
    $sys == theia || $sys == intel_general || $sys == gnu_general ]] || {
   echo "*** Usage: $0 wcoss|dell|cray|theia|intel_general|gnu_general [debug|build] [prefix=<installpath>] [[local]install[only]]" >&2
   exit 1
 }
 debg=false
 inst=false
 skip=false
 local=false
 instloc="---"
 for n in 2 3 4; do
   (( $# > (n-1) )) && {
     [[ ${!n,,} == build ]] && debg=false
     [[ ${!n,,} == debug ]] && debg=true
     [[ ${!n,,} == install ]] && inst=true
     [[ ${!n,,} == localinstall ]] && { local=true; inst=true; }
     [[ ${!n,,} == installonly ]] && { inst=true; skip=true; }
     [[ ${!n,,} == localinstallonly ]] && { local=true; inst=true; skip=true; }
     [[ ${!n,,} == prefix=* ]] && {
       local=false
       instloc=${!n:$(expr length "prefix=")}
     }
   }
 done

 source ./Conf/Collect_info.sh
 source ./Conf/Gen_cfunction.sh
 source ./Conf/Reset_version.sh

 if [[ ${sys} == "intel_general" ]]; then
   sys6=${sys:6}
   source ./Conf/Bufr_${sys:0:5}_${sys6^}.sh
 elif [[ ${sys} == "gnu_general" ]]; then
   sys4=${sys:4}
   source ./Conf/Bufr_${sys:0:3}_${sys4^}.sh
 else
   source ./Conf/Bufr_intel_${sys^}.sh
 fi
 $CC --version &> /dev/null || {
   echo "??? BUFR: compilers not set." >&2
   exit 1
 }
 [[ -z $BUFR_VER || -z $BUFR_LIB4 ]] && {
   echo "??? BUFR: module/environment not set." >&2
   exit 1
 }

set -x
 bufrLib4=$(basename ${BUFR_LIB4})
 bufrLib8=$(basename ${BUFR_LIB8})
 bufrLibd=$(basename ${BUFR_LIBd})
 bufrLib4da=$(basename ${BUFR_LIB4_DA})
 bufrLib8da=$(basename ${BUFR_LIB8_DA})
 bufrLibdda=$(basename ${BUFR_LIBd_DA})

#################
 cd src
#################

#-------------------------------------------------------------------
# Get the version number for this build from the bvers.f source file
#
 version=v$(grep "CVERSTR =" bvers.f | tr -d "\t CVERSTR='")
 echo "version is $version"
 echo

 $skip || {
#-------------------------------------------------------------------
# Get byte order from cpu information
#
   bOrder=$(lscpu | grep "Byte Order" | tr -d " ")
   if [[ $bOrder == "ByteOrder:BigEndian" ]]; then
      byte_order=BIG_ENDIAN
   elif [[ $bOrder == "ByteOrder:LittleEndian" ]]; then
      byte_order=LITTLE_ENDIAN
   else
      bOrder=$(python2 -c "import sys; print sys.byteorder")
      if [[ $bOrder == "big" ]]; then
         byte_order=BIG_ENDIAN
      elif [[ $bOrder == "little" ]]; then
         byte_order=LITTLE_ENDIAN
      fi
   fi

#-------------------------------------------------------------------
# Start building libraries
#
   for array_type in DYNAMIC STATIC; do
      FPPCPP="-D${byte_order} -D${array_type}_ALLOCATION"
      CFLAGSDEFS="-D${array_type}_ALLOCATION"
#
#     Update 4-byte version of libbufr_4.a
#
 echo
 echo "   ... build i4/r4 bufr library ..."
 echo
      FFLAGS4="$I4R4 $FFLAGS"
      [[ $array_type == "DYNAMIC"  ]] && {
        export LIB=$bufrLib4da
        bufrInfo4=bufr_info_and_log4da.txt
        collect_info bufr 4/4da OneLine4 LibInfo4
      }
      [[ $array_type == "STATIC"  ]] && {
        export LIB=$bufrLib4
        bufrInfo4=bufr_info_and_log4.txt
        collect_info bufr 4 OneLine4 LibInfo4
      }
      make clean
      setx_status=${-//[^x]/}
      [[ -n $setx_status ]] && set +x
      echo "$bufrInfo0" > $bufrInfo4
      [[ -n $setx_status ]] && set -x
      $debg && make debug FFLAGS="$FFLAGS4" &>> $bufrInfo4 \
            || make build FFLAGS="$FFLAGS4" &>> $bufrInfo4
      make message MSGSRC="$(gen_cfunction $bufrInfo4 OneLine4 LibInfo4)"

#
#     Update 8-byte version of libbufr_8.a
#
 echo
 echo "   ... build i8/r8 bufr library ..."
 echo
      FFLAGS8="$I8R8 $FFLAGS"
      [[ $array_type == "DYNAMIC"  ]] && {
        export LIB=$bufrLib8da
        bufrInfo8=bufr_info_and_log8da.txt
        collect_info bufr 8/8da OneLine8 LibInfo8
      }
      [[ $array_type == "STATIC"  ]] && {
        export LIB=$bufrLib8
        bufrInfo8=bufr_info_and_log8.txt
        collect_info bufr 8 OneLine8 LibInfo8
      }
      make clean
      setx_status=${-//[^x]/}
      [[ -n $setx_status ]] && set +x
      echo "$bufrInfo0" > $bufrInfo8
      [[ -n $setx_status ]] && set -x
      $debg && make debug FFLAGS="$FFLAGS8" &>> $bufrInfo8 \
            || make build FFLAGS="$FFLAGS8" &>> $bufrInfo8
      make message MSGSRC="$(gen_cfunction $bufrInfo8 OneLine8 LibInfo8)"

#
#     Update 8-byte version of libbufr_d.a
#
 echo
 echo "   ... build i4/r8 bufr library ..."
 echo
      FFLAGSd="$I4R8 $FFLAGS"
      [[ $array_type == "DYNAMIC"  ]] && {
        export LIB=$bufrLibdda
        bufrInfod=bufr_info_and_logdda.txt
        collect_info bufr d/dda OneLined LibInfod
      }
      [[ $array_type == "STATIC"  ]] && {
        export LIB=$bufrLibd
        bufrInfod=bufr_info_and_logd.txt
        collect_info bufr d OneLined LibInfod
      }
      make clean
      setx_status=${-//[^x]/}
      [[ -n $setx_status ]] && set +x
      echo "$bufrInfo0" > $bufrInfod
      [[ -n $setx_status ]] && set -x
      $debg && make debug FFLAGS="$FFLAGSd" &>> $bufrInfod \
            || make build FFLAGS="$FFLAGSd" &>> $bufrInfod
      make message MSGSRC="$(gen_cfunction $bufrInfod OneLined LibInfod)"

   done
 }

 $inst && {
#
#     Install libraries and source files 
#
   $local && {
              instloc=..
              LIB_DIR4=$instloc
              LIB_DIR8=$instloc
              LIB_DIRd=$instloc
              LIB_DIR4da=$instloc
              LIB_DIR8da=$instloc
              LIB_DIRdda=$instloc
              SRC_DIR=
             } || {
              [[ $instloc == --- ]] && {
                LIB_DIR4=$(dirname $BUFR_LIB4)
                LIB_DIR8=$(dirname $BUFR_LIB8)
                LIB_DIRd=$(dirname $BUFR_LIBd)
                LIB_DIR4da=$(dirname $BUFR_LIB4_DA)
                LIB_DIR8da=$(dirname $BUFR_LIB8_DA)
                LIB_DIRdda=$(dirname $BUFR_LIBd_DA)
                SRC_DIR=$BUFR_SRC
              } || {
                LIB_DIR4=$instloc
                LIB_DIR8=$instloc
                LIB_DIRd=$instloc
                LIB_DIR4da=$instloc
                LIB_DIR8da=$instloc
                LIB_DIRdda=$instloc
                SRC_DIR=$instloc/src
                [[ $instloc == .. ]] && SRC_DIR=
              }
              [ -d $LIB_DIR4 ] || mkdir -p $LIB_DIR4
              [ -d $LIB_DIR8 ] || mkdir -p $LIB_DIR8
              [ -d $LIB_DIRd ] || mkdir -p $LIB_DIRd
              [ -d $LIB_DIR4da ] || mkdir -p $LIB_DIR4_DA
              [ -d $LIB_DIR8da ] || mkdir -p $LIB_DIR8_DA
              [ -d $LIB_DIRdda ] || mkdir -p $LIB_DIRd_DA
              [ -z $SRC_DIR ] || { [ -d $SRC_DIR ] || mkdir -p $SRC_DIR; }
             }

   make clean LIB=
   make install LIB=$bufrLib4da LIB_DIR=$LIB_DIR4da SRC_DIR=
   make install LIB=$bufrLib8da LIB_DIR=$LIB_DIR8da SRC_DIR=
   make install LIB=$bufrLibdda LIB_DIR=$LIB_DIRdda SRC_DIR=
   make install LIB=$bufrLib4   LIB_DIR=$LIB_DIR4   SRC_DIR=
   make install LIB=$bufrLib8   LIB_DIR=$LIB_DIR8   SRC_DIR=
   make install LIB=$bufrLibd   LIB_DIR=$LIB_DIRd   SRC_DIR=$SRC_DIR
 }

