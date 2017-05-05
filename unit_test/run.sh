#!/bin/bash

#BUFRLIB="/gpfs/hps/nco/ops/nwprod/lib/bufr/v11.0.1/intel/libbufr_v11.0.1_4_64_DA.a"
#BUFRLIB="/gpfs/hps/nco/ops/nwprod/lib/bufr/v11.0.1/intel/libbufr_v11.0.1_4_64.a"
#BUFRLIB="/gpfs/hps/emc/meso/noscrub/Jeff.Ator/intel/libbufr_v11.0.2_4_64_DA.a"
#BUFRLIB="/gpfs/hps/nco/ops/nwtest/lib/bufr/v11.0.2/cray/libbufr_v11.0.2_4_64.a"
#BUFRLIB="/nwprod2/lib/bufr/v11.1.0/libbufr_v11.1.0_4_64.a"
#BUFRLIB="/gpfs/hps/emc/meso/noscrub/Jeff.Ator/bufrlib/cray/libbufr_v11.1.0_4_64_DA.a"
BUFRLIB=/nwtest2/lib/bufr/v11.2.0/libbufr_v11.2.0_4_64.a
#BUFRLIB=/meso/save/Jeff.Ator/sib-bufrlib/trunk/libbufr_v11.2.0_4_64_DA.a
#BUFRLIB=/ptmpp1/Jeff.Ator/v11.2.0/libbufr_v11.2.0_4_64_DA.a

echo
echo "testing BUFRLIB in $BUFRLIB"

comp=intel  # set intel as the default compiler environment

hncc1="`hostname | cut -c1`"

if [ ${hncc1} = "l" -o ${hncc1} = "s" ]  # luna or surge
then
    ml=`module list 2>&1 1>/dev/null`   # discard the stdout and save only the stderr

    if [ `echo $BUFRLIB | grep -c cray` -gt 0 ]
    then
	if [ `cat /etc/dev | cut -c1` != ${hncc1} ]
	then
	    echo "cannot bsub on production Cray machine; use development machine instead"
	    exit
	fi
	comp=cray  # reset the compiler environment to cray
	#   confirm we have everything we need for the cray environment
	if [ `echo $ml | grep -c PrgEnv-cray` -eq 0 ]
	then
	    module switch PrgEnv-intel PrgEnv-cray
	fi
	if [ `echo $ml | grep -c craype-haswell` -eq 0 ]
	then
	    module switch craype-sandybridge craype-haswell
	fi
    else
	#   confirm we have everything we need for the intel environment 
	if [ `echo $ml | grep -c PrgEnv-intel` -eq 0 ]
	then
	    module switch PrgEnv-cray PrgEnv-intel
	fi
	if [ `echo $ml | grep -c craype-sandybridge` -eq 0 ]
	then
	    module switch craype-haswell craype-sandybridge
	fi
    fi
    module list
fi

if [ `echo $BUFRLIB | grep -c _DA` -eq 1 ]
then  #  we're testing a dynamic allocation build of the BUFRLIB
    bufrlib_build="dynamic"
else
    bufrlib_build="static"
fi

if [ `uname` = "AIX" ]
then
    FC=ncepxlf
    FFLAGS="-O3"
elif [ `uname` = "Linux" ]
then
    if [ ${hncc1} = "l" -o ${hncc1} = "s" ]  # luna or surge
    then
	FC=ftn
    else
	FC=ifort
    fi
    FFLAGS="-g"
fi

######################################################
#  build the test programs
######################################################

echo
echo "building test_IN_1"
$FC $FFLAGS -o test_IN_1.x test_IN_1.f $BUFRLIB

echo
echo "building test_IN_2"
$FC $FFLAGS -o test_IN_2.x test_IN_2.f $BUFRLIB

echo
echo "building test_OUT_1"
$FC $FFLAGS -o test_OUT_1.x test_OUT_1.f $BUFRLIB

echo
echo "building test_OUT_2"
$FC $FFLAGS -o test_OUT_2.x test_OUT_2.f $BUFRLIB

echo
echo "building test_IN_3"
$FC $FFLAGS -o test_IN_3.x test_IN_3.f $BUFRLIB

echo
echo "building test_IN_4"
$FC $FFLAGS -o test_IN_4.x test_IN_4.f $BUFRLIB

if [ ${bufrlib_build} = "dynamic" ]
then
    echo
    echo "building test_OUT_3"
    $FC $FFLAGS -o test_OUT_3.x test_OUT_3.f $BUFRLIB
fi

######################################################
#  run the test programs
#
#  for cray testing (using the PrgEnv-cray compiler and craype-haswell instruction
#  set), bsub will be used to run the test programs on the compute nodes, since
#  those are the only nodes which support craype-haswell
######################################################

cat > run_progs.sh << RUNPROGS
#BSUB -J BUFRLIB_test_harness	# job name
#BSUB -W 0:05                   # wall clock time of 5 minutes
#BSUB -P OBSPROC-T2O            # project code
#BSUB -R rusage[mem=100]        # memory requirement in MB
#BSUB -q "dev_shared"           # job queue
#BSUB -o run_progs.stdout       # location of stdout
#BSUB -e run_progs.stderr       # location of stderr

echo
echo " running test_IN_1"
./test_IN_1.x

echo
echo " running test_IN_2"
./test_IN_2.x

echo
echo " running test_OUT_1"
./test_OUT_1.x
./check_output.sh out1.bufr testfiles/OUT_1

echo
echo " running test_OUT_2"
cp testfiles/OUT_2_preAPX out2.bufr
./test_OUT_2.x
./check_output.sh out2.bufr testfiles/OUT_2

echo
echo " running test_IN_3"
./test_IN_3.x

echo
echo " running test_IN_4"
./test_IN_4.x

if [ ${bufrlib_build} = "dynamic" ]
then
    echo
    echo " running test_OUT_3"
    ./test_OUT_3.x
    ./check_output.sh out3.bufr testfiles/OUT_3
fi
RUNPROGS

chmod +x run_progs.sh
if [ $comp = "cray" ]
then
    echo "using bsub - see run_progs.stdout and run_progs.stderr for output"
    /bin/rm -f run_progs.stdout run_progs.stderr  # remove any old versions
    module load xt-lsfhpc
    bsub < run_progs.sh
else
    ./run_progs.sh
fi

######################################################
#  clean up
######################################################

echo
echo "cleaning up"
/bin/rm -f run_progs.sh
if [ $comp = "intel" ]
then
    /bin/rm -f test_*.x
#  we can't do this for cray, since when using bsub we have no way to be
#  sure that the test programs have finished running at this point
fi
