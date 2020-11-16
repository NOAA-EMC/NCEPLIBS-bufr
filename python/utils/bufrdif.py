"""bufrdif.py - read in two bufr files, write out a third containing data
that is unique in the 2nd file

version 0.1: Jeff Whitaker 20190227
"""
from __future__ import print_function
import ncepbufr, sys, os, tempfile, hashlib, argparse
from datetime import datetime, timedelta

# Parse command line args
ap = argparse.ArgumentParser()
ap.add_argument("input_bufr1", help="path to first input BUFR file")
ap.add_argument("input_bufr2", help="path to second input BUFR file")
ap.add_argument("output_bufr", help="output BUFR containing data in BUFR 2 that is not in BUFR 1")
ap.add_argument("--bufr_type", type=str, default='prep',help="bufr file type ('prep' or 'satwnd')")
ap.add_argument('--verbose', '-v', action='store_true')

MyArgs = ap.parse_args()

filename_in1 = MyArgs.input_bufr1
filename_in2 = MyArgs.input_bufr2
filename_out = MyArgs.output_bufr
bufr_type = MyArgs.bufr_type
verbose=MyArgs.verbose
print("""input_bufr1=%s 
input_bufr2=%s
output_bufr=%s
bufr_type=%s""" % (filename_in1,filename_in2,filename_out,bufr_type))

if filename_out == filename_in1 or filename_out == filename_in2:
    msg="output file must not have same name as input files"
    raise SystemExit(msg)

if bufr_type == 'prep':
    hdstr='SID XOB YOB DHR TYP ELV T29'
    if hdstr.split()[3] != 'DHR':
        raise ValueError("DHR mnemonic must be 4th in header list")
    obstr='POB QOB TOB UOB VOB PMO PRSS PWO'
    qcstr='PQM QQM TQM WQM PMQ PWQ'
elif bufr_type == 'satwnd':
    hdstr = 'SAID CLAT CLON CLATH CLONH YEAR MNTH DAYS HOUR MINU SWCM SAZA SCCF SWQM'
    obstr = 'EHAM HAMD PRLC WDIR WSPD'
    qcstr = 'OGCE GNAP PCCF'
elif bufr_type == 'goesfv':
    hdstr = 'CLON CLAT ELEV SOEL BEARAZ SOLAZI SAID DINU YEAR MNTH DAYS HOUR MINU SECO ACAV' 
    obstr = 'TMBR'
elif bufr_type == 'ascatw':
    hdstr = 'YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CTCN'
    obstr = 'WC10 WS10'
    qcstr = 'WVCQ'
else:
    msg="unrecognized bufr_type, must be one of 'prep','satwnd','goesfv','ascatw', got '%s'" % bufr_type
    raise SystemExit(msg)

def splitdate(yyyymmddhh):
    """
 yyyy,mm,dd,hh = splitdate(yyyymmddhh)

 give an date string (yyyymmddhh) return integers yyyy,mm,dd,hh.
    """
    yyyymmddhh = str(yyyymmddhh)
    yyyy = int(yyyymmddhh[0:4])
    mm = int(yyyymmddhh[4:6])
    dd = int(yyyymmddhh[6:8])
    hh = int(yyyymmddhh[8:10])
    return yyyy,mm,dd,hh

def get_bufr_dict(bufr,verbose=False,bufr_type='prep'):
    bufr_dict = {}
    ndup = 0
    delta = timedelta(seconds=1)
    while bufr.advance() == 0: 
        nsubset = 0
        if bufr_type == 'prep':
            yyyy,mm,dd,hh = splitdate(bufr.msg_date)
            refdate = datetime(yyyy,mm,dd,hh)
        while bufr.load_subset() == 0: # loop over subsets in message.
            hdr = bufr.read_subset(hdstr).squeeze()
            hdr = hdr.filled()
            if bufr_type == 'prep':
                secs = int(hdr[3]*3600.)
                obdate = refdate + secs*delta
                hdr[3]=float(obdate.strftime('%Y%m%d%H%M%S')) # YYYYMMDDHHMMSS
            hdrhash = hash(hdr.tostring())
            obshash = hash(bufr.read_subset(obstr).tostring())
            if bufr_type == 'goesfv':
                key = '%s %s %s' % (bufr.msg_type,hdrhash,obshash)
            else:
                qchash = hash(bufr.read_subset(qcstr).tostring())
                key = '%s %s %s %s' % (bufr.msg_type,hdrhash,obshash,qchash)
            # human readable header string
            #if bufr_type == 'prep':
            #    hdrstr = '%8s %8.3f %7.3f %14s %3i %5i %3i' % (hdr[0].tostring(),hdr[1],hdr[2],int(hdr[3]),int(hdr[4]),int(hdr[5]),int(hdr[6])))
            nsubset += 1
            if key in bufr_dict:
                ndup += 1
                if verbose: print('warning: duplicate key for msg type %s' % bufr.msg_type)
            else:
                bufr_dict[key] = bufr.msg_counter,nsubset
    return bufr_dict,ndup

# create dictionaries with md5 hashes for each message as keys, 
# (msg number,subset number) tuple as values.
bufr = ncepbufr.open(filename_in1)
bufr_dict1,ndup = get_bufr_dict(bufr,verbose=verbose,bufr_type=bufr_type)
print('%s duplicate keys found in %s' % (ndup,filename_in1))
bufr.close()

bufr = ncepbufr.open(filename_in2)
bufr_dict2,ndup = get_bufr_dict(bufr,verbose=verbose,bufr_type=bufr_type)
print('%s duplicate keys found in %s' % (ndup,filename_in2))

# find message subsets in bufr 2 that aren't in bufr 1
# uniq_messages is a dict whose keys are message numbers.
# dict entry is a list with unique subset numbers.
ncount = 0; uniq_messages = {}
for bkey in bufr_dict2:
    if bkey not in bufr_dict1:
        ncount += 1
        nmsg, nsubset = bufr_dict2[bkey]
        if nmsg in uniq_messages:
           uniq_messages[nmsg].append(nsubset)
        else:
           uniq_messages[nmsg] = [nsubset]
        if verbose: print('msg/subset %s/%s in %s not in %s' % (nmsg,nsubset,filename_in2,filename_in1))
print('%s unique message subsets (out of %s) in %s' % (ncount,len(bufr_dict2),filename_in2))

# write unique messages in bufr 2 to new file
# open output bufr file using same prepbufr table.
print('creating %s' % filename_out)
bufr.rewind()
bufrout = ncepbufr.open(filename_out,'w',bufr)
while bufr.advance() == 0: 
    if bufr.msg_counter in uniq_messages: # this message has a unique subset
        bufrout.open_message(bufr.msg_type,bufr.msg_date) # open output message
        nsubset = 0
        while bufr.load_subset() == 0: # loop over subsets in message.
            nsubset += 1 
            if nsubset in uniq_messages[bufr.msg_counter]: # write unique subsets
                bufrout.copy_subset(bufr)
        bufrout.close_message() # close message

# close up shop.
bufr.close(); bufrout.close()
