from __future__ import print_function
import ncepbufr, sys, os
import numpy as np

hdstr='RCYR RCMO RCDY RCHR RCMI'
hdstr2='RSRD EXPRSRD'

filename = sys.argv[1]
rtime1 = int(sys.argv[2])
rtime2 = int(sys.argv[3])
filenameo = sys.argv[4]

print('receipt time range = %s to %s' % (rtime1,rtime2))
if filenameo == filename:
    msg="output file must not have same name as input file"
    raise SystemExit(msg)

bufr = ncepbufr.open(filename)
bufrout = ncepbufr.open(filenameo,'w',bufr)
nskip=0; nmsg=0; nmsgo = 0
while bufr.advance() == 0: # loop over messages.
    msg_receipt_time = bufr.receipt_time
    # write entire message at once, since subsets all have same receipt time.
    if msg_receipt_time > rtime1 and msg_receipt_time <= rtime2:
       bufrout.copy_message(bufr)
       nmsgo += 1
    else:
       nskip += 1
    nmsg+=1

if nmsgo: 
    print('%s messages (out of %s) written to %s' % (nmsgo,nmsg,filenameo))
else:
    print('no output created')
    os.remove(filenameo)
if nskip: print('%s messages skipped due to receipt time missing or not in specified range' % nskip)
bufr.close()
bufrout.close()
