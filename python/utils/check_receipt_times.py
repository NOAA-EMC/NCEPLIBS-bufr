from __future__ import print_function
import ncepbufr, sys
import numpy as np
hdstr='RCYR RCMO RCDY RCHR RCMI'
bufr = ncepbufr.open(sys.argv[1])
print(sys.argv[1])
# check by subset.
nsubs = 0
receipt_times = []
while bufr.advance() == 0: # loop over messages.
    #print(bufr.msg_counter, bufr.msg_type, bufr.msg_date, bufr.receipt_time)
    while bufr.load_subset() == 0: # loop over subsets in message.
        hdr1 = bufr.read_subset(hdstr)
        receipt_time_subset = 0
        for nlev in range(hdr1.shape[1]):
           nlevp1=nlev+1
           rtime = int('%04i%02i%02i%02i%02i' % (hdr1[0,nlev],hdr1[1,nlev],hdr1[2,nlev],hdr1[3,nlev],hdr1[4,nlev]))
           if rtime > receipt_time_subset: receipt_time_subset=rtime
        receipt_times.append(receipt_time_subset)
        nsubs += 1
receipt_times=np.array(receipt_times)
indx = receipt_times > 0
if indx.any():
    print('%s out of %s subsets have receipt times, min %s max %s' % (indx.sum(), len(receipt_times), receipt_times[indx].min(), receipt_times[indx].max()))
else: # check by message
    bufr.close()
    bufr = ncepbufr.open(sys.argv[1])
    receipt_times = []
    while bufr.advance() == 0: # loop over messages.
        #print(bufr.msg_counter, bufr.msg_type, bufr.msg_date, bufr.receipt_time)
        receipt_times.append(int(bufr.receipt_time))
    receipt_times = np.array(receipt_times)
    indx = receipt_times > 0
    if indx.any():
        print('%s out of %s messages have receipt times, min %s max %s' % (indx.sum(), len(receipt_times), receipt_times[indx].min(), receipt_times[indx].max()))
    else:
        print('WARNING:  No receipt times in this bufr file!')
        raise IOError
bufr.close()
