from __future__ import print_function
import ncepbufr, sys, os
import numpy as np

def get_subset_info(bufr):
    hdstr1='RCYR RCMO RCDY RCHR RCMI'
    hdstr2='RSRD EXPRSRD'
    hdr1 = bufr.read_subset(hdstr1)
    hdr2 = (bufr.read_subset(hdstr2).squeeze()).filled(0)
    try:
        rsrd = int(hdr2[0]); exprsrd = int(hdr2[1])
    except:
        rsrd = 0; exprsrd = 0
    if rsrd > 0 and exprsrd == 0 : 
       no_restricted_data = False
    else:
       no_restricted_data = True
    receipt_time = 0
    for nlev in range(hdr1.shape[1]):
        nlevp1=nlev+1
        rtime = int('%04i%02i%02i%02i%02i' % (hdr1[0,nlev],hdr1[1,nlev],hdr1[2,nlev],hdr1[3,nlev],hdr1[4,nlev]))
        if rtime > receipt_time: receipt_time=rtime
    return receipt_time, rsrd, exprsrd

filename = sys.argv[1]
rtime1 = int(sys.argv[2])
rtime2 = int(sys.argv[3])
filenameo = sys.argv[4]

print('receipt time range = %s to %s' % (rtime1,rtime2))
if filenameo == filename:
    msg="output file must not have same name as input file"
    raise SystemExit(msg)

bufr = ncepbufr.open(filename)
nsubs=0; nsubso=0; nskip1=0; nskip2=0
print(filename)
bufrout = ncepbufr.open(filenameo,'w',bufr)
while bufr.advance() == 0: # loop over messages.
    # check individual subsets
    bufrout.open_message(bufr.msg_type,bufr.msg_date) # open message
    while bufr.load_subset() == 0: # loop over subsets in message.
        receipt_time, rsrd, exprsrd = get_subset_info(bufr)
        if receipt_time > rtime1 and receipt_time <= rtime2:
            #rs_bitstring =  "{0:b}".format(rsrd)
            # skip restricted data with no expiration time
            if rsrd == 0 or exprsrd > 0 : 
                # keep this bufr subset, write out to file.
                bufrout.copy_subset(bufr)
                nsubso+=1
            else:
                nskip1 += 1
                #print('skipping ob with restricted data flags %s' % rs_bitstring[0:5])
        else:
            nskip2+=1
        nsubs+=1
    bufrout.close_message()

if nsubso: 
    print('%s subsets (out of %s) written to %s' % (nsubso,nsubs,filenameo))
else:
    print('no output created')
    os.remove(filenameo)
if nskip1: print('%s obs skipped due to use restrictions' % nskip1)
if nskip2: print('%s subsets skipped due to receipt time missing or not in specified range' % nskip2)
bufr.close()
bufrout.close()
