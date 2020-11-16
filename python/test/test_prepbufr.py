from __future__ import print_function
import ncepbufr

hdstr='SID XOB YOB DHR TYP ELV SAID T29'
obstr='POB QOB TOB ZOB UOB VOB PWO MXGS HOVI CAT PRSS TDO PMO'
qcstr='PQM QQM TQM ZQM WQM NUL PWQ PMQ'
oestr='POE QOE TOE NUL WOE NUL PWE'

# read prepbufr file.

bufr = ncepbufr.open('data/prepbufr')
bufr.print_table() # print embedded table
bufr.dump_table('prepbufr.table') # dump table to file
while bufr.advance() == 0: # loop over messages.
    print(bufr.msg_counter, bufr.msg_type, bufr.msg_date, bufr.receipt_time)
    #bufr.read_subset(obstr) # should raise subset not loaded error
    while bufr.load_subset() == 0: # loop over subsets in message.
        hdr = bufr.read_subset(hdstr).squeeze()
        station_id = hdr[0].tostring()
        obs = bufr.read_subset(obstr)
        nlevs = obs.shape[-1]
        oer = bufr.read_subset(oestr)
        qcf = bufr.read_subset(qcstr)
        print('station_id, lon, lat, time, station_type, levels =',\
        station_id,hdr[1],hdr[2],hdr[3],int(hdr[4]),nlevs)
        for k in range(nlevs):
            if nlevs > 1:
                print('level',k+1)
            print('obs',obs[:,k])
            print('oer',oer[:,k])
            print('qcf',qcf[:,k])
    # stop after first 2 messages.
    if bufr.msg_counter == 2: break
bufr.close()
