from __future__ import print_function
import ncepbufr

hdstr='SID XOB YOB DHR TYP ELV SAID T29'
obstr='POB QOB TOB ZOB UOB VOB PWO MXGS HOVI CAT PRSS TDO PMO'
qcstr='PQM QQM TQM ZQM WQM NUL PWQ PMQ'
oestr='POE QOE TOE NUL WOE NUL PWE     '

# read prepbufr file.

print('the following lines should be the same')
bufr = ncepbufr.open('data/prepbufr')
nmsg = 0
while bufr.advance() == 0:
    #print(nmsg, bufr.msg_counter, bufr.msg_type, bufr.msg_date)
    while bufr.load_subset() == 0:
        hdr = bufr.read_subset(hdstr).squeeze()
        station_id = hdr[0].tostring()
        obs = bufr.read_subset(obstr)
        nlevs = obs.shape[-1]
        oer = bufr.read_subset(oestr)
        qcf = bufr.read_subset(qcstr)
        if nmsg == 10:
            print('station_id, lon, lat, time, station_type, levels =',\
            station_id,hdr[1],hdr[2],hdr[3],int(hdr[4]),nlevs)
            bufr.checkpoint()
    if nmsg == 15: break
    nmsg += 1
bufr.restore()
bufr.load_subset()
hdr = bufr.read_subset(hdstr).squeeze()
station_id = hdr[0].tostring()
obs = bufr.read_subset(obstr)
nlevs = obs.shape[-1]
oer = bufr.read_subset(oestr)
qcf = bufr.read_subset(qcstr)
print('station_id, lon, lat, time, station_type, levels =',\
station_id,hdr[1],hdr[2],hdr[3],int(hdr[4]),nlevs)
bufr.close()
