from __future__ import print_function
import ncepbufr
import numpy as np

# read prepbufr file.
hdstr='SID XOB YOB DHR TYP ELV SAID T29'
obstr='POB QOB TOB ZOB UOB VOB PWO MXGS HOVI CAT PRSS TDO PMO'
qcstr='PQM QQM TQM ZQM WQM NUL PWQ PMQ'
oestr='POE QOE TOE NUL WOE NUL PWE'
bufr = ncepbufr.open('data/prepbufr')
while bufr.advance() == 0: # loop over messages.
    while bufr.load_subset() == 0: # loop over subsets in message.
        hdr = bufr.read_subset(hdstr).squeeze()
        station_id = hdr[0].tostring()
        lon = hdr[1]; lat = hdr[2]
        station_type = int(hdr[4])
        obs = bufr.read_subset(obstr)
        nlevs = obs.shape[-1]
        oer = bufr.read_subset(oestr)
        qcf = bufr.read_subset(qcstr)
    # stop after first 2 messages.
    if bufr.msg_counter == 2: break
# check data
# station_id,lon,lat,time,station_type,nlevs
assert station_id.rstrip() == b'91925'
np.testing.assert_almost_equal(lon,220.97)
np.testing.assert_almost_equal(lat,-9.8)
assert station_type == 220
assert nlevs == 41
obs_str = '%s' % obs[:,-1]
oer_str = '%s' % oer[:,-1]
qc_str = '%s' % qcf[:,-1]
assert obs_str ==\
       '[19.0 -- -- -- -11.200000000000001 5.2 -- -- -- 3.0 -- -- --]'
assert oer_str == \
       '[-- -- -- -- 2.1 -- --]'
assert qc_str == \
       '[2.0 -- -- -- 2.0 -- -- --]'
bufr.close()

# read gpsro file
hdrstr ='YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU'
bufr = ncepbufr.open('data/gpsbufr')
nskip = 0
while bufr.advance() == 0:
    while bufr.load_subset() == 0:
        hdr = bufr.read_subset(hdrstr).squeeze()
        yyyymmddhh ='%04i%02i%02i%02i%02i' % tuple(hdr[0:5])
        satid = int(hdr[7])
        ptid = int(hdr[8])
        nreps_this_ROSEQ2 = bufr.read_subset('{ROSEQ2}').squeeze()
        nreps_this_ROSEQ1 = len(nreps_this_ROSEQ2)
        data1b = bufr.read_subset('ROSEQ1',seq=True) # bending angle
        data2a = bufr.read_subset('ROSEQ3',seq=True) # refractivity
        levs_bend = data1b.shape[1]
        levs_ref = data2a.shape[1]
        if levs_ref != levs_bend:
            #print('skip report due to bending angle/refractivity mismatch')
            nskip += 1
            continue
        for k in range(levs_ref):
            rlat = data1b[0,k]
            rlon = data1b[1,k]
            height = data2a[0,k]
            ref = data2a[1,k]
            for i in range(int(nreps_this_ROSEQ2[k])):
                m = 6*(i+1)-3
                freq = data1b[m,k]
                bend = data1b[m+2,k]
                # look for zero frequency bending angle ob
                if int(freq) == 0: break
    # only loop over first 6 subsets
    if bufr.msg_counter == 6: break
# check data
assert levs_ref == 247
assert nskip == 2
assert satid == 3
assert ptid == 7
assert yyyymmddhh == '201504072109'
np.testing.assert_almost_equal(rlon,-11.62675)
np.testing.assert_almost_equal(rlat,46.85915)
np.testing.assert_almost_equal(height,59844.0)
np.testing.assert_almost_equal(ref,0.068)
np.testing.assert_almost_equal(bend*1.e6,4.8)
bufr.close()

# read amsua radiance file.
hdstr1 ='SAID SIID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS'
hdstr2 ='SAZA SOZA BEARAZ SOLAZI'
bufr = ncepbufr.open('data/1bamua')
while bufr.advance() == 0:
    while bufr.load_subset() == 0:
        hdr1 = bufr.read_subset(hdstr1).squeeze()
        hdr2 = bufr.read_subset(hdstr2).squeeze()
        yyyymmddhhss ='%04i%02i%02i%02i%02i%02i' % tuple(hdr1[3:9])
        # for satellite id, see common code table c-5
        # (http://www.emc.ncep.noaa.gov/mmb/data_processing/common_tbl_c1-c5.htm#c-5)
        # for sensor id, see common code table c-8
        # (http://www.emc.ncep.noaa.gov/mmb/data_processing/common_tbl_c8-c14.htm#c-8)
        satid = int(hdr1[0])
        sensorid = int(hdr1[1])
        lat = hdr1[9]
        lon = hdr1[10]
        obs = bufr.read_subset('TMBR',rep=True).squeeze()
        nchanl = len(obs)
    # only loop over first 4 subsets
    if bufr.msg_counter == 4: break
assert satid == 3
assert nchanl == 15
assert sensorid == 570
assert yyyymmddhhss == '20131231210233'
assert bufr.msg_type == 'NC021023'
assert bufr.msg_date == 2013123121
np.testing.assert_almost_equal(lat,37.6066)
np.testing.assert_almost_equal(lon,-167.3253)
obs_tst=np.array([1.4555e+02,1.4618e+02,2.1374e+02,2.4871e+02,2.4807e+02,2.3607e+02,\
 2.2802e+02,2.2255e+02,2.1699e+02,2.1880e+02,2.2440e+02,2.2970e+02,\
 2.3407e+02,1.0000e+11,2.0008e+02],np.float)
np.testing.assert_array_almost_equal(obs,obs_tst)
bufr.close()

# read satellite wind file.
hdrstr = 'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SWCM SAZA GCLONG SCCF SWQM'
obstr = 'HAMD PRLC WDIR WSPD'
qcstr = 'OGCE GNAP PCCF'
bufr = ncepbufr.open('data/satwndbufr')
while bufr.advance() == 0:
    while bufr.load_subset() == 0:
        hdr = bufr.read_subset(hdrstr).squeeze()
        yyyymmddhh ='%04i%02i%02i%02i%02i' % tuple(hdr[3:8])
        satid = int(hdr[0])
        windtype = int(hdr[8])
        lat = hdr[1]; lon = hdr[2]
        qm = hdr[12]
        obdata = bufr.read_subset(obstr).squeeze()
    # only loop over first 4 subsets
    if bufr.msg_counter == 4: break
assert satid == 257
assert windtype == 1
assert bufr.msg_type == 'NC005010'
assert bufr.msg_date == 2014040822
assert yyyymmddhh == '201404082245'
np.testing.assert_almost_equal(lat,14.15)
np.testing.assert_almost_equal(lon,-32.7)
np.testing.assert_almost_equal(obdata[1],77500.0) # pressure
np.testing.assert_almost_equal(qm,2.0) # quality mark
np.testing.assert_almost_equal(obdata[3],10.0) # speed
np.testing.assert_almost_equal(obdata[2],62.0) # direction
bufr.close()

# test checkpointing.
hdstr='SID XOB YOB DHR TYP ELV SAID T29'
obstr='POB QOB TOB ZOB UOB VOB PWO MXGS HOVI CAT PRSS TDO PMO'
qcstr='PQM QQM TQM ZQM WQM NUL PWQ PMQ'
oestr='POE QOE TOE NUL WOE NUL PWE     '
bufr = ncepbufr.open('data/prepbufr')
nmsg = 0
while bufr.advance() == 0:
    while bufr.load_subset() == 0:
        hdr = bufr.read_subset(hdstr).squeeze()
        station_id = hdr[0].tostring()
        obs = bufr.read_subset(obstr)
        nlevs = obs.shape[-1]
        oer = bufr.read_subset(oestr)
        qcf = bufr.read_subset(qcstr)
        if nmsg == 10:
            obs_save = obs.copy(); oer_save = oer.copy(); qcf_save = qcf.copy()
            str1 =\
            'station_id=%s, lon=%7.2f, lat=%7.2f, time=%6.3f, station_type=%s, levels=%s' %\
            (station_id.rstrip(),hdr[1],hdr[2],hdr[3],int(hdr[4]),nlevs)
            bufr.checkpoint()
    if nmsg == 15: break
    nmsg += 1
bufr.restore()
bufr.load_subset()
hdr = bufr.read_subset(hdstr).squeeze()
station_id = hdr[0].tostring()
obs2 = bufr.read_subset(obstr)
nlevs = obs2.shape[-1]
oer2 = bufr.read_subset(oestr)
qcf2 = bufr.read_subset(qcstr)
str2 =\
'station_id=%s, lon=%7.2f, lat=%7.2f, time=%6.3f, station_type=%s, levels=%s' %\
(station_id.rstrip(),hdr[1],hdr[2],hdr[3],int(hdr[4]),nlevs)
assert str1 == str2
np.testing.assert_array_almost_equal(obs_save.filled(), obs2.filled())
np.testing.assert_array_almost_equal(oer_save.filled(), oer2.filled())
np.testing.assert_array_almost_equal(qcf_save.filled(), qcf2.filled())
bufr.close()

# test reading long strings
bufr = ncepbufr.open('data/xx103')
test_station_names = ['BOUEE_LION', 'BOUEE_ANTILLES',
                      'BOUEE_COTE D\'AZUR',
                      'GULF OF MAINE', 'TENERIFE']
test_report_ids = ['6100002', '4100300', '6100001', '4400005', '1300131']
i_msg = 0
while bufr.advance() == 0:
    # Just read the first subset from each message.
    if bufr.load_subset() == 0:
        stsn = bufr.read_long_string(mnemonic='STSN')
        rpid = bufr.read_long_string(mnemonic='RPID')
        assert stsn == test_station_names[i_msg]
        assert rpid == test_report_ids[i_msg]
        i_msg = i_msg + 1
    # only loop over first 5 subsets
    if i_msg == 5: break
bufr.close()
    
    
