from __future__ import print_function
import ncepbufr

hdrstr ='YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU'

# read gpsro file.

bufr = ncepbufr.open('data/gpsbufr')
bufr.print_table()
while bufr.advance() == 0:
    print(bufr.msg_counter, bufr.msg_type, bufr.msg_date)
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
            print('skip report due to bending angle/refractivity mismatch')
            continue
        print('sat id,platform transitter id, levels, yyyymmddhhmm =',\
        satid,ptid,levs_ref,yyyymmddhh)
        print('k, height, lat, lon, ref, bend:')
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
            print(k,rlat,rlon,height,ref,bend)
    # only loop over first 6 subsets
    if bufr.msg_counter == 6: break
bufr.close()
