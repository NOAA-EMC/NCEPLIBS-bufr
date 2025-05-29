from __future__ import print_function
import ncepbufr
import sys

# print inventory of specified bufr file

bufr = ncepbufr.open(sys.argv[1])
nsubsets = 0
inv = bufr.inventory()
for n,msg in enumerate(inv):
    out = (n+1,)+msg
    print('message %s: %s %s %s %s subsets' % out)
    nsubsets += out[4]
bufr.close()
print('%s total subsets in %s messages' % (nsubsets,len(inv)))
