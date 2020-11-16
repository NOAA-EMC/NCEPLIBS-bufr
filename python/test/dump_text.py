from __future__ import print_function
import ncepbufr
import sys

# dump contents of bufr file to stdout or to a text file.
# Warning: resulting output may be HUGE.

bufr = ncepbufr.open(sys.argv[1])
first_dump = True # after first write, append to existing file.
verbose = False # this produces more readable output.
while bufr.advance() == 0: # loop over messages.
    while bufr.load_subset() == 0: # loop over subsets in message.
        if len(sys.argv) > 2:
            # dump decoded data to a file.
            if first_dump:
                # clobber file if it exists
                bufr.dump_subset(sys.argv[2],verbose=verbose)
                first_dump = False
            else:
                # append to existing file.
                bufr.dump_subset(sys.argv[2],append=True,verbose=verbose)
        else:
            bufr.print_subset(verbose=verbose) # print decoded subset to stdout
bufr.close()
