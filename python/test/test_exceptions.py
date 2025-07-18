import ncepbufr

# Test exception generation by trying to create a message with a non-existent mnemonic
# Open the BUFR table and output file
outfile = 'outpyexceptions.bufr'
bufr = ncepbufr.open(outfile,mode='n',table='OUT_12_bufrtab')
try:
    bufr.open_message('nonexistent',2025080218)
    print('Exception was not generated, this should not happen')
except Exception as e:
    print('Successfully caught exception:', e)
