import ncepbufr
import filecmp
import numpy as np

# Create a copy of OUT_12 using similar logic to outtest12.F90, but using Python instead of Fortran

# Open the BUFR table and output file
outfile = 'out12py.bufr'
bufr = ncepbufr.open(outfile,mode='n',table='OUT_12_bufrtab')
ncepbufr.standardize('Y')  # generate WMO-standard messages
ncepbufr.set_Section01_value('BEN',4)  # generate BUFR edition 4 messages

# Create list arrays, but note that Python array indices start at 0, whereas Fortran array indices start at 1
r8data = np.empty((9,1))
r8data2 = np.empty((7,1))
r8bitmap = np.empty((1,16))
r8pccf = np.empty((1,3))
r8mrkr = np.empty((1,3))

for jj in range(1,5):
    # Data subsets 1 and 2 will be uncompressed, while data subsets 3 and 4 will be compressed
    if jj == 3:
        bufr.cmpmsg('Y')

    # Open a new message for output.
    bufr.open_message('FN004017',2024072412)

    # Store the subset data.
    r8data[0,0] = 2024.
    r8data[1,0] = 7.
    r8data[2,0] = 24.
    r8data[3,0] = 12.
    r8data[4,0] = 55. + jj
    r8data[5,0] = 10.
    r8data[6,0] = -24.55750 + (jj * 0.001)
    r8data[7,0] = 86.23435 + (jj * 0.001)
    r8data[8,0] = 10500.
    bufr.write_subset(r8data,'YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH HMSL')
    r8data2[0,0] = 283.5 - jj
    r8data2[1,0] = 170.
    r8data2[2,0] = 290. - (jj * 5)
    r8data2[3,0] = 6.5 + (jj * 0.1)
    r8data2[4,0] = 3.
    r8data2[5,0] = 0.
    r8data2[6,0] = 10.
    bufr.write_subset(r8data2,'TMDBST SMMO WDIR WSPD POAF ROLQ FOST')

    # Store the bitmap.
    for ii in range(16):
        r8bitmap[0,ii] = 1.
    r8bitmap[0,10] = 0.
    r8bitmap[0,12] = 0.
    r8bitmap[0,13] = 0.
    bufr.write_subset(r8bitmap,'DPRI',rep=True)

    # Store the percent confidences.
    r8pccf[0,0] = 93. - jj
    r8pccf[0,1] = 94. - jj
    r8pccf[0,2] = 87. - jj
    bufr.write_subset(r8pccf,'PCCF',rep=True)

    # Store the marker operators containing standard deviations.
    r8mrkr[0,0] = 0.3 + (jj * 0.1)
    r8mrkr[0,1] = 1.0 * jj
    r8mrkr[0,2] = 0.1 + (jj * 0.3)
    bufr.write_subset(r8mrkr,'224255',rep=True)

    # Write a long character string to the message, and note that the use of first=True in the following
    # statement forces writsb to be called internally before actually writing the long character string.
    ptidc = 'HC888V3497074363'
    bufr.write_long_string(ptidc,'PTIDC',first=True)

    # Data subsets 1 and 2 will be in their own uncompressed BUFR message, while data subsets 3 and 4 will be
    # in a separate compressed BUFR message.
    if jj == 2:
        bufr.close_message()

# Close the output file.
bufr.close()

# Confirm the output file is correct.
if not filecmp.cmp(outfile,'OUT_12',shallow=False):
    raise Exception("Generated output did not match expected output!")

print("SUCCESS!")
