import ncepbufr
import filecmp

# Create a copy of OUT_13 using similar logic to outtest13.F90, but using Python instead of Fortran

# Open the BUFR output file
outfile = 'out13py.bufr'
bufro = ncepbufr.open(outfile,mode='w',table='OUT_13_bufrtab')
bufro.cmpmsg('Y')  # generate compressed BUFR messages
ncepbufr.set_Section01_value('BEN',4)  # generate BUFR edition 4 messages
ncepbufr.set_outputmessage_maxlen(199900)  # increase limit of output message size
ncepbufr.set_mastertables_dir('../../tables')

# Open the BUFR input file
bufri = ncepbufr.open('OUT_13_infile',mode='s')

# Read the message from the input file
assert bufri.advance() == 0

# Read each subset from the input message and write it to the output file
nchn = [500]
ndrpcs = [90,120,90]
while bufri.load_subset() == 0:
    r8vals = bufri.read_subset('IASIL1CT',seq=True)
    bufro.open_message('NC021039',2024100102)
    bufro.initialize_drfs(nchn,'(IASICHN)')
    bufro.initialize_drfs(ndrpcs,'(IASIPCS)')
    bufro.write_subset(r8vals,'NC021039',seq=True,end=True)

# Close the output file.
bufro.close()

# Confirm the output file is correct.
if not filecmp.cmp(outfile,'OUT_13',shallow=False):
    raise Exception("Generated output did not match expected output!")

print("SUCCESS!")
