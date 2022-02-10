import _bufrlib
import random
import bisect
import numpy as np
from .bufr_mnemonics import *
import os

__version__ = "1.1.1"
__bufrlib_version__ = _bufrlib.bvers().rstrip()

# create list of allowed fortran unit numbers
_funits = list(range(1,100))
# remove unit numbers used for stdin and stdout
_funits.remove(5)
_funits.remove(6)
_maxdim = 5000 # max number of data levels in message
_maxevents = 255 # max number of prepbufr events in message
_nmaxseq = _maxevents # max size of sequence in message

def set_param(key, value):
    """
    set BUFRLIB internal parameters controlling size limits.
    Must be done prior to opening a bufr file.  Valid parameters are:

    'MXMSGL' = MAXIMUM LENGTH (IN BYTES) OF A BUFR\
               MESSAGE

    'MAXSS'  = MAXIMUM NUMBER OF DATA VALUES IN AN
               UNCOMPRESSED BUFR SUBSET

    'MXCDV'  = MAXIMUM NUMBER OF DATA VALUES THAT CAN BE
               WRITTEN INTO A COMPRESSED BUFR SUBSET

    'MXLCC'  = MAXIMUM LENGTH (IN BYTES) OF A CHARACTER
               STRING THAT CAN BE WRITTEN INTO A
               COMPRESSED BUFR SUBSET

    'MXCSB'  = MAXIMUM NUMBER OF SUBSETS THAT CAN BE
               WRITTEN INTO A COMPRESSED BUFR MESSAGE

    'NFILES' = MAXIMUM NUMBER OF BUFR FILES THAT CAN BE
               ACCESSED FOR READING OR WRITING AT ANY
               ONE TIME

    'MAXTBA' = MAXIMUM NUMBER OF ENTRIES IN INTERNAL BUFR
               TABLE A PER BUFR FILE

    'MAXTBB' = MAXIMUM NUMBER OF ENTRIES IN INTERNAL BUFR
               TABLE B PER BUFR FILE

    'MAXTBD' = MAXIMUM NUMBER OF ENTRIES IN INTERNAL BUFR
               TABLE D PER BUFR FILE

    'MAXMEM' = MAXIMUM NUMBER OF BYTES THAT CAN BE USED
               TO STORE BUFR MESSAGES IN INTERNAL MEMORY

    'MAXMSG' = MAXIMUM NUMBER OF BUFR MESSAGES THAT CAN
               BE STORED IN INTERNAL MEMORY

    'MXDXTS' = MAXIMUM NUMBER OF DICTIONARY TABLES THAT
               CAN BE STORED FOR USE WITH BUFR MESSAGES
               IN INTERNAL MEMORY

    'MXMTBB' = MAXIMUM NUMBER OF MASTER TABLE B ENTRIES

    'MXMTBD' = MAXIMUM NUMBER OF MASTER TABLE D ENTRIES

    'MXMTBF' = MAXIMUM NUMBER OF MASTER CODE/FLAG ENTRIES

    'MAXCD'  = MAXIMUM NUMBER OF CHILD DESCRIPTORS IN A
               TABLE D DESCRIPTOR SEQUENCE DEFINITION

    'MAXJL'  = MAXIMUM NUMBER OF ENTRIES IN THE INTERNAL
               JUMP/LINK TABLE

    'MXS01V' = MAXIMUM NUMBER OF DEFAULT SECTION 0 OR
               SECTION 1 VALUES THAT CAN BE OVERWRITTEN
               WITHIN AN OUTPUT BUFR MESSAGE

    'MXBTM'  = MAXIMUM NUMBER OF BITMAPS THAT CAN BE
               STORED INTERNALLY FOR A BUFR SUBSET

   'MXBTMSE' = MAXIMUM NUMBER OF ENTRIES THAT CAN BE
               SET WITHIN A BITMAP

    'MXTAMC' = MAXIMUM NUMBER OF TABLE A MNEMONICS IN THE
               INTERNAL JUMP/LINK TABLE WHICH CONTAIN AT
               LEAST ONE TABLE C OPERATOR WITH X>=21 IN
               THEIR SUBSET DEFINITION

    'MXTCO'  = MAXIMUM NUMBER OF TABLE C OPERATORS (WITH
               X>=21) IN THE SUBSET DEFINITION OF A
               TABLE A MNEMONIC

    'MXNRV'  = MAXIMUM NUMBER OF 2-03 REFERENCE VALUES
               IN THE INTERNAL JUMP/LINK TABLE

    The 'get_param' function can be used to obtain the current value of these
    parameters."""
    _bufrlib.isetprm(key, value)

def get_param(key):
    """
    get the values BUFRLIB internal parameters controlling size limits.
    see 'set_param' docstring for allowable parameter names."""
    return _bufrlib.igetprm(key)

def set_missing_value(missing_value):
    """
    set bufr missing value.
    """
    _bufrlib.setbmiss(missing_value)

def get_missing_value():
    """
    get bufr missing value.
    """
    return _bufrlib.getbmiss()

def set_datelength(charlen):
    """
    set number of digits for date specification (10 gives `YYYYMMDDHH`)
    """
    _bufrlib.datelen(charlen)

set_datelength(10) # set default date length to 10 (YYYYMDDHH)

class open:
    """
    bufr file object.

    `ncepbufr.open.__init__` used to construct instance.

    `ncepbufr.open.advance` method can be used step through bufr messages.
    """
    def __init__(self,filename,mode='r',table=None):
        """
        bufr object constructor

        `filename`: bufr file name.

        `mode`: `'r'` for read, `'w'` for write, `'a'` for append (default
        `'r'`).

        `table`:  bufr table filename or ncepbufr.open instance.
        Must be specified for `mode='w'`, optional for `mode='r'`.
        If table is an existing ncepbufr.open instance, the table
        will be shared. If not, it is assumed to be the filename of a bufr table.
        For `mode='r'`, bufr table embedded in file will be used if not specified.
        """
        # randomly choose available fortran unit number
        self.lunit = random.choice(_funits)
        self.filename = filename
        '''bufr file opened with this fortran unit number'''
        _funits.remove(self.lunit)
        if not _funits:
            raise IOError("too many files open")
        if mode == 'r':
            self._ioflag = 'IN'
        elif mode == 'w':
            if table is None:
                msg="must specify file containing bufr table when mode='w'"
                raise ValueError(msg)
            self._ioflag = 'OUT'
        elif mode == 'a':
            self._ioflag = 'APN'
        else:
            raise ValueError("mode must be 'r', 'w' or 'a'")
        if mode == 'r' or mode == 'a':
            if not os.path.isfile(filename):
                msg='%s does not exist' % filename
                raise IOError(msg)
            iret = _bufrlib.fortran_open(filename,self.lunit,"unformatted","rewind")
            if iret != 0:
                msg='error opening %s' % filename
                raise IOError(msg)
            if table is None:
                # table embedded in bufr file
                _bufrlib.openbf(self.lunit,self._ioflag,self.lunit)
                self.lundx = self.lunit # table unit number same as bufr unit number
            else:
                try:
                    # share a bufr table with another instance
                    self.lundx = table.lunit
                except AttributeError:
                    # external table file specified
                    self.lundx = random.choice(_funits)
                    iret = _bufrlib.fortran_open(table,self.lundx,"formatted","rewind")
                    if iret != 0:
                        msg='error opening %s' % filename
                        raise IOError(msg)
                    _funits.remove(self.lundx)
                _bufrlib.openbf(self.lunit,self._ioflag,self.lundx)
        elif mode == 'w':
            try:
                # share a bufr table with another instance
                self.lundx = table.lunit
            except AttributeError:
                # read bufr table from a file.
                self.lundx = random.choice(_funits)
                iret = _bufrlib.fortran_open(table,self.lundx,"formatted","rewind")
                if iret != 0:
                    msg='error opening %s' % table
                    raise IOError(msg)
                _funits.remove(self.lundx)
            iret = _bufrlib.fortran_open(filename,self.lunit,"unformatted","rewind")
            if iret != 0:
                msg='error opening %s' % filename
                raise IOError(msg)
            _bufrlib.openbf(self.lunit,self._ioflag,self.lundx)
        # initialized message number counter
        self.msg_counter = 0
        '''current bufr message number'''
        self.msg_type = None
        '''current bufr message type'''
        self.msg_date = None
        '''reference date for bufr message'''
        self.receipt_time = None
        '''tank receipt time for bufr message (`YYYYMMDDHHMM`), -1 if missing'''
        self.subsets = None
        '''number of subsets in the bufr message'''
        # missing value in decoded data.
        # (if equal to self.missing_value, data is masked)
        self.missing_value = get_missing_value()
        '''bufr missing value'''
    def _receipt_time(self):
        """
        return 'tank' receipt time (`YYYYMMDDHHMM`).

        returns -1 if there is no tank receipt time for this message.
        """
        iyr,imon,iday,ihr,imin,iret = _bufrlib.rtrcpt(self.lunit)
        if iret == 0:
            return int('%04i%02i%02i%02i%02i' % (iyr,imon,iday,ihr,imin))
        else:
            return iret
    def _subsets(self):
        """
        return the number of subsets in this bufr message
        """
        return _bufrlib.nmsub(self.lunit)
    def dump_table(self,filename):
        """
        dump embedded bufr table to a file
        """
        lundx = random.choice(_funits)
        iret = _bufrlib.fortran_open(filename,lundx,'formatted','rewind')
        if iret != 0:
            msg='error opening %s' % filename
        _bufrlib.dxdump(self.lunit,lundx)
        iret = _bufrlib.fortran_close(lundx)
        if iret == 0:
            bisect.insort_left(_funits,lundx)
        else:
            raise IOError('error closing %s' % filename)
    def print_table(self):
        """
        print embedded bufr table to stdout
        """
        _bufrlib.dxdump(self.lunit,6)
    def close(self):
        """
        close the bufr file
        """
        _bufrlib.closbf(self.lunit)
        # add fortran unit number back to pool
        bisect.insort_left(_funits,self.lunit)
        if self.lundx != self.lunit:
            iret = _bufrlib.fortran_close(self.lundx)
            if iret == 0:
                bisect.insort_left(_funits,self.lundx)
            else:
                raise IOError('error closing bufr table')
    def advance(self):
        """
        advance to the next msg in the bufr file
        returns 0 if advance was sucessful,
        1 if not (presumably because the end
        of the file was reached).

        The following attributes are set each time
        file is advanced to the next message:

        `msg_type`: string describing type of message.

        `msg_date`: reference date (YYYYMMDDHH) for message.

        `msg_counter`: message number.

        `receipt_time`: bufr tank receipt time.

        `subsets`: number of subsets in the message.

        `subset_loaded`: Boolean indicating whether a subset has been
        loaded with `ncepbufr.open.load_subset`.  Initialized to `False`.

        To loop through all the bufr messages in a file:

            :::python
            >>> bufr = ncepbufr.open(filename)
            >>> while bufr.advance() == 0:
            >>>     # processing code for each message here

        """
        subset, idate, iret = _bufrlib.readmg(self.lunit)
        if iret:
            return iret
        else:
            self.msg_type = subset.decode('ascii').rstrip()
            self.msg_date = idate
            self.msg_counter += 1
            self.subset_loaded = False
            self.receipt_time = self._receipt_time()
            self.subsets = self._subsets()
            return 0
    def inventory(self):
        """
        return a list containing an inventory of the bufr file.
        The list contains a tuple for each message.
        containing (msg_type,msg_date,receipt_time,subsets).
        """
        self.checkpoint()
        inv = []
        while self.advance() == 0:
            inv.append((self.msg_type,self.msg_date,self.receipt_time,self.subsets))
        self.restore()
        return inv

    def print_subset(self,verbose=False):
        """
        print a textual representation of the decoded
        data in the currently loaded subset.

        If `verbose=True`, more complete but harder to read info is written.

        `ncepbufr.open.load_subset` must be called before
        trying to print the decoded subset using `ncepbufr.open.print_subset`.
        """
        if not verbose:
            _bufrlib.ufdump(self.lunit,6)
        else:
            _bufrlib.ufbdmp(self.lunit,6)
    def copy_subset(self,bufrin):
        """
        copy the currently loaded subset from the specified bufr file object
        and write to the current bufr message"""
        _bufrlib.ufbcpy(bufrin.lunit, self.lunit)
        _bufrlib.writsb(self.lunit)
    def dump_subset(self,filename,append=False,verbose=False):
        """
        dump a textual representation of the decoded
        data in the currently loaded subset to a file.

        If `append=True`, append to an existing file
        (otherwise over-write file).

        If `verbose=True`, more complete but harder to read info is written.

        `ncepbufr.open.load_subset` must be called before
        trying to print the decoded subset using `ncepbufr.open.dump_subset`.
        """
        lunout = random.choice(_funits)
        if not append:
            iret = _bufrlib.fortran_open(filename,lunout,'formatted','rewind')
        else:
            iret = _bufrlib.fortran_open(filename,lunout,'formatted','append')
        if iret != 0:
            msg='error opening %s' % filename
        if not verbose:
            _bufrlib.ufdump(self.lunit,lunout)
        else:
            _bufrlib.ufbdmp(self.lunit,lunout)
        iret = _bufrlib.fortran_close(lunout)
        if iret == 0:
            bisect.insort_left(_funits,lunout)
        else:
            raise IOError('error closing %s' % filename)
    def get_program_code(self,mnemonic):
        """
        return prepbufr event program code
        associated with specified mnemonic
        (see `src/ufbqcd.f` for more details)
        """
        return _bufrlib.ufbqcd(self.lunit, mnemonic)
    def get_flag_table_bits(self, mnemonic, val):
        """
        return bit settings associated with
        a specifed value and flag table mnemonic
        (see src/upftbv.f90 for more details)
        """
        ibits, nbits = _bufrlib.upftbv_body(self.lunit, mnemonic, float(val), _maxevents)
        return ibits[:nbits]
    def checkpoint(self):
        """
        mark where we are in the bufr file,
        and rewind the file.
        The `ncepbufr.open.restore` method can then be
        used to go back to this state.
        """
        _bufrlib.rewnbf(self.lunit,0)
        self.msg_counter = 0
        self.msg_type = None
        self.msg_date = None
        self.receipt_time = None
        self.subsets = None
    def rewind(self):
        """
        rewind the bufr file (same as `ncepbufr.open.checkpoint`).
        """
        self.checkpoint()
    def restore(self):
        """
        restore the state of the bufr
        file that recorded by a previous call
        to `ncepbufr.open.checkpoint`.
        """
        _bufrlib.rewnbf(self.lunit,1)
    def open_message(self,msg_type,msg_date,msg_receipt_time=None):
        """
        open new bufr message.

        Mandatory arguments:

        `msg_type`: string describing type of message.

        `msg_date`: reference date (e.g. `YYYYMMDDHH`) for message. The
        number of digits in the reference date is controlled by
        module function `set_datelength`, and is 10 by default.

        `msg_receipt_time` bufr tank receipt time YYYYMMDDHHMM (optional).
        """
        if msg_receipt_time is not None:
            yyyymmddhhmm = str(msg_receipt_time)
            try:
                yyyy = int(yyyymmddhhmm[0:4])
                mm = int(yyyymmddhhmm[4:6])
                dd = int(yyyymmddhhmm[6:8])
                hh = int(yyyymmddhhmm[8:10])
                mm = int(yyyymmddhhmm[10:12])
                _bufrlib.strcpt('Y',yyyy,mm,dd,hh,mm)
            except IndexError:
                pass # don't write receipt time
        _bufrlib.openmb(self.lunit,msg_type,int(msg_date))
    def copy_message(self,bufrin):
        """
        copy the currently loaded message from the specified bufr file object
        and write to the file"""
        _bufrlib.copymg(bufrin.lunit, self.lunit)
    def close_message(self):
        """
        close bufr message
        """
        _bufrlib.closmg(self.lunit)
    def load_subset(self):
        """
        load subset data from the current message
        (must be called before `ncepbufr.open.read_subset`).
        To loop through all messages in a file, and
        all subsets in each message:

            :::python
            >>> bufr = ncepbufr.open(filename)
            >>> while bufr.advance() == 0:
            >>>     while bufr.load_subset() == 0:
            >>>         # processing code for each subset here

        """
        iret = _bufrlib.ireadsb(self.lunit)
        if iret == 0:
            self.subset_loaded = True
        return iret
    def read_subset(self,mnemonics,rep=False,seq=False,events=False):
        """
        decode the data from the currently loaded message subset
        using the specified mnemonics (a 'mnemonic' is simply a
        descriptive, alphanumeric name for a data value, like
        a key in a python dictionary). The mnemonics string
        may contain multiple space delimited mnemonics
        (e.g. `mnemonics='MNEMONIC1 MNEMONIC2 MNEMONIC3'`).

        By default, the mnemonics are assumed to be part of a delayed
        replication sequence, or have no replication at all, and `ufbint_body`
        is used to read the data.

        `ncepbufr.open.load_subset` must be called before
        trying to decode a subset using `ncepbufr.open.read_subset`.

        if `rep = True`, `ufbrep_body` is used to read data represented
        a regular replication sequence.  See the comments in `src/ufbrep.f90` for
        more details. Used for radiance data.

        if `seq=True`, `ufbseq` is used to read data represented by
        a sequence mnemonic. Used for gps data.

        if `events=True`, `ufbevn` is used to read prepbufr
        "events", and a 3-d array is returned.

        Only one of seq, rep and events can be True.

        returns a numpy masked array with decoded values
        (missing values are masked).
        The shape of the array is `(nm,nlevs)`, where
        where `nm` is the number of elements in the specified
        mnemonics string, and `nlevs` is the number of levels in the report.
        If `events=True`, a 3rd dimension representing the prepbufr
        event codes is added.
        """
        if not self.subset_loaded:
            raise IOError('subset not loaded, call load_subset first')
        ndim = len(mnemonics.split())
        if np.array([rep,seq,events]).sum() > 1:
            raise ValueError('only one of rep, seq and events cannot be True')
        if seq:
            data = np.empty((_nmaxseq,_maxdim),np.float,order='F')
            levs = _bufrlib.ufbseq(self.lunit,data,mnemonics,_nmaxseq,_maxdim)
        elif rep:
            data = np.empty((ndim,_maxdim),np.float,order='F')
            levs = _bufrlib.ufbrep_body(self.lunit,data,mnemonics,ndim,_maxdim)
        elif events:
            #data = np.empty((ndim,_maxdim,maxevents),np.float,order='F')
            data = np.empty((ndim,_maxdim,_maxevents),np.float,order='F')
            levs = _bufrlib.ufbevn(self.lunit,data,mnemonics,ndim,_maxdim,_maxevents)
        else:
            data = np.empty((ndim,_maxdim),np.float,order='F')
            levs = _bufrlib.ufbint_body(self.lunit,data,mnemonics,ndim,_maxdim)
        if events:
            return np.ma.masked_values(data[:,:levs,:],self.missing_value)
        else:
            return np.ma.masked_values(data[:,:levs],self.missing_value)
    def write_subset(self,data,mnemonics,rep=False,seq=False,events=False,end=False):
        """
        write data to message subset using the specified mnemonics
        (a 'mnemonic' is simply a descriptive, alphanumeric name for a
        data value, like a key in a python dictionary). The mnemonics string
        may contain multiple space delimited mnemonics
        (e.g. `mnemonics='MNEMONIC1 MNEMONIC2 MNEMONIC3'`).

        By default, the mnemonics are assumed to be part of a delayed
        replication sequence, or have no replication at all, and `ufbint_body`
        is used to write the data.

        if `rep = True`, `ufbrep_body` is used to write data represented
        a regular replication sequence.  See the comments in `src/ufbrep.f90` for
        more details. Used for radiance data.

        if `seq=True`, `ufbseq` is used to write data represented by
        a sequence mnemonic. Used for gps data.

        if `events=True`, `ufbevn` is used to write prepbufr
        "events" (a 3-d data array is required)

        Only one of seq, rep and events can be True.

        If `end=True`, the message subset is closed and written
        to the bufr file (default `False`).
        """
        # make a fortran contiguous copy of input data.
        if len(data.shape) in [2,3]:
            dataf = np.empty(data.shape, np.float, order='F')
            dataf[:] = data[:]
        elif len(data.shape) == 1:
            # make 1d array into 2d array with 1 level
            dataf = np.empty((data.shape[0],1), np.float, order='F')
            dataf[:,0] = data[:]
        else:
            msg = 'data in write_subset must be 1,2 or 3d'
            raise ValueError(msg)
        if np.array([rep,seq,events]).sum() > 1:
            raise ValueError('only one of rep, seq and events cannot be True')
        if seq:
            levs = _bufrlib.ufbseq(self.lunit,dataf,mnemonics,dataf.shape[0],\
                    dataf.shape[1])
        elif rep:
            levs = _bufrlib.ufbrep_body(self.lunit,dataf,mnemonics,dataf.shape[0],\
                    dataf.shape[1])
        elif events:
            levs = _bufrlib.ufbevn(self.lunit,dataf,mnemonics,dataf.shape[0],\
                    dataf.shape[1],dataf.shape[2])
        else:
            levs = _bufrlib.ufbint_body(self.lunit,dataf,mnemonics,dataf.shape[0],\
                    dataf.shape[1])
        # end subset if desired.
        if end:
            _bufrlib.writsb(self.lunit)
