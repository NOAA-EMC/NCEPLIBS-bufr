C> @file
C> @brief Initialize global variables and arrays within internal memory.

C> This subroutine initializes numerous global variables and arrays
C> within internal modules and COMMON blocks throughout the BUFRLIB
C> software.
C>
C> <p>This subroutine isn't normally called directly by any
C> application program, because it's automatically called internally
C> from within subroutine openbf() during the first time that subroutine
C> is called by any application program.
C>
C> @authors J. Woollen
C> @authors J. Ator
C> @date 1994-01-06
C>
C> <b>Program history log:</b>
C> - 1994-01-06  J. Woollen -- Original author
C> - 1995-06-28  J. Woollen -- Increased the size of internal BUFR table
C>                           arrays in order to handle bigger files
C> - 1998-07-08  J. Woollen -- Modified to make Y2K-compliant
C> - 1999-11-18  J. Woollen -- The number of BUFR files which can be
C>                             opened at one time increased from 10 to 32
C>                             (necessary in order to process multiple
C>                             BUFR files under the MPI)
C> - 2000-09-19  J. Woollen -- Maximum message length increased from
C>                           10,000 to 20,000 bytes
C> - 2003-11-04  J. Ator    -- Added documentation
C> - 2003-11-04  S. Bender  -- Added remarks and routine interdependencies
C> - 2003-11-04  D. Keyser  -- MAXJL (maximum number of jump/link entries)
C>                           increased from 15000 to 16000 (was in
C>                           verification version); initialize
C>                           variable JSR to zero in new COMMON block
C>                           /BUFRSR/ (was in verification version);
C>                           unified/portable for WRF; added history
C>                           documentation
C> - 2004-08-18  J. Ator    -- Added initialization of COMMON /MSGSTD/;
C>                           maximum message length increased from
C>                           20,000 to 50,000 bytes
C> - 2005-11-29  J. Ator    -- Added initialization of COMMON /MSGCMP/
C>			    and calls to pkvs1 and pkvs01()
C> - 2009-03-23  J. Ator    -- Added initialization of COMMON /DSCACH/,
C>                           COMMON /MSTINF/ and COMMON /TNKRCP/
C> - 2012-09-15  J. Woollen -- Modified for C/I/O/BUFR interface,
C>                           added initialization of COMMON blocks
C>                           /ENDORD/ and /BUFRBMISS/
C> - 2014-09-15  J. Ator    -- Change default location of MTDIR
C> - 2014-11-18  J. Ator    -- Added initialization of modules MSGLIM
C>                           and USRINT; remove S01CM initialization
C> - 2016-11-29  J. Ator    -- Explicitly initialize BMISS as 10E10_8
C> - 2017-10-13  J. Ator    -- Added initialization of COMMON /TABLEF/
C> - 2019-05-03  J. Ator    -- Change default location of MTDIR
C> - 2019-05-09  J. Ator    -- Added dimensions for MSGLEN
C>
C> <b>This routine calls:</b> ifxy()  ipkm()
C>
C> <b>This routine is called by:</b> openbf()
C>                <br>Normally not called by any application programs.
C>
      SUBROUTINE BFRINI

      USE MODA_STBFR
      USE MODA_IDRDM
      USE MODA_MSGLIM
      USE MODA_BITBUF
      USE MODA_BUFRMG
      USE MODA_BUFRSR
      USE MODA_TABABD
      USE MODA_USRINT
      USE MODA_TABLES
      USE MODA_H4WLC

      INCLUDE 'bufrlib.inc'

      COMMON /MAXCMP/ MAXCMB,MAXROW,MAXCOL,NCMSGS,NCSUBS,NCBYTS
      COMMON /PADESC/ IBCT,IPD1,IPD2,IPD3,IPD4
      COMMON /REPTAB/ IDNR(5,2),TYPS(5,2),REPS(5,2),LENS(5)
      COMMON /DXTAB / MAXDX,IDXV,NXSTR(10),LDXA(10),LDXB(10),LDXD(10),
     .                LD30(10),DXSTR(10)
      COMMON /DSCACH/ NCNEM,CNEM(MXCNEM),NDC(MXCNEM),
     .                IDCACH(MXCNEM,MAXNC)
      COMMON /MRGCOM/ NRPL,NMRG,NAMB,NTOT
      COMMON /DATELN/ LENDAT
      COMMON /ACMODE/ IAC
      COMMON /MSGSTD/ CSMF
      COMMON /MSGCMP/ CCMF
      COMMON /TABLEF/ CDMF
      COMMON /TNKRCP/ ITRYR,ITRMO,ITRDY,ITRHR,ITRMI,CTRT
      COMMON /MSTINF/ LUN1,LUN2,LMTD,MTDIR
      COMMON /ENDORD/ IBLOCK,IORDBE(4),IORDLE(4)

      CHARACTER*100 MTDIR
      CHARACTER*56  DXSTR
      CHARACTER*8   CNEM
      CHARACTER*6   ADSN(5,2),DNDX(25,10)
      CHARACTER*3   TYPX(5,2),TYPS
      CHARACTER*1   REPX(5,2),REPS
      CHARACTER*1   CSMF
      CHARACTER*1   CCMF
      CHARACTER*1   CDMF
      CHARACTER*1   CTRT
      DIMENSION     NDNDX(10),NLDXA(10),NLDXB(10),NLDXD(10),NLD30(10)
      DIMENSION     LENX(5)

      DATA ADSN   / '101000','360001','360002','360003','360004' ,
     .              '101255','031002','031001','031001','031000' /
      DATA TYPX   /    'REP',   'DRP',   'DRP',   'DRS' ,  'DRB' ,
     .                 'SEQ',   'RPC',   'RPC',   'RPS' ,  'SEQ' /
      DATA REPX   /      '"',     '(',     '{',     '[' ,    '<' ,
     .                   '"',     ')',     '}',     ']' ,    '>' /
      DATA LENX   /       0 ,     16 ,      8 ,      8  ,     1  /

      DATA (DNDX(I,1),I=1,25)/
     .'102000','031001','000001','000002',
     .'110000','031001','000010','000011','000012','000013','000015',
     .                  '000016','000017','000018','000019','000020',
     .'107000','031001','000010','000011','000012','000013','101000',
     .                  '031001','000030'/

      DATA (DNDX(I,2),I=1,15)/
     .'103000','031001','000001','000002','000003',
     .'101000','031001','300004',
     .'105000','031001','300003','205064','101000','031001','000030'/

      DATA NDNDX /  25 ,  15 , 8*0 /
      DATA NLDXA /  35 ,  67 , 8*0 /
      DATA NLDXB /  80 , 112 , 8*0 /
      DATA NLDXD /  38 ,  70 , 8*0 /
      DATA NLD30 /   5 ,   6 , 8*0 /

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  INITIALIZE /ENDORD/ TO CONTROL OUTPUT BLOCKING -1=LE 0=NONE +1=BE
C  -----------------------------------------------------------------

      IBLOCK = 0

C  INITIALIZE /BUFRBMISS/
C  ----------------------

      BMISS = 10E10_8

C  INITIALIZE MODULE BITBUF
C  ------------------------

      MAXBYT = MIN(10000,MXMSGL)

C  INITIALIZE MODULE H4WLC
C  -----------------------

      NH4WLC = 0

C  INITIALIZE /MAXCMP/
C  -------------------

      MAXCMB = MAXBYT
      MAXROW = 0
      MAXCOL = 0
      NCMSGS = 0
      NCSUBS = 0
      NCBYTS = 0

C  INITIALIZE /PADESC/
C  -------------------

      IBCT = IFXY('063000')
      IPD1 = IFXY('102000')
      IPD2 = IFXY('031001')
      IPD3 = IFXY('206001')
      IPD4 = IFXY('063255')

C  INITIALIZE MODULE STBFR
C  -----------------------

      DO I=1,NFILES
      IOLUN(I) = 0
      IOMSG(I) = 0
      ENDDO

C  INITIALIZE MODULE IDRDM
C  -----------------------

      DO I=1,NFILES
      IDRDM(I) = 0
      ENDDO

C  INITIALIZE MODULE MSGLIM
C  ------------------------

      DO I=1,NFILES
      MSGLIM(I) = 3
      ENDDO

C  INITIALIZE MODULE USRINT
C  ------------------------

      DO I=1,NFILES
      NVAL(I) = 0
      ENDDO

C  INITIALIZE /REPTAB/
C  -------------------

      DO I=1,5
      LENS(I) = LENX(I)
      DO J=1,2
      IDNR(I,J) = IFXY(ADSN(I,J))
      TYPS(I,J) = TYPX(I,J)
      REPS(I,J) = REPX(I,J)
      ENDDO
      ENDDO

C  INITIALIZE TABABD (INTERNAL ARRAYS HOLDING DICTIONARY TABLE)
C  ------------------------------------------------------------

C    NTBA(0) is the maximum number of entries w/i internal BUFR table A

      NTBA(0) = MAXTBA

C    NTBB(0) is the maximum number of entries w/i internal BUFR Table B

      NTBB(0) = MAXTBB

C    NTBD(0) is the maximum number of entries w/i internal BUFR Table D

      NTBD(0) = MAXTBD

C  INITIALIZE /DXTAB/
C  ------------------

      MAXDX = MAXBYT
c  .... IDXV is the version number of the local tables
      IDXV  = 1

      DO J=1,10
      LDXA(J)  = NLDXA(J)
      LDXB(J)  = NLDXB(J)
      LDXD(J)  = NLDXD(J)
      LD30(J)  = NLD30(J)
      DXSTR(J) = '      '
      NXSTR(J) = NDNDX(J)*2
      DO I=1,NDNDX(J)
      I1 = I*2-1
      CALL IPKM(DXSTR(J)(I1:I1),2,IFXY(DNDX(I,J)))
      ENDDO
      ENDDO

C  INITIALIZE MODULE TABLES
C  ------------------------

      MAXTAB = MAXJL

C  INITIALIZE MODULE BUFRMG
C  ------------------------

      DO I=1,NFILES
      MSGLEN(I) = 0
      ENDDO

C  INITIALIZE /MRGCOM/
C  -------------------

      NRPL = 0
      NMRG = 0
      NAMB = 0
      NTOT = 0

C  INITIALIZE /DATELN/
C  -------------------

      IF(LENDAT.NE.10) LENDAT = 8

C  INITIALIZE /ACMODE/
C  ------------------_

c  .... DK: What does this control??
      IAC = 0

C  INITIALIZE MODULE BUFRSR
C  ------------------------

      DO I=1,NFILES
      JSR(I) = 0
      ENDDO

C  INITIALIZE /DSCACH/
C  -------------------

      NCNEM = 0

C  INITIALIZE /MSGSTD/
C  -------------------

      CSMF = 'N'

C  INITIALIZE /MSGCMP/
C  -------------------

      CCMF = 'N'

C  INITIALIZE /TABLEF/
C  -------------------

      CDMF = 'N'

C  INITIALIZE /TNKRCP/
C  -------------------

      CTRT = 'N'

C  INITIALIZE /MSTINF/
C  -------------------

      MTDIR = '/gpfs/dell1/nco/ops/nwprod/decoders/decod_shared/fix'
      LMTD = 33

      LUN1 = 98
      LUN2 = 99

      RETURN
      END
