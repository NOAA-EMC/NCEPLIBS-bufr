!> @file
!> @brief Allocate or deallocate Fortran language arrays.
!>
!> @author J. Ator @date 2014-12-04

!> Dynamically allocate Fortran language arrays.
!>
!> This subroutine is called internally during the first call to
!> subroutine openbf() from an application program, in order to
!> dynamically allocate internal Fortran language arrays based on
!> parameter values set during one or more previous calls to function
!> isetprm().
!>
!> All memory allocated within this subroutine can be freed via a
!> subsequent call to subroutine exitbufr().
!>
!> @author J. Ator @date 2014-12-04
subroutine arallocf

  use modv_vars, only: maxcd, maxjl, maxmem, maxmsg, maxss, maxtba, maxtbb, maxtbd, mxbtm, mxbtmse, &
                       mxcdv, mxcsb, mxdxts, mxlcc, mxmsgl, mxmsgld4, mxmtbb, mxmtbd, mxnrv, mxrst, &
                       mxs01v, mxtamc, mxtco, mxh4wlc, nfiles, mxcnem, maxnc, maxrcr

  use moda_usrint
  use moda_usrbit
  use moda_ival
  use moda_msgcwd
  use moda_stbfr
  use moda_ufbcpl
  use moda_sc3bfr
  use moda_unptyp
  use moda_lushr
  use moda_nulbfr
  use moda_stcode
  use moda_idrdm
  use moda_xtab
  use moda_msglim
  use moda_bitbuf
  use moda_mgwa
  use moda_mgwb
  use moda_bufrmg
  use moda_bufrsr
  use moda_msgmem
  use moda_tababd
  use moda_tables
  use moda_usrtmp
  use moda_ivttmp
  use moda_comprx
  use moda_comprs
  use moda_mstabs
  use moda_rdmtb
  use moda_nmikrp
  use moda_s01cm
  use moda_bitmaps
  use moda_nrv203
  use moda_rlccmn
  use moda_h4wlc
  use moda_dscach
  use moda_s3list

  implicit none

  character*80 errstr
  character*36 brtstr

  integer iost, iprt

  common /quiet/ iprt

  if ( iprt .ge. 1 ) then
    call errwrt ('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
    call errwrt ('BUFRLIB: ARRAYS WILL BE DYNAMICALLY ALLOCATED USING THE FOLLOWING VALUES:')
    write ( errstr, '(a,i7)' ) '    MAXSS = ', maxss
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '   NFILES = ', nfiles
    call errwrt (errstr)
    write ( errstr, '(a,i7)' ) '   MXMSGL = ', mxmsgl
    call errwrt (errstr)
    write ( errstr, '(a,i5)' ) '   MXDXTS = ', mxdxts
    call errwrt (errstr)
    write ( errstr, '(a,i7)' ) '   MAXMSG = ', maxmsg
    call errwrt (errstr)
    write ( errstr, '(a,i9)' ) '   MAXMEM = ', maxmem
    call errwrt (errstr)
    write ( errstr, '(a,i5)' ) '   MAXTBA = ', maxtba
    call errwrt (errstr)
    write ( errstr, '(a,i5)' ) '   MAXTBB = ', maxtbb
    call errwrt (errstr)
    write ( errstr, '(a,i5)' ) '   MAXTBD = ', maxtbd
    call errwrt (errstr)
    write ( errstr, '(a,i7)' ) '    MAXJL = ', maxjl
    call errwrt (errstr)
    write ( errstr, '(a,i6)' ) '    MXCDV = ', mxcdv
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '    MXLCC = ', mxlcc
    call errwrt (errstr)
    write ( errstr, '(a,i6)' ) '    MXCSB = ', mxcsb
    call errwrt (errstr)
    write ( errstr, '(a,i5)' ) '   MXMTBB = ', mxmtbb
    call errwrt (errstr)
    write ( errstr, '(a,i5)' ) '   MXMTBD = ', mxmtbd
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '    MAXCD = ', maxcd
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '    MXNRV = ', mxnrv
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '   MXS01V = ', mxs01v
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '   MXTAMC = ', mxtamc
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '    MXTCO = ', mxtco
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '    MXBTM = ', mxbtm
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '  MXBTMSE = ', mxbtmse
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '  MXH4WLC = ', mxh4wlc
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '    MXRST = ', mxrst
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '   MXCNEM = ', mxcnem
    call errwrt (errstr)
    write ( errstr, '(a,i4)' ) '    MAXNC = ', maxnc
    call errwrt (errstr)
    call errwrt ('++++++++++++++BUFR ARCHIVE LIBRARY+++++++++++++++++')
  end if

  brtstr = 'BUFRLIB: ARALLOCF FAILED ALLOCATING '

  ! moda_usrint arrays.

  allocate( nval(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NVAL' )

  allocate( inv(maxss,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'INV' )

  allocate( nrfelm(maxss,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NRFELM' )

  allocate( val(maxss,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'VAL' )

  ! moda_usrbit arrays.

  allocate( nbit(maxss), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NBIT' )

  allocate( mbit(maxss), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MBIT' )

  ! moda_ival arrays.

  allocate( ival(maxss), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IVAL' )

  ! moda_msgcwd arrays.

  allocate( nmsg(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NMSG' )

  allocate( nsub(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NSUB' )

  allocate( msub(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MSUB' )

  allocate( inode(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'INODE' )

  allocate( idate(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDATE' )

  ! moda_stbfr arrays.

  allocate( iolun(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IOLUN' )

  allocate( iomsg(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IOMSG' )

  ! moda_ufbcpl arrays.

  allocate( luncpy(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'LUNCPY' )

  ! moda_sc3bfr arrays.

  allocate( isc3(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ISC3' )

  allocate( tamnem(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'TAMNEM' )

  ! moda_unptyp arrays.

  allocate( msgunp(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MSGUNP' )

  ! moda_lushr arrays.

  allocate( lus(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'LUS' )

  ! moda_nulbfr arrays.

  allocate( null(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NULL' )

  ! moda_stcode arrays.

  allocate( iscodes(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ISCODES' )

  ! moda_idrdm arrays.

  allocate( idrdm(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDRDM' )

  ! moda_xtab arrays.

  allocate( xtab(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'XTAB' )

  ! moda_msglim arrays.

  allocate( msglim(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MSGLIM' )

  ! Calculate mxmsgld4 from mxmsgl.

  if ( mod(mxmsgl,4) .eq. 0 ) then
      mxmsgld4 = mxmsgl/4
  else
      mxmsgld4 = mxmsgl/4 + 1
  end if

  ! moda_bitbuf arrays.

  allocate( ibay(mxmsgld4), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IBAY' )

  allocate( mbyt(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MBYT' )

  allocate( mbay(mxmsgld4,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MBAY' )

  ! moda_mgwa arrays.

  allocate( mgwa(mxmsgld4), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MGWA' )

  ! moda_mgwb arrays.

  allocate( mgwb(mxmsgld4), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MGWB' )

  ! moda_bufrmg arrays.

  allocate( msglen(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MSGLEN' )
  allocate( msgtxt(mxmsgld4,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MSGTXT' )

  ! moda_bufrsr arrays.

  allocate( jsr(nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'JSR' )

  allocate( jbay(mxmsgld4), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'JBAY' )

  ! Calculate mxdxm and mxdxw from mxdxts and mxmsgld4.

  mxdxm = mxdxts*3
  mxdxw = mxdxm*mxmsgld4

  ! moda_msgmem arrays.

  allocate( msgp(0:maxmsg), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MSGP' )

  allocate( msgs(maxmem), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MSGS' )

  allocate( mdx(mxdxw), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MDX' )

  allocate( ipdxm(mxdxm), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IPDXM' )

  allocate( ifdxts(mxdxts), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ifDXTS' )

  allocate( icdxts(mxdxts), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ICDXTS' )

  allocate( ipmsgs(mxdxts), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IPMSGS' )

  ! moda_tababd arrays.

  allocate( ntba(0:nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NTBA' )

  allocate( ntbb(0:nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NTBB' )

  allocate( ntbd(0:nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NTBD' )

  allocate( mtab(maxtba,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MTAB' )

  allocate( idna(maxtba,nfiles,2), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDNA' )

  allocate( idnb(maxtbb,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDNB' )

  allocate( idnd(maxtbd,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDND' )

  allocate( taba(maxtba,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'TABA' )

  allocate( tabb(maxtbb,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'TABB' )

  allocate( tabd(maxtbd,nfiles), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'TABD' )

  ! moda_tables arrays.

  allocate( tag(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'TAG' )

  allocate( typ(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'TYP' )

  allocate( knt(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'KNT' )

  allocate( jump(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'JUMP' )

  allocate( link(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'LINK' )

  allocate( jmpb(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'JMPB' )

  allocate( ibt(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IBT' )

  allocate( irf(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IRF' )

  allocate( isc(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ISC' )

  allocate( itp(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ITP' )

  allocate( vali(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'VALI' )

  allocate( knti(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'KNTI' )

  allocate( iseq(maxjl,2), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ISEQ' )

  allocate( jseq(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'JSEQ' )

  ! moda_usrtmp arrays.

  allocate( iutmp(maxjl,maxrcr), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IUTMP' )

  allocate( vutmp(maxjl,maxrcr), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'VUTMP' )

  ! moda_ivttmp arrays.

  allocate( ttmp(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'TTMP' )

  allocate( itmp(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ITMP' )

  allocate( vtmp(maxjl), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'VTMP' )

  ! moda_comprx arrays.

  allocate( kmin(mxcdv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'KMIN' )

  allocate( kmax(mxcdv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'KMAX' )

  allocate( kmis(mxcdv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'KMIS' )

  allocate( kbit(mxcdv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'KBIT' )

  allocate( ityp(mxcdv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ITYP' )

  allocate( iwid(mxcdv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IWID' )

  allocate( character*(mxlcc) :: cstr(mxcdv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CSTR' )

  ! moda_comprs arrays.

  allocate( matx(mxcdv,mxcsb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'MATX' )

  allocate( character*(mxlcc) :: catx(mxcdv,mxcsb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CATX' )

  ! moda_mstabs arrays.

  allocate( ibfxyn(mxmtbb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IBFXYN' )

  allocate( cbscl(4,mxmtbb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CBSCL' )

  allocate( cbsref(12,mxmtbb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CBSREF' )

  allocate( cbbw(4,mxmtbb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CBBW' )

  allocate( cbunit(24,mxmtbb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CBUNIT' )

  allocate( cbmnem(8,mxmtbb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CBMNEM' )

  allocate( cbelem(120,mxmtbb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CBELEM' )

  allocate( idfxyn(mxmtbd), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDFXYN' )

  allocate( cdseq(120,mxmtbd), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CDSEQ' )

  allocate( cdmnem(8,mxmtbd), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CDMNEM' )

  allocate( ndelem(mxmtbd), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NDELEM' )

  allocate( idefxy(mxmtbd*maxcd), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDEFXY' )

  ! moda_rdmtb arrays.

  allocate( iefxyn(mxmtbd,maxcd), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IEFXYN' )

  allocate( cmdscb(mxmtbb), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CMDSCB' )

  allocate( cmdscd(mxmtbd), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CMDSCD' )

  allocate( ceelem(mxmtbd,maxcd), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CEELEM' )

  ! moda_nmikrp arrays.

  allocate( nem(maxcd,10), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NEM' )

  allocate( irp(maxcd,10), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IRP' )

  allocate( krp(maxcd,10), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'KRP' )

  ! moda_s01cm arrays.

  allocate( ivmnem(mxs01v), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IVMNEM' )

  allocate( cmnem(mxs01v), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CMNEM' )

  ! moda_bitmaps arrays.

  allocate( inodtamc(mxtamc), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'INODTAMC' )

  allocate( ntco(mxtamc), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NTCO' )

  allocate( ctco(mxtamc,mxtco), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CTCO' )

  allocate( inodtco(mxtamc,mxtco), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'INODTCO' )

  allocate( nbtmse(mxbtm), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NBTMSE' )

  allocate( istbtm(mxbtm), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ISTBTM' )

  allocate( iszbtm(mxbtm), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ISZBTM' )

  allocate( ibtmse(mxbtm,mxbtmse), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IBTMSE' )

  ! moda_nrv203 arrays.

  allocate( tagnrv(mxnrv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'TAGNRV' )

  allocate( inodnrv(mxnrv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'INODNRV' )

  allocate( nrv(mxnrv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NRV' )

  allocate( isnrv(mxnrv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'ISNRV' )

  allocate( ienrv(mxnrv), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IENRV' )

  ! moda_rlccmn arrays.

  allocate( irnch(mxrst), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IRNCH' )

  allocate( irbit(mxrst), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IRBIT' )

  allocate( crtag(mxrst), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CRTAG' )

  ! moda_h4wlc arrays.

  allocate( luh4wlc(mxh4wlc), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'LUH4WLC' )

  allocate( sth4wlc(mxh4wlc), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'STH4WLC' )

  allocate( chh4wlc(mxh4wlc), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CHH4WLC' )

  ! moda_dscach arrays.

  allocate( cnem(mxcnem), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CNEM' )

  allocate( ndc(mxcnem), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'NDC' )

  allocate( idcach(mxcnem,maxnc), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDCACH' )

  ! moda_s3list arrays.

  allocate( ids3(maxnc), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'IDS3' )

  allocate( cds3(maxnc), stat=iost )
  if ( iost .ne. 0 ) call bort( brtstr // 'CDS3' )

  return
end subroutine arallocf

!> Free all memory that was dynamically allocated during a previous call to subroutine arallocf().
!>
!> @author J. Ator @date 2014-12-04
subroutine ardllocf

  use bufrlib

  use moda_usrint
  use moda_usrbit
  use moda_ival
  use moda_msgcwd
  use moda_stbfr
  use moda_ufbcpl
  use moda_sc3bfr
  use moda_unptyp
  use moda_lushr
  use moda_nulbfr
  use moda_stcode
  use moda_idrdm
  use moda_xtab
  use moda_msglim
  use moda_bitbuf
  use moda_mgwa
  use moda_mgwb
  use moda_bufrmg
  use moda_bufrsr
  use moda_msgmem
  use moda_tababd
  use moda_tables
  use moda_usrtmp
  use moda_ivttmp
  use moda_comprx
  use moda_comprs
  use moda_mstabs
  use moda_rdmtb
  use moda_nmikrp
  use moda_s01cm
  use moda_bitmaps
  use moda_nrv203
  use moda_rlccmn
  use moda_h4wlc
  use moda_dscach
  use moda_s3list

  implicit none

  ! moda_usrint arrays.

  deallocate( nval )
  deallocate( inv )
  deallocate( nrfelm )
  deallocate( val )

  ! moda_usrbit arrays.

  deallocate( nbit )
  deallocate( mbit )

  ! moda_ival arrays.

  deallocate( ival )

  ! moda_msgcwd arrays.

  deallocate( nmsg )
  deallocate( nsub )
  deallocate( msub )
  deallocate( inode )
  deallocate( idate )

  ! moda_stbfr arrays.

  deallocate( iolun )
  deallocate( iomsg )

  ! moda_ufbcpl arrays.

  deallocate( luncpy )

  ! moda_sc3bfr arrays.

  deallocate( isc3 )
  deallocate( tamnem )

  ! moda_unptyp arrays.

  deallocate( MSGUNP )

  ! moda_lushr arrays.

  deallocate( lus )

  ! moda_nulbfr arrays.

  deallocate( null )

  ! moda_stcode arrays.

  deallocate( iscodes )

  ! moda_idrdm arrays.

  deallocate( idrdm )

  ! moda_xtab arrays.

  deallocate( xtab )

  ! moda_msglim arrays.

  deallocate( msglim )

  ! moda_bitbuf arrays.

  deallocate( ibay )
  deallocate( mbyt )
  deallocate( mbay )

  ! moda_mgwa arrays.

  deallocate( mgwa )

  ! moda_mgwb arrays.

  deallocate( mgwb )

  ! moda_bufrmg arrays.

  deallocate( msglen )
  deallocate( msgtxt )

  ! moda_bufrsr arrays.

  deallocate( jsr )
  deallocate( jbay )

  ! moda_msgmem arrays.

  deallocate( msgp )
  deallocate( msgs )
  deallocate( mdx )
  deallocate( ipdxm )
  deallocate( ifdxts )
  deallocate( icdxts )
  deallocate( ipmsgs )

  ! moda_tababd arrays.

  deallocate( ntba )
  deallocate( ntbb )
  deallocate( ntbd )
  deallocate( mtab )
  deallocate( idna )
  deallocate( idnb )
  deallocate( idnd )
  deallocate( taba )
  deallocate( tabb )
  deallocate( tabd )

  ! moda_tables arrays.

  deallocate( tag )
  deallocate( typ )
  deallocate( knt )
  deallocate( jump )
  deallocate( link )
  deallocate( jmpb )
  deallocate( ibt )
  deallocate( irf )
  deallocate( isc )
  deallocate( itp )
  deallocate( vali )
  deallocate( knti )
  deallocate( iseq )
  deallocate( jseq )

  ! moda_usrtmp arrays.

  deallocate( iutmp )
  deallocate( vutmp )

  ! moda_ivttmp arrays.

  deallocate( ttmp )
  deallocate( itmp )
  deallocate( vtmp )

  ! moda_comprx arrays.

  deallocate( kmin )
  deallocate( kmax )
  deallocate( kmis )
  deallocate( kbit )
  deallocate( ityp )
  deallocate( iwid )
  deallocate( cstr )

  ! moda_comprs arrays.

  deallocate( matx )
  deallocate( catx )

  ! moda_mstabs arrays.

  deallocate( ibfxyn )
  deallocate( cbscl )
  deallocate( cbsref )
  deallocate( cbbw )
  deallocate( cbunit )
  deallocate( cbmnem )
  deallocate( cbelem )
  deallocate( idfxyn )
  deallocate( cdseq )
  deallocate( cdmnem )
  deallocate( ndelem )
  deallocate( idefxy )

  ! moda_rdmtb arrays.

  deallocate( iefxyn )
  deallocate( cmdscb )
  deallocate( cmdscd )
  deallocate( ceelem )

  ! moda_nmikrp arrays.

  deallocate( nem )
  deallocate( irp )
  deallocate( krp )

  ! moda_s01cm arrays.

  deallocate( ivmnem )
  deallocate( cmnem )

  ! moda_bitmaps arrays.

  deallocate( inodtamc )
  deallocate( ntco )
  deallocate( ctco )
  deallocate( inodtco )
  deallocate( nbtmse )
  deallocate( istbtm )
  deallocate( iszbtm )
  deallocate( ibtmse )

  ! moda_nrv203 arrays.

  deallocate( tagnrv )
  deallocate( inodnrv )
  deallocate( nrv )
  deallocate( isnrv )
  deallocate( ienrv )

  ! moda_rlccmn arrays.

  deallocate( irnch )
  deallocate( irbit )
  deallocate( crtag )

  ! moda_h4wlc arrays.

  deallocate( luh4wlc )
  deallocate( sth4wlc )
  deallocate( chh4wlc )

  ! moda_dscach arrays.

  deallocate( cnem )
  deallocate( ndc )
  deallocate( idcach )

  ! moda_s3list arrays.

  deallocate( ids3 )
  deallocate( cds3 )

  ! C language arrays.

  call ardllocc_c

  return
end subroutine ardllocf

!> Free all dynamically-allocated memory,
!> close all logical units that are open within the
!> NCEPLIBS-bufr software, and reset the library to all of its
!> default settings as though it had never been called.
!>
!> @remarks
!> - Calling this subroutine allows an application program to
!>   potentially resize arrays and reallocate memory all over again
!>   with a new subsequent series of calls to subroutines isetprm()
!>   and openbf().  However, if and when this subroutine is called,
!>   there is no longer any internal memory available within the
!>   NCEPLIBS-bufr software, and the remainder of the library becomes
!>   essentially unusable within the application program, unless
!>   and until subroutine openbf() is called once again to
!>   dynamically allocate new array space.  This may be a useful
!>   capability for application programs that are finished with
!>   using the NCEPLIBS-bufr software and wish to move on to other
!>   unrelated tasks without continuing to tie up all of the
!>   allocated memory space within the library.  Otherwise, and
!>   unless there's a need to change parameter sizes following the
!>   first call to subroutine openbf(), then there's no need to ever
!>   call this subroutine within an application program, since all
!>   allocated memory will automatically get freed anyway by the
!>   operating system once the application program terminates.
!>
!> @author J. Ator @date 2015-03-02
recursive subroutine exitbufr

  use bufrlib

  use modv_vars, only: ifopbf, nfiles

  use moda_stbfr
  use moda_s01cm

  common /tablef/ cdmf

  character*1 cdmf

  ! Close any logical units that are open to the library.

  do jj = 1, nfiles
    if ( iolun(jj) .ne. 0 ) call closbf( abs(iolun(jj)) )
  end do

  ! Deallocate all allocated memory.

  call ardllocf

  if ( cdmf .eq. 'Y' ) call dlloctbf_c

  ! Reset the library.

  ns01v = 0
  ifopbf = 0

  return
end subroutine exitbufr
