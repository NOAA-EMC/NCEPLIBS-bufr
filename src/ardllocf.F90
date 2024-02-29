!> @file
!> @brief Free all dynamically-allocated memory within internal Fortran language arrays.
!>
!> @author J. Ator @date 2014-12-04

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

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

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
