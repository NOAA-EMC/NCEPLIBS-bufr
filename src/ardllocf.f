C> @file
C> @brief Free all dynamically-allocated memory within internal
C> Fortran language arrays.
C>
C> @author J. Ator @date 2014-12-04

C> Free all memory that was dynamically allocated
C> during a previous call to subroutine arallocf().
C>
C> @author J. Ator @date 2014-12-04
        SUBROUTINE ARDLLOCF

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

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       MODA_USRINT arrays.

        DEALLOCATE( NVAL )
        DEALLOCATE( INV )
        DEALLOCATE( NRFELM )
        DEALLOCATE( VAL )

C       MODA_USRBIT arrays.

        DEALLOCATE( NBIT )
        DEALLOCATE( MBIT )

C       MODA_IVAL arrays.

        DEALLOCATE( IVAL )

C       MODA_MSGCWD arrays.

        DEALLOCATE( NMSG )
        DEALLOCATE( NSUB )
        DEALLOCATE( MSUB )
        DEALLOCATE( INODE )
        DEALLOCATE( IDATE )

C       MODA_STBFR arrays.

        DEALLOCATE( IOLUN )
        DEALLOCATE( IOMSG )

C       MODA_UFBCPL arrays.

        DEALLOCATE( LUNCPY )

C       MODA_SC3BFR arrays.

        DEALLOCATE( ISC3 )
        DEALLOCATE( TAMNEM )

C       MODA_UNPTYP arrays.

        DEALLOCATE( MSGUNP )

C       MODA_LUSHR arrays.

        DEALLOCATE( LUS )

C       MODA_NULBFR arrays.

        DEALLOCATE( NULL )

C       MODA_STCODE arrays.

        DEALLOCATE( ISCODES )

C       MODA_IDRDM arrays.

        DEALLOCATE( IDRDM )

C       MODA_XTAB arrays.

        DEALLOCATE( XTAB )

C       MODA_MSGLIM arrays.

        DEALLOCATE( MSGLIM )

C       MODA_BITBUF arrays.

        DEALLOCATE( IBAY )
        DEALLOCATE( MBYT )
        DEALLOCATE( MBAY )

C       MODA_MGWA arrays.

        DEALLOCATE( MGWA )

C       MODA_MGWB arrays.

        DEALLOCATE( MGWB )

C       MODA_BUFRMG arrays.

        DEALLOCATE( MSGLEN )
        DEALLOCATE( MSGTXT )

C       MODA_BUFRSR arrays.

        DEALLOCATE( JSR )
        DEALLOCATE( JBAY )

C       MODA_MSGMEM arrays.

        DEALLOCATE( MSGP )
        DEALLOCATE( MSGS )
        DEALLOCATE( MDX )
        DEALLOCATE( IPDXM )
        DEALLOCATE( IFDXTS )
        DEALLOCATE( ICDXTS )
        DEALLOCATE( IPMSGS )

C       MODA_TABABD arrays.

        DEALLOCATE( NTBA )
        DEALLOCATE( NTBB )
        DEALLOCATE( NTBD )
        DEALLOCATE( MTAB )
        DEALLOCATE( IDNA )
        DEALLOCATE( IDNB )
        DEALLOCATE( IDND )
        DEALLOCATE( TABA )
        DEALLOCATE( TABB )
        DEALLOCATE( TABD )

C       MODA_TABLES arrays.

        DEALLOCATE( TAG )
        DEALLOCATE( TYP )
        DEALLOCATE( KNT )
        DEALLOCATE( JUMP )
        DEALLOCATE( LINK )
        DEALLOCATE( JMPB )
        DEALLOCATE( IBT )
        DEALLOCATE( IRF )
        DEALLOCATE( ISC )
        DEALLOCATE( ITP )
        DEALLOCATE( VALI )
        DEALLOCATE( KNTI )
        DEALLOCATE( ISEQ )
        DEALLOCATE( JSEQ )

C       MODA_USRTMP arrays.

        DEALLOCATE( IUTMP )
        DEALLOCATE( VUTMP )

C       MODA_IVTTMP arrays.

        DEALLOCATE( TTMP )
        DEALLOCATE( ITMP )
        DEALLOCATE( VTMP )

C       MODA_COMPRX arrays.

        DEALLOCATE( KMIN )
        DEALLOCATE( KMAX )
        DEALLOCATE( KMIS )
        DEALLOCATE( KBIT )
        DEALLOCATE( ITYP )
        DEALLOCATE( IWID )
        DEALLOCATE( CSTR )

C       MODA_COMPRS arrays.

        DEALLOCATE( MATX )
        DEALLOCATE( CATX )

C       MODA_MSTABS arrays.

        DEALLOCATE( IBFXYN )
        DEALLOCATE( CBSCL )
        DEALLOCATE( CBSREF )
        DEALLOCATE( CBBW )
        DEALLOCATE( CBUNIT )
        DEALLOCATE( CBMNEM )
        DEALLOCATE( CBELEM )
        DEALLOCATE( IDFXYN )
        DEALLOCATE( CDSEQ )
        DEALLOCATE( CDMNEM )
        DEALLOCATE( NDELEM )
        DEALLOCATE( IDEFXY )

C       MODA_RDMTB arrays.

        DEALLOCATE( IEFXYN )
        DEALLOCATE( CMDSCB )
        DEALLOCATE( CMDSCD )
        DEALLOCATE( CEELEM )

C       MODA_NMIKRP arrays.

        DEALLOCATE( NEM )
        DEALLOCATE( IRP )
        DEALLOCATE( KRP )

C       MODA_S01CM arrays.

        DEALLOCATE( IVMNEM )
        DEALLOCATE( CMNEM )

C       MODA_BITMAPS arrays.

        DEALLOCATE( INODTAMC )
        DEALLOCATE( NTCO )
        DEALLOCATE( CTCO )
        DEALLOCATE( INODTCO )
        DEALLOCATE( NBTMSE )
        DEALLOCATE( ISTBTM )
        DEALLOCATE( ISZBTM )
        DEALLOCATE( IBTMSE )

C       MODA_NRV203 arrays.

        DEALLOCATE( TAGNRV )
        DEALLOCATE( INODNRV )
        DEALLOCATE( NRV )
        DEALLOCATE( ISNRV )
        DEALLOCATE( IENRV )

C       MODA_RLCCMN arrays.

        DEALLOCATE( IRNCH )
        DEALLOCATE( IRBIT )
        DEALLOCATE( CRTAG )

C       MODA_H4WLC arrays.

        DEALLOCATE( LUH4WLC )
        DEALLOCATE( STH4WLC )
        DEALLOCATE( CHH4WLC )

C       MODA_DSCACH arrays.

        DEALLOCATE( CNEM )
        DEALLOCATE( NDC )
        DEALLOCATE( IDCACH )

C       C language arrays.

        CALL ARDLLOCC_C

        RETURN
        END
