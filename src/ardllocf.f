C> @file
C> @brief Free all dynamically-allocated memory within internal
C> Fortran language arrays.
C>
C> @author J. Ator @date 2014-12-04

C> This subroutine frees any memory that was dynamically allocated
C> during a previous call to subroutine arallocf().
C>
C> @author J. Ator @date 2014-12-04
        SUBROUTINE ARDLLOCF

        USE MODA_USRINT
        USE MODA_USRBIT
        USE MODA_IVAL
        USE MODA_MSGCWD
        USE MODA_STBFR
        USE MODA_UFBCPL
        USE MODA_SC3BFR
        USE MODA_UNPTYP
        USE MODA_LUSHR
        USE MODA_NULBFR
        USE MODA_STCODE
        USE MODA_IDRDM
        USE MODA_XTAB
        USE MODA_MSGLIM
        USE MODA_BITBUF
        USE MODA_MGWA
        USE MODA_MGWB
        USE MODA_BUFRMG
        USE MODA_BUFRSR
        USE MODA_MSGMEM
        USE MODA_TABABD
        USE MODA_TABLES
        USE MODA_USRTMP
        USE MODA_IVTTMP
        USE MODA_COMPRX
        USE MODA_COMPRS
        USE MODA_MSTABS
        USE MODA_RDMTB
        USE MODA_NMIKRP
        USE MODA_S01CM
        USE MODA_BITMAPS
        USE MODA_NRV203
        USE MODA_RLCCMN
        use moda_dscach

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       moda_dscach arrays.

        deallocate( idcach )

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

C       C language arrays.

        CALL ARDLLOCC

        RETURN
        END
