C> @file
C> @brief Get Table B and Table D information from the internal DX BUFR
C> tables
      
C> This subroutine reads Table B and Table D information from the
C> internal DX BUFR tables for a specified Fortran logical unit, then
C> returns this information in a pre-defined ASCII format.
C>
C> @author J. Ator
C> @date 2005-11-29
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR file
C> @param[in] ITAB  -- integer: Dimensioned size of TABDB array; used
C>                     by the subroutine to ensure that it doesn't
C>                     overflow the TABDB array
C> @param[out] TABDB -- character*128(*): Internal Table B and Table D
C>                      information
C> @param[out] JTAB -- integer: Number of entries stored within TABDB
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2005-11-29 | J. Ator    | Original author      |
C> | 2014-12-10 | J. Ator    | Use modules instead of COMMON blocks |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      SUBROUTINE GETABDB(LUNIT,TABDB,ITAB,JTAB)

      USE MODA_TABABD
      USE MODA_NMIKRP
      USE MODV_IM8B

      CHARACTER*128 TABDB(*)
      CHARACTER*8   NEMO
      INTEGER*8 LUNIT_8,ITAB_8,JTAB_8

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         LUNIT_8=LUNIT
         ITAB_8=ITAB
         CALL GETABDB_8(LUNIT_8,TABDB,ITAB_8,JTAB_8)
         JTAB=JTAB_8

         IM8B=.TRUE.
         RETURN
      ENDIF

      JTAB = 0

C  MAKE SURE THE FILE IS OPEN
C  --------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) RETURN

C  WRITE OUT THE TABLE D ENTRIES FOR THIS FILE
C  -------------------------------------------

      DO I=1,NTBD(LUN)
      NEMO = TABD(I,LUN)(7:14)
      CALL NEMTBD(LUN,I,NSEQ,NEM(1,1),IRP(1,1),KRP(1,1))
      DO J=1,NSEQ,10
      JTAB = JTAB+1
      IF(JTAB.LE.ITAB) THEN
         WRITE(TABDB(JTAB),1) NEMO,(NEM(K,1),K=J,MIN(J+9,NSEQ))
1        FORMAT('D ',A8,10(1X,A10))
      ENDIF
      ENDDO
      ENDDO

C  ADD THE TABLE B ENTRIES
C  -----------------------

      DO I=1,NTBB(LUN)
      JTAB = JTAB+1
      IF(JTAB.LE.ITAB) THEN
         WRITE(TABDB(JTAB),2) TABB(I,LUN)(7:14),TABB(I,LUN)(71:112)
2        FORMAT('B ',A8,1X,A42)
      ENDIF
      ENDDO

      RETURN
      END

C> This subroutine is an internal wrapper for handling 8-byte integer
C> arguments to subroutine getabdb().
C>
C> <p>Application programs which use 8-byte integer arguments should
C> never call this subroutine directly; instead, such programs should
C> make an initial call to subroutine setim8b() with int8b=.TRUE. and
C> then call subroutine getabdb() directly.
C>
C> @author J. Woollen
C> @date 2022-08-04
C>
C> @param[in] LUNIT_8 -- integer*8: Fortran logical unit number for BUFR file
C> @param[in] ITAB_8  -- integer*8: Dimensioned size of TABDB array; used
C>                       by the subroutine to ensure that it doesn't
C>                       overflow the TABDB array
C> @param[out] TABDB -- character*128(*): Internal Table B and Table D
C>                      information
C> @param[out] JTAB_8 -- integer*8: Number of entries stored within TABDB
C>
C> <b>Program history log:</b>
C> | Date       | Programmer | Comments             |
C> | -----------|------------|----------------------|
C> | 2022-08-04 | J. Woollen | Original author      |

      SUBROUTINE GETABDB_8(LUNIT_8,TABDB,ITAB_8,JTAB_8) 

      CHARACTER*128 TABDB(*)
      INTEGER*8 LUNIT_8,ITAB_8,JTAB_8

      LUNIT=LUNIT_8
      ITAB=ITAB_8
      CALL GETABDB(LUNIT,TABDB,ITAB,JTAB)
      JTAB_8=JTAB

      RETURN
      END
