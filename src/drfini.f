C> @file
C> @brief Initialize replication factors for delayed replication
C> sequences.

C> This subroutine explicitly initializes delayed replication factors
C> and allocates a corresponding amount of space within internal arrays,
C> thereby allowing the subsequent use of subroutine ufbseq() to write
C> data into delayed replication sequences.
C>
C> @author J. Woollen
C> @date 2002-05-14
C>
C> @param[in] LUNIT -- integer: Fortran logical unit number for BUFR
C>                     file
C> @param[in] MDRF  -- integer(*): Array of delayed replication factors,
C>                     in one-to-one correspondence with the number
C>                     of occurrences of DRFTAG within the overall
C>                     subset definition, and explicitly defining
C>                     how much space (i.e. how many replications)
C>                     to allocate within each successive occurrence
C> @param[in] NDRF  -- integer: Number of delayed replication factors
C>                     within MDRF
C> @param[in] DRFTAG -- character*(*): Table D sequence mnemonic,
C>                      bracketed by appropriate delayed replication
C>                      notation (e.g. {}, () OR <>)
C>
C> <p>Logical unit LUNIT should have already been opened for output
C> operations (i.e. writing/encoding BUFR) via a previous call to
C> subroutine openbf(), and a message for output should have already
C> been opened via a previous call to one of the
C> [message-writing subroutines].
C>
C> <p>The use of this subroutine is only required when writing data
C> into delayed replication sequences using ufbseq(), or for cases
C> where ufbint() or ufbrep() are being used to write data into
C> delayed replication sequences which occur more than once within
C> an overall subset definition.  In such cases, the use of this
C> subroutine allows the application code to explicitly specify how
C> many replications of the sequence are to be allocated to each
C> occurrence of the delayed replication sequence within the overall
C> subset definition, prior to storing all of the actual data values
C> themselves via a single subsequent call to ufbint() or ufbrep().
C> In contrast, the use of this subroutine is not required when
C> ufbint() or ufbrep() are to be called to store data values
C> for a delayed replication sequence which only occurs one time
C> within an overall subset definition, because in that case the
C> same type of initialization and space allocation functionality
C> will be automatically handled internally within subroutine
C> ufbint() or ufbrep().
C>
C> <b>Program history log:</b>
C> | Date | Programmer | Comments |
C> | -----|------------|----------|
C> | 2002-05-14 | J. Woollen | Original author |
C> | 2003-11-04 | S. Bender | Added remarks and routine interdependencies
C> | 2003-11-04 | D. Keyser | Unified/portable for WRF; added history documentation; outputs more complete diagnostic info when routine terminates abnormally |
C> | 2005-03-04 | J. Ator | Updated documentation |
C> | 2014-09-08 | J. Ator | Increase NDRF limit from 100 to 200 |
C> | 2014-12-10 | J. Ator | Use modules instead of COMMON blocks |
C> | 2018-06-07 | J. Ator | Increase NDRF limit from 200 to 2000 |
C> | 2022-08-04 | J. Woollen | Added 8-byte wrapper |

      RECURSIVE SUBROUTINE DRFINI(LUNIT,MDRF,NDRF,DRFTAG)

      USE MODA_USRINT
      USE MODA_TABLES
      USE MODV_IM8B

      PARAMETER ( MXDRF = 2000 )

      CHARACTER*(*) DRFTAG
      CHARACTER*128 BORT_STR
      DIMENSION     MDRF(*),LUNIT(*),NDRF(*)
      DIMENSION     MY_MDRF(MXDRF),MY_LUNIT(1),MY_NDRF(1)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUNIT,MY_LUNIT,1)
         CALL X84(NDRF,MY_NDRF,1)
         CALL X84(MDRF,MY_MDRF,MY_NDRF)
         CALL DRFINI(MY_LUNIT,MY_MDRF,MY_NDRF,DRFTAG)

         IM8B=.TRUE.
         RETURN
      ENDIF

      IF(NDRF(1).GT.MXDRF) GOTO 900

      CALL STATUS(LUNIT,LUN,IL,IM)

C  COMFORM THE TEMPLATES TO THE DELAYED REPLICATION FACTORS
C  --------------------------------------------------------

      M = 0
      N = 0

10    DO N=N+1,NVAL(LUN)
      NODE = INV(N,LUN)
      IF(ITP(NODE).EQ.1 .AND. TAG(NODE).EQ.DRFTAG) THEN
         M = M+1
         CALL USRTPL(LUN,N,MDRF(M))
         GOTO 10
      ENDIF
      ENDDO

C  EXITS
C  -----

      RETURN
 900  WRITE(BORT_STR,'("BUFRLIB: DRFINI - THE NUMBER OF DELAYED '//
     . 'REPLICATION FACTORS (",I5,") EXCEEDS THE LIMIT (",I5,")")')
     .  NDRF(1),MXDRF
      CALL BORT(BORT_STR)
      END
