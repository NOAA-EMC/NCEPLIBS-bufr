C> @file
C> @brief Pad a BUFR data subset with zeroed-out bits up to the
C> next byte boundary.
C>
C> @author Woollen @date 1994-01-06

C> Pack the value for the number of bits being "padded", starting with bit
C> ibit+1 and using eight bits in the packed array ibay (which
C> represents a subset packed into ibit bits). Then, starting with
C> ibit+9, pack zeroes (i.e., "pads") to the specified bit
C> boundary (ipadb).
C>
C> Note that it's the number of bits padded here that
C> was packed in bits ibit+1 through ibit+8 - this is actually a
C> delayed replication factor! IPADB must be a multiple of eight and
C> represents the bit boundary on which the packed subset in ibay
C> should end after padding. For example, if ipadb is "8", then the
C> number of bits in ibay actually consumed by packed data (including
C> the padding) will be a multiple of eight. If ipadb is "16", it
C> will be a multiple of sixteen.  in either (or any) case, this
C> ensures that the packed subset will always end on a full byte
C> boundary.
C>
C> @param[inout] IBAY - integer(*):
C>  - on input, contains BUFR data subset to be padded
C>  - on output, contains BUFR data subset padded with zeroed-out bits up to IPADB
C> @param[inout] IBIT - integer:
C>  - on input, contains bit pointer within IBAY after which to begin padding.
C>  - on output, contains bit pointer within IBAY to last bit that was padded.
C> @param[out] IBYT - integer: number of bytes within IBAY containing packed data,
C> including padding
C> @param[in] IPADB - integer: bit boundary to pad to (must be a multiple of 8).
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE PAD(IBAY,IBIT,IBYT,IPADB)

      CHARACTER*128 BORT_STR
      DIMENSION     IBAY(*)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  PAD THE SUBSET TO AN IPADB BIT BOUNDARY
C  ----------------------------------------

      IPAD = IPADB - MOD(IBIT+8,IPADB)
c  .... First pack the # of bits being padded (this is a delayed
c  .... replication factor)
      CALL PKB(IPAD,8,IBAY,IBIT)
c  .... Now pad with zeroes to the byte boundary
      CALL PKB(0,IPAD,IBAY,IBIT)
      IBYT = IBIT/8

      IF(MOD(IBIT,8).NE.0) GOTO 901

C  EXITS
C  -----

      RETURN
901   WRITE(BORT_STR,'("BUFRLIB: PAD - THE NUMBER OF BITS IN A PACKED'//
     . ' SUBSET AFTER PADDING (",I8,") IS NOT A MULTIPLE OF 8")') IBIT
      CALL BORT(BORT_STR)
      END
