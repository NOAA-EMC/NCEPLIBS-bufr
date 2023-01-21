C> @file
C>
C> ### Program History Log
C> Date | Programmer | Comments
C> -----|------------|----------
C> 1994-01-06 | J. Woollen | Original author.
C> 1998-07-08 | J. Woollen | Replaced cray routine "abort" with bort().
C> 2003-11-04 | S. Bender  | Added remarks/bufrlib routine interdependencies.
C> 2003-11-04 | D. Keyser  | Unified/portable for wrf; documentation; outputs more info.
C>
C> @author Woollen @date 1994-01-06
      
C> This subroutine first packs the value for the number of
C> bits being "padded" (we'll get to that later), starting with bit
C> ibit+1 and using eight bits in the packed array ibay (which
C> represents a subset packed into ibit bits). Then, starting with
C> ibit+9, it packs zeroes (i.e., "pads") to the specified bit
C> boundary (ipadb). (Note: it's the number of bits padded here that
C> was packed in bits ibit+1 through ibit+8 - this is actually a
C> delayed replication factor). IPADB must be a multiple of eight and
C> represents the bit boundary on which the packed subset in ibay
C> should end after padding. For example, if ipabd is "8", then the
C> number of bits in ibay actually consumed by packed data (including
C> the padding) will be a multiple of eight. If ipadb is "16", it
C> will be a multiple of sixteen.  in either (or any) case, this
C> ensures that the packed subset will always end on a full byte
C> boundary.
C>
C> @param[inout] IBAY - integer: *-word packed binary array not yet padded.
C> Out: *-word packed binary array now padded.
C> @param[inout] IBIT - integer: bit pointer within ibay to start padding from.
C> Out: number of bits within ibay containing packed data (including padding, must be a multiple of 8).
C> @param[out] IBYT - integer: number of bytes within ibay containing packed data
C> (including padding) (i.e., ibit/8).
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

      IF(MOD(IBIT,IPADB).NE.0) GOTO 900
      IF(MOD(IBIT,8    ).NE.0) GOTO 901

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: PAD - THE INPUT BIT BOUNDARY TO PAD '//
     . 'TO (",I8,") IS NOT A MULTIPLE OF 8")') IPADB
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: PAD - THE NUMBER OF BITS IN A PACKED'//
     . ' SUBSET AFTER PADDING (",I8,") IS NOT A MULTIPLE OF 8")') IBIT
      CALL BORT(BORT_STR)
      END
