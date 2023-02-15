C> @file
C> @brief Search for a specified mnemonic within unexpanded sequences
C> of the internal jump/link table.
C> @author Woollen @date 1994-01-06

C> This subroutine is called by subroutine
C> ufbrw() whenever it can't find a mnemonic it wants to write within the
C> current subset buffer. It looks for the mnemonic within any
C> unexpanded "drs" (stack) or "drb" (1-bit delayed replication)
C> sequences inside of the portion of the subset buffer bounded by the
C> indices inv1 and inv2. If found, it expands the applicable "drs" or
C> "drb" sequence to the point where the mnemonic in question now
C> appears in the subset buffer, and in doing so it will also return
C> a new value for inv2.
C>
C> @param[in] INOD - integer: jump/link table index of mnemonic to look for.
C> @param[in] LUN - integer: I/O stream index into internal memory arrays.
C> @param[in] INV1 - integer: starting index of the portion of the subset
C> buffer currently being processed by ufbrw().
C> @param[inout] INV2 - integer:
C>  - on input, ending index of the portion of the subset buffer currently
C>    being processed by ufbrw().
C>  - on output, if invn = 0 then inv2 is unchanged from its input value.
C>    Otherwise, it contains the redefined ending index of the portion of the subset
C>    buffer currently being processed by ufbrw(), since expanding a delayed
C>    replication sequence will have necessarily increased the size of this buffer.
C> @param[out] INVN - integer: location index of inod within subset buffer.
C>  - 0 not found.
C>
C> @author Woollen @date 1994-01-06
      SUBROUTINE DRSTPL(INOD,LUN,INV1,INV2,INVN)

      USE MODA_TABLES

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

1     NODE = INOD
2     NODE = JMPB(NODE)
      IF(NODE.EQ.0) GOTO 100
      IF(TYP(NODE).EQ.'DRS' .OR. TYP(NODE).EQ.'DRB') THEN
         INVN = INVWIN(NODE,LUN,INV1,INV2)
         IF(INVN.GT.0) THEN
            CALL USRTPL(LUN,INVN,1)
            CALL NEWWIN(LUN,INV1,INV2)
            INVN = INVWIN(INOD,LUN,INVN,INV2)
            IF(INVN.GT.0) GOTO 100
            GOTO 1
         ENDIF
      ENDIF
      GOTO 2

C  EXIT
C  ----

100   RETURN
      END
