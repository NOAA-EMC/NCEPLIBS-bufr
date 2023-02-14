C> @file
C> @brief Copy a BUFR data subset.
C>
C> @author J. Woollen @date 1994-01-06

C> This subroutine copies a BUFR data subset from one Fortran logical
C> unit to another.
C>
C> It is similar to subroutine copysb(), except that here a
C> BUFR data subset should have already been read into internal arrays
C> for logical unit LUBIN via a previous call to one of the
C> [subset-reading subroutines](@ref hierarchy), whereas copysb()
C> only requires that a BUFR message should have already been read
C> into internal arrays via a previous call to one of the
C> [message-reading subroutines](@ref hierarchy).
C>
C> For logical unit LUBOT, a BUFR message should already be open
C> for output within internal arrays via a previous call to one of
C> the [message-writing subroutines](@ref hierarchy).
C>
C> The [DX BUFR Table information](@ref dfbftab) associated with
C> each of the logical units LUBIN and LUBOT must contain identical
C> definitions for the data subset to be copied.
C>
C> @param[in] LUBIN   -- integer: Fortran logical unit number for
C>                       source BUFR file
C> @param[in] LUBOT   -- integer: Fortran logical unit number for
C>                       target BUFR file
C>
C> @author J. Woollen @date 1994-01-06
      RECURSIVE SUBROUTINE UFBCPY(LUBIN,LUBOT)

      USE MODV_IM8B

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_UFBCPL
      USE MODA_TABLES

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C  CHECK FOR I8 INTEGERS
C  ---------------------

      IF(IM8B) THEN
         IM8B=.FALSE.

         CALL X84(LUBIN,MY_LUBIN,1)
         CALL X84(LUBOT,MY_LUBOT,1)
         CALL UFBCPY(MY_LUBIN,MY_LUBOT)

         IM8B=.TRUE.
         RETURN
      ENDIF

C  CHECK THE FILE STATUSES AND I-NODE
C  ----------------------------------

      CALL STATUS(LUBIN,LUI,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902
      IF(INODE(LUI).NE.INV(1,LUI)) GOTO 903

      CALL STATUS(LUBOT,LUO,IL,IM)
      IF(IL.EQ.0) GOTO 904
      IF(IL.LT.0) GOTO 905
      IF(IM.EQ.0) GOTO 906

      IF(INODE(LUI).NE.INODE(LUO)) THEN
        IF( (TAG(INODE(LUI)).NE.TAG(INODE(LUO))) .OR.
     .     (IOK2CPY(LUI,LUO).NE.1) ) GOTO 907
      ENDIF

C  EVERYTHING OKAY COPY USER ARRAY FROM LUI TO LUO
C  -----------------------------------------------

      NVAL(LUO) = NVAL(LUI)

      DO N=1,NVAL(LUI)
      INV(N,LUO) = INV(N,LUI)
      NRFELM(N,LUO) = NRFELM(N,LUI)
      VAL(N,LUO) = VAL(N,LUI)
      ENDDO

      LUNCPY(LUO)=LUBIN

C  EXITS
C  -----

      RETURN
900   CALL BORT('BUFRLIB: UFBCPY - INPUT BUFR FILE IS CLOSED, IT MUST'//
     . ' BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: UFBCPY - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: UFBCPY - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: UFBCPY - LOCATION OF INTERNAL TABLE FOR '//
     . 'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//
     . 'INTERNAL SUBSET ARRAY')
904   CALL BORT('BUFRLIB: UFBCPY - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
905   CALL BORT('BUFRLIB: UFBCPY - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
906   CALL BORT('BUFRLIB: UFBCPY - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
907   CALL BORT('BUFRLIB: UFBCPY - INPUT AND OUTPUT BUFR FILES MUST '//
     . 'HAVE THE SAME INTERNAL TABLES, THEY ARE DIFFERENT HERE')
      END
