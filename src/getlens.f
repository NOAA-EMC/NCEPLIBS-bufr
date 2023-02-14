C> @file
C> @brief Read the section lengths of a BUFR message.
C>
C> @author J. Ator @date 2005-11-29

C> This subroutine reads the lengths of all of the individual
C> sections of a given BUFR message, up to a specified point in
C> the message.
C>
C> <p>This subroutine will work on any BUFR message encoded using
C> BUFR edition 2, 3, or 4
C>
C> @param[in]  MBAY -- integer(*): BUFR message
C> @param[in]   LL  -- integer: Number of last section for
C>                     which the length is to be read.
C>                     In other words, setting LL = N means to
C>                     read and return the lengths of Sections 0
C>                     through N (i.e. LEN0, LEN1,...,LENN).
C>                     Any section lengths that are not specified
C>                     to be read are returned with a default
C>                     placeholder value of -1.
C> @param[out]  LEN0 -- integer: Length (in bytes) of Section 0
C> @param[out]  LEN1 -- integer: Length (in bytes) of Section 1
C> @param[out]  LEN2 -- integer: Length (in bytes) of Section 2
C> @param[out]  LEN3 -- integer: Length (in bytes) of Section 3
C> @param[out]  LEN4 -- integer: Length (in bytes) of Section 4
C> @param[out]  LEN5 -- integer: Length (in bytes) of Section 5
C>
C> @remarks
C> - The start of the BUFR message (i.e. the string 'BUFR') must be
C>   aligned on the first 4 bytes of MBAY.
C>
C> @author J. Ator @date 2005-11-29
        RECURSIVE SUBROUTINE GETLENS
     .          (MBAY,LL,LEN0,LEN1,LEN2,LEN3,LEN4,LEN5)

        USE MODV_IM8B

        DIMENSION   MBAY(*)

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
           IM8B=.FALSE.

           CALL X84(LL,MY_LL,1)
           CALL GETLENS(MBAY,MY_LL,LEN0,LEN1,LEN2,LEN3,LEN4,LEN5)
           CALL X48(LEN0,LEN0,1)
           CALL X48(LEN1,LEN1,1)
           CALL X48(LEN2,LEN2,1)
           CALL X48(LEN3,LEN3,1)
           CALL X48(LEN4,LEN4,1)
           CALL X48(LEN5,LEN5,1)

           IM8B=.TRUE.
           RETURN
        ENDIF

        LEN0 = -1
        LEN1 = -1
        LEN2 = -1
        LEN3 = -1
        LEN4 = -1
        LEN5 = -1

        IF(LL.LT.0) RETURN
        LEN0 = IUPBS01(MBAY,'LEN0')

        IF(LL.LT.1) RETURN
        LEN1 = IUPBS01(MBAY,'LEN1')

        IF(LL.LT.2) RETURN
        IAD2 = LEN0 + LEN1
        LEN2 = IUPB(MBAY,IAD2+1,24) * IUPBS01(MBAY,'ISC2')

        IF(LL.LT.3) RETURN
        IAD3 = IAD2 + LEN2
        LEN3 = IUPB(MBAY,IAD3+1,24)

        IF(LL.LT.4) RETURN
        IAD4 = IAD3 + LEN3
        LEN4 = IUPB(MBAY,IAD4+1,24)

        IF(LL.LT.5) RETURN
        LEN5 = 4

        RETURN
        END
