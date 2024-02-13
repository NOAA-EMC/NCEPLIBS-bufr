C> @file
C> @brief Read a data value from Section 3 of a BUFR message.
C>
C> @author J. Ator @date 2009-03-23

C> Return a specified value from within Section 3 of a BUFR message.
C>
C> @remarks
C> The start of the BUFR message (i.e. the string 'BUFR') must be
C> aligned on the first 4 bytes of MBAY.
C>
C> @param[in] MBAY - integer(*): BUFR message
C> @param[in] S3MNEM - character*(*): Value to be read from
C> Section 3 of MBAY
C> - 'NSUB'  = Number of data subsets
C> - 'IOBS'  = Flag indicating whether the message contains observed data:
C>   - 0 = No
C>   - 1 = Yes
C> - 'ICMP'  = Flag indicating whether the message contains compressed data:
C>   - 0 = No
C>   - 1 = Yes
C>
C> @returns iupbs3 - integer: Value corresponding to S3MNEM
C> - -1 = S3MNEM was invalid
C>
C> @author J. Ator @date 2009-03-23

        RECURSIVE FUNCTION IUPBS3(MBAY,S3MNEM) RESULT(IRET)

        use modv_vars, only: im8b

        DIMENSION       MBAY(*)

        CHARACTER*(*)   S3MNEM

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C       Check for I8 integers.

        IF(IM8B) THEN
           IM8B=.FALSE.

           IRET = IUPBS3(MBAY,S3MNEM)

           IM8B=.TRUE.
           RETURN
        ENDIF

C       Skip to the beginning of Section 3.

        CALL GETLENS(MBAY,3,LEN0,LEN1,LEN2,LEN3,L4,L5)
        IPT = LEN0 + LEN1 + LEN2

C       Unpack the requested value.

        IF(S3MNEM.EQ.'NSUB') THEN
           IRET = IUPB(MBAY,IPT+5,16)
        ELSE IF( (S3MNEM.EQ.'IOBS') .OR. (S3MNEM.EQ.'ICMP') ) THEN
           IVAL = IUPB(MBAY,IPT+7,8)
           IF(S3MNEM.EQ.'IOBS') THEN
              IMASK = 128
           ELSE
              IMASK = 64
           ENDIF
           IRET = MIN(1,IAND(IVAL,IMASK))
        ELSE
           IRET = -1
        ENDIF

        RETURN
        END
