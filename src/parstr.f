C> @file
C> @brief Parse a string containing one or more
C> substrings into an array of substrings.      
C> @author J @date 2007-01-19
      
C> This subroutine parses a string containing one or more
C> substrings into an array of substrings. The separator for the
C> substrings is specified during input, and multiple adjacent
C> occurrences of this character will be treated as a single
C> occurrence when the string is actually parsed.
C>
C> @param[in] STR - character*(*): string.
C> @param[out] TAGS - character*(*): mtag-word array of substrings (first.
C> ntag words filled)
C> @param[in] MTAG - integer: maximum number of substrings to be parsed from string.
C> @param[out] NTAG - integer: number of substrings returned.
C> @param[in] SEP - character*1: separator character for substrings.
C> @param[in] LIMIT80 - logical: .true. if an abort should occur when str is
C> longer than 80 characters; included for historical consistency with old subroutine parseq.
C>
C> @author J @date 2007-01-19
      SUBROUTINE PARSTR(STR,TAGS,MTAG,NTAG,SEP,LIMIT80)



      CHARACTER*(*) STR,TAGS(MTAG)
      CHARACTER*128 BORT_STR1,BORT_STR2
      CHARACTER*1   SEP
      LOGICAL       SUBSTR,LIMIT80

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      LSTR = LEN(STR)
      LTAG = LEN(TAGS(1))
      IF( LIMIT80 .AND. (LSTR.GT.80) ) GOTO 900
      NTAG = 0
      NCHR = 0
      SUBSTR = .FALSE.

      DO I=1,LSTR

      IF( .NOT.SUBSTR .AND. (STR(I:I).NE.SEP) ) THEN
         NTAG = NTAG+1
         IF(NTAG.GT.MTAG) GOTO 901
         TAGS(NTAG) = ' '
      ENDIF

      IF( SUBSTR .AND. (STR(I:I).EQ.SEP) ) NCHR = 0
      SUBSTR = STR(I:I).NE.SEP

      IF(SUBSTR) THEN
         NCHR = NCHR+1
         IF(NCHR.GT.LTAG) GOTO 902
         TAGS(NTAG)(NCHR:NCHR) = STR(I:I)
      ENDIF

      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR1,'("BUFRLIB: PARSTR - INPUT STRING (",A,") HAS ")')
     . STR
      WRITE(BORT_STR2,'(18X,"LENGTH (",I4,"), > LIMIT OF 80 CHAR.")')
     . LSTR
      CALL BORT2(BORT_STR1,BORT_STR2)
901   WRITE(BORT_STR1,'("BUFRLIB: PARSTR - INPUT STRING (",A,") '//
     . 'CONTAINS",I4)') STR,NTAG
      WRITE(BORT_STR2,'(18X,"SUBSTRINGS, EXCEEDING THE LIMIT {",I4,'//
     . '" - THIRD (INPUT) ARGUMENT}")') MTAG
      CALL BORT2(BORT_STR1,BORT_STR2)
902   WRITE(BORT_STR1,'("BUFRLIB: PARSTR - INPUT STRING (",A,") ")') STR
      WRITE(BORT_STR2,'(18X,"CONTAINS A PARSED SUBSTRING WITH LENGTH '//
     . 'EXCEEDING THE MAXIMUM OF",I4," CHARACTERS")') LTAG
      CALL BORT2(BORT_STR1,BORT_STR2)
      END
