C> @file
C> @brief Reset the string cache.
C>
C> @author J. Woollen @date 1994-01-06

C> Reset the internal mnemonic string cache.
C> The string cache is a performance-enhancing feature which saves
C> processing time when the same mnemonics are encountered repeatedly.
C>
C> @author J. Woollen @date 1994-01-06

      SUBROUTINE STRCLN

      use modv_vars, only: mxs

      COMMON /STCACH/ MSTR,NSTR,LSTR,LUNS(MXS,2),USRS(MXS),ICON(52,MXS)
      CHARACTER*80 USRS

      MSTR = MXS
      NSTR = 0
      LSTR = 0
      RETURN
      END
