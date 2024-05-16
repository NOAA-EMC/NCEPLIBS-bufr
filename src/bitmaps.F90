!> @file
!> @brief Process bitmaps within BUFR messages
!>
!> @author J Ator @date 2016-05-27

!> Store internal information in module @ref moda_bitmaps if the input element is part of a bitmap.
!>
!> @param n - Subset element
!> @param lun - File ID
!>
!> @author J. Ator @date 2016-05-27
subroutine strbtm ( n, lun )

  use modv_vars, only: mxbtm, mxbtmse

  use moda_msgcwd
  use moda_usrint
  use moda_tables
  use moda_bitmaps

  implicit none

  integer, intent(in) :: n, lun
  integer node, nodtam, ii, jj, ibfms, lstjpb

  logical isbtme

  node = inv( n, lun )

  if ( tag(node)(1:5) .eq. 'DPRI ' ) then
    ! Confirm that this is really an entry within a bitmap.  Although it's rare, it is possible for a DPRI element
    ! to appear in a subset definition outside of a bitmap.
    isbtme = .false.
    if ( ntamc .gt. 0 ) then
      nodtam = lstjpb( node, lun, 'SUB' )
      do ii = 1, ntamc
        if ( nodtam .eq. inodtamc(ii) ) then
          do jj = 1, ntco(ii)
            if ( ( inodtco(ii,jj) .ge. inode(lun) ) .and. ( inodtco(ii,jj) .le. isc(inode(lun)) ) .and. &
              ( inodtco(ii,jj) .lt. node ) ) then
              if ( ctco(ii,jj) .eq. '236000' ) then
                isbtme = .true.
              else if ( ( ctco(ii,jj) .eq. '235000' ) .or. ( ctco(ii,jj) .eq. '237255' ) ) then
                isbtme = .false.
              end if
            end if
          end do
        end if
      end do
    end if
    if ( .not. isbtme ) then
      linbtm = .false.
      return
    endif
    if ( .not. linbtm ) then
      ! This is the start of a new bitmap.
      if ( nbtm .ge. mxbtm ) call bort('BUFRLIB: STRBTM - MXBTM OVERFLOW')
      nbtm = nbtm + 1
      istbtm(nbtm) = n
      iszbtm(nbtm) = 0
      nbtmse(nbtm) = 0
      linbtm = .true.
    end if
    iszbtm(nbtm) = iszbtm(nbtm) + 1
    if ( ibfms(val(n,lun)) .eq. 0 ) then
      ! This is a "set" (value=0) entry in the bitmap.
      if ( nbtmse(nbtm) .ge. mxbtmse ) call bort('BUFRLIB: STRBTM - MXBTMSE OVERFLOW')
      nbtmse(nbtm) = nbtmse(nbtm) + 1
      ibtmse(nbtm,nbtmse(nbtm)) = iszbtm(nbtm)
    end if
  else if ( itp(node) .gt. 1 ) then
    linbtm = .false.
  end if

  return
end subroutine strbtm

!> Check whether a subset element refers to a previous element within the same subset via an internal bitmap.
!>
!> If so, then return the referenced element. In addition, if
!> the input element is a 2-XX-255 marker operator, then set its scale
!> factor, bit width and reference values internally to match those
!> of the referenced element.
!>
!> @param n - Subset element
!> @param lun - File ID
!>
!> @return Subset element referenced by element n within the same subset
!> - 0 = input element does not refer to a previous element, or referenced element not found
!>
!> @author J. Ator @date 2016-05-27
integer function igetrfel ( n, lun ) result ( iret )

  use moda_msgcwd
  use moda_usrint
  use moda_tables
  use moda_bitmaps
  use moda_nrv203

  implicit none

  integer, intent(in) :: n, lun
  integer node, ii, jj, nn, idxta, idn, ntc, nodflw, nodl236, nodbmap, nodrfe, nodnn, nodtam, idxbtm, iemrk, iect, &
    lstjpb, imrkopr

  character*(*), parameter :: bort_str_mrkopr = &
    'BUFRLIB: IGETRFEL - UNABLE TO FIND PREVIOUS ELEMENT REFERENCED BY MARKER OPERATOR '
  character*128 bort_str
  character*6 cflwopr, adn30, fxy
  character*1 tab

  iret = 0

  node = inv( n, lun )

  if ( itp(node) .gt. 1 ) then
    if ( node .eq. lstnod ) then
      lstnodct = lstnodct + 1
    else
      lstnod = node
      lstnodct = 1
    end if
    ! Does this subset definition contain any Table C operators with an X value of 21 or greater?
    idxta = 0
    if ( ntamc .gt. 0 ) then
      nodtam = lstjpb( node, lun, 'SUB' )
      do ii = 1, ntamc
        if ( nodtam .eq. inodtamc(ii) ) then
          idxta = ii
          ntc = ntco(ii)
        end if
      end do
    end if
    if ( ( idxta .gt. 0 ) .and. ( nbtm .gt. 0 ) ) then
      ! Check whether this element references a previous element in the same subset via an internal bitmap.  To do this,
      ! we first need to determine the appropriate "follow" operator (if any) corresponding to this element.
      cflwopr = 'XXXXXX'
      if ( imrkopr(tag(node)) .eq. 1 ) then
        cflwopr = tag(node)(1:3) // '000'
      else
        call nemtab( lun, tag(node), idn, tab, nn )
        if ( tab .eq. 'B' ) then
          fxy = adn30(idn,6)
          if ( fxy(2:3) .eq. '33' ) cflwopr = '222000'
        end if
      end if
      if ( cflwopr .eq. 'XXXXXX' ) return
      ! Now, check whether the appropriate "follow" operator was  actually present in the subset.  If there are multiple
      ! occurrences, we want the one that most recently precedes the element in question.
      nodflw = 0
      do jj = 1, ntc
        if ( ( ctco(idxta,jj) .eq. cflwopr ) .and. ( inodtco(idxta,jj) .ge. inode(lun) ) .and. &
          ( inodtco(idxta,jj) .le. isc(inode(lun)) ) .and. ( inodtco(idxta,jj) .lt. node ) )  nodflw = inodtco(idxta,jj)
      enddo
      if ( nodflw .eq. 0 ) then
        if ( imrkopr(tag(node)) .eq. 1 ) then
          write(bort_str,'("BUFRLB: IGETRFEL - UNABLE TO FIND FOLLOW OPERATOR ",A," IN SUBSET")') cflwopr
          call bort(bort_str)
        endif
        return
      end if
      ! We found an appropriate corresponding "follow" operator, so now we need to look for a bitmap corresponding to
      ! this operator.  First, look for a bitmap indicator.
      nodl236 = 0
      nodbmap = 0
      jj = 1
      do while ( ( jj .le. ntc ) .and. ( inodtco(idxta,jj) .ge. inode(lun) ) .and. &
        ( inodtco(idxta,jj) .le. isc(inode(lun)) ) .and. ( nodbmap .eq. 0 ) )
        if ( ctco(idxta,jj) .eq. '236000' ) then
          nodl236 = inodtco(idxta,jj)
          if ( inodtco(idxta,jj) .eq. nodflw ) nodbmap = nodflw
        else if ( ( ctco(idxta,jj) .eq. '235000' ) .or. ( ctco(idxta,jj) .eq. '237255' ) ) then
          nodl236 = 0
        else if ( ( ctco(idxta,jj) .eq. '237000' ) .and. ( inodtco(idxta,jj) .eq. nodflw ) .and. ( nodl236 .ne. 0 ) ) then
          nodbmap = nodl236
        end if
        jj = jj + 1
      end do
      if ( nodbmap .eq. 0 ) then
        ! There was no valid bitmap indicator, so we'll just look for a bitmap after the "follow" indicator.
        nodbmap = nodflw
      end if
      ! Find the corresponding bitmap.
      nn = 1
      idxbtm = 0
      do while ( ( idxbtm .eq. 0 ) .and. ( nn .le. nval(lun) ) )
        if ( inv( nn, lun ) .gt. nodbmap ) then
          ii = 1
          do while ( ( idxbtm .eq. 0 ) .and. ( ii .le. nbtm ) )
            if ( nn .eq. istbtm(ii) ) then
              idxbtm = ii
            else
              ii = ii + 1
            end if
          end do
        end if
        nn = nn + 1
      end do
      if ( idxbtm .eq. 0 ) then
        if ( imrkopr(tag(node)) .eq. 1 ) then
          write(bort_str,'("BUFRLB: IGETRFEL - UNABLE TO FIND BITMAP FOR MARKER OPERATOR ",A)') tag(node)
          call bort(bort_str)
        endif
        return
      end if
      ! Use the bitmap to find the previous element in the subset that is referenced by the current element.
      ! Search backwards from the start of the bitmap, but make sure not to cross a 2-35-000 operator.
      if ( lstnodct .gt. nbtmse(idxbtm) ) then
        if ( imrkopr(tag(node)) .eq. 1 ) call bort( bort_str_mrkopr // tag(node) )
        return
      end if
      iemrk = iszbtm(idxbtm) - ibtmse(idxbtm,lstnodct) + 1
      iect = 0
      do while ( ( nn .ge. 1 ) .and. ( iret .eq. 0 ) )
        nodnn = inv( nn, lun )
        if ( nodnn .le. nodbmap ) then
          do jj = 1, ntc
            if ( ( nodnn .eq. inodtco(idxta,jj) ) .and. ( ctco(idxta,jj) .eq. '235000' ) ) then
              if ( imrkopr(tag(node)) .eq. 1 ) call bort( bort_str_mrkopr // tag(node) )
              return
            end if
          end do
          if ( itp(nodnn) .gt. 1 ) then
            iect = iect + 1
            if ( iect .eq. iemrk ) iret = nn
          end if
        end if
        nn = nn - 1
      end do
      if ( iret .eq. 0 ) then
        if ( imrkopr(tag(node)) .eq. 1 ) call bort( bort_str_mrkopr // tag(node) )
        return
      end if
      if ( imrkopr(tag(node)) .eq. 1 ) then
        ! This element is a marker operator, so set the scale, reference value and bit width accordingly based on
        ! those of the previous referenced element.
        nodrfe = inv( iret, lun )
        isc(node) = isc(nodrfe)
        if ( tag(node)(1:3) .eq. '225' ) then
          ibt(node) = ibt(nodrfe) + 1
          irf(node) = -1 * (2 ** ibt(nodrfe))
        else
          ibt(node) = ibt(nodrfe)
          irf(node) = irf(nodrfe)
          if ( nnrv .gt. 0 ) then
            do ii = 1, nnrv
              if ( ( nodrfe .ne. inodnrv(ii) ) .and. ( tag(nodrfe)(1:8) .eq. tagnrv(ii) ) .and. &
                ( nodrfe .ge. isnrv(ii) ) .and. ( nodrfe .le. ienrv(ii) ) ) then
                irf(node) = int(nrv(ii))
                return
              end if
            end do
          end if
        end if
      end if
    end if
  end if

  return
end function igetrfel

!> Check whether a specified mnemonic is a Table C marker operator.
!>
!> @param nemo - Mnemonic
!> @returns Flag indicating whether nemo is a Table C marker operator:
!> - 0 = No
!> - 1 = Yes
!>
!> @author J. Ator @date 2016-05-04
integer function imrkopr(nemo) result(iret)

  implicit none

  character*(*), intent(in) :: nemo

  if (len(nemo).lt.6) then
    iret = 0
  else if ( ( nemo(4:6).eq.'255' ) .and. &
    ( ( nemo(1:3).eq.'223' ) .or. ( nemo(1:3).eq.'224' ) .or. ( nemo(1:3).eq.'225' ) .or. ( nemo(1:3).eq.'232' ) ) ) then
    iret = 1
  else
    iret = 0
  endif

  return
end function imrkopr
