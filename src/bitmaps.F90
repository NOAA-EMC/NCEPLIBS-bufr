!> @file
!> @brief Process bitmaps within BUFR messages
!>
!> @author J Ator @date 2016-05-27

!> Store internal information in module @ref moda_bitmaps if the input element is part of a bitmap.
!>
!> This subroutine first determines whether the input element is part of a bitmap.  If so, then information about the
!> element is stored internally for later use.
!>
!> @param n - Subset element
!> @param lun - File ID
!> @param ival - Value associated with n:
!>   - If n is determined to be part of a bitmap, then a value of 0 means that n is a "set" entry in the bitmap, and
!>     any other (i.e. non-zero) value means that n is not a "set" entry in the bitmap
!>   - If n is determined to not be part of a bitmap, then this value is ignored
!>
!> @author J. Ator @date 2016-05-27
subroutine strbtm ( n, lun, ival )

  use modv_vars, only: mxbtm, mxbtmse

  use moda_msgcwd
  use moda_usrint
  use moda_tables
  use moda_bitmaps

  implicit none

  integer, intent(in) :: n, lun, ival
  integer node, nodtam, ii, jj, lstjpb

  logical isbtme

  node = inv( n, lun )

  if ( tag(node)(1:5) == 'DPRI ' ) then
    ! Confirm that this is really an entry within a bitmap.  Although it's rare, it is possible for a DPRI element
    ! to appear in a subset definition outside of a bitmap.
    isbtme = .false.
    if ( ntamc > 0 ) then
      nodtam = lstjpb( node, lun, 'SUB' )
      do ii = 1, ntamc
        if ( nodtam == inodtamc(ii) ) then
          do jj = 1, ntco(ii)
            if ( ( inodtco(ii,jj) >= inode(lun) ) .and. ( inodtco(ii,jj) <= isc(inode(lun)) ) .and. &
              ( inodtco(ii,jj) < node ) ) then
              if ( ctco(ii,jj) == '236000' ) then
                isbtme = .true.
              else if ( ( ctco(ii,jj) == '235000' ) .or. ( ctco(ii,jj) == '237255' ) ) then
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
      if ( nbtm >= mxbtm ) call bort('BUFRLIB: STRBTM - MXBTM OVERFLOW')
      nbtm = nbtm + 1
      istbtm(nbtm) = n
      iszbtm(nbtm) = 0
      nbtmse(nbtm) = 0
      linbtm = .true.
    end if
    iszbtm(nbtm) = iszbtm(nbtm) + 1
    if ( ival == 0 ) then
      ! This is a "set" (value=0) entry in the bitmap.
      if ( nbtmse(nbtm) >= mxbtmse ) call bort('BUFRLIB: STRBTM - MXBTMSE OVERFLOW')
      nbtmse(nbtm) = nbtmse(nbtm) + 1
      ibtmse(nbtm,nbtmse(nbtm)) = iszbtm(nbtm)
    end if
  else if ( itp(node) > 1 ) then
    linbtm = .false.
  end if

  return
end subroutine strbtm

!> Check whether a specified Table B mnemonic references another Table B mnemonic within the same data
!> subset via an internal bitmap.
!>
!> If so, then the referenced mnemonic is returned along with its location within the subset.
!>
!> @param lunit - Fortran logical unit number for BUFR file
!> @param tagi - Table B mnemonic
!> @param ntagi - Ordinal occurrence of tagi for which tagre is to be returned, counting from the
!> beginning of the overall subset definition
!> @param tagre - Table B mnemonic referenced by tagi via an internal bitmap
!> @param ntagre - Ordinal occurrence of tagre referenced by (ntagi)th occurrence of tagi, counting from
!> the beginning of the overall subset definition
!> @param iret - Return code
!>    - 0 = normal return
!>    - -1 = tagre could not be found, or some other error occurred
!>
!> A data subset must already be in scope within the NCEPLIBS-bufr
!> internal arrays for lunit, either via a previous call to one
!> of the [subset-reading subroutines](@ref hierarchy)
!> (when reading BUFR data subsets) or via a previous call to one
!> of the [message-writing subroutines](@ref hierarchy)
!> (when writing BUFR data subsets).
!>
!> @author J. Ator @date 2016-06-07
recursive subroutine gettagre ( lunit, tagi, ntagi, tagre, ntagre, iret )

  use modv_vars, only: im8b

  use moda_usrint
  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lunit, ntagi
  integer, intent(out) :: iret, ntagre
  integer my_lunit, my_ntagi, lun, il, im, ni, nre, ltre, ii

  character*(*), intent(in) :: tagi
  character*(*), intent(out) :: tagre
  character*10 tagtmp

  ! Check for I8 integers.

  if(im8b) then
    im8b=.false.
    call x84(lunit,my_lunit,1)
    call x84(ntagi,my_ntagi,1)
    call gettagre(my_lunit,tagi,my_ntagi,tagre,ntagre,iret)
    call x48(ntagre,ntagre,1)
    call x48(iret,iret,1)
    im8b=.true.
    return
  endif

  iret = -1

  ! Get lun from lunit.

  call status( lunit, lun, il, im )
  if ( il == 0 ) return
  if ( inode(lun) /= inv(1,lun) ) return

  ! Get tagre and ntagre from the (ntagi)th occurrence of tagi.

  call fstag( lun, tagi, ntagi, 1, ni, iret )
  if ( iret /= 0 ) return
  nre = nrfelm(ni,lun)
  if ( nre > 0 ) then
    iret = 0
    tagre = tag(inv(nre,lun))
    call strsuc( tagre, tagtmp, ltre )
    ntagre = 0
    do ii = 1, nre
      if ( tag(inv(ii,lun))(1:ltre) == tagre(1:ltre) ) then
        ntagre = ntagre + 1
      end if
    end do
  end if

  return
end subroutine gettagre

!> Check whether a subset element refers to a previous element within the same subset via an internal bitmap.
!>
!> If so, then the referenced element is returned. In addition, if
!> the input element is a 2-XX-255 marker operator, then its scale
!> factor, bit width and reference values are set internally to match those
!> of the referenced element.
!>
!> @param n - Subset element
!> @param lun - File ID
!>
!> @return Subset element referenced by element n within the same subset
!> - 0 = Input element does not refer to a previous element, or referenced element not found
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

  if ( itp(node) > 1 ) then
    if ( node == lstnod ) then
      lstnodct = lstnodct + 1
    else
      lstnod = node
      lstnodct = 1
    end if
    ! Does this subset definition contain any Table C operators with an X value of 21 or greater?
    idxta = 0
    if ( ntamc > 0 ) then
      nodtam = lstjpb( node, lun, 'SUB' )
      do ii = 1, ntamc
        if ( nodtam == inodtamc(ii) ) then
          idxta = ii
          ntc = ntco(ii)
        end if
      end do
    end if
    if ( ( idxta > 0 ) .and. ( nbtm > 0 ) ) then
      ! Check whether this element references a previous element in the same subset via an internal bitmap.  To do this,
      ! we first need to determine the appropriate "follow" operator (if any) corresponding to this element.
      cflwopr = 'XXXXXX'
      if ( imrkopr(tag(node)) == 1 ) then
        cflwopr = tag(node)(1:3) // '000'
      else
        call nemtab( lun, tag(node), idn, tab, nn )
        if ( tab == 'B' ) then
          fxy = adn30(idn,6)
          if ( fxy(2:3) == '33' ) cflwopr = '222000'
        end if
      end if
      if ( cflwopr == 'XXXXXX' ) return
      ! Now, check whether the appropriate "follow" operator was  actually present in the subset.  If there are multiple
      ! occurrences, we want the one that most recently precedes the element in question.
      nodflw = 0
      do jj = 1, ntc
        if ( ( ctco(idxta,jj) == cflwopr ) .and. ( inodtco(idxta,jj) >= inode(lun) ) .and. &
          ( inodtco(idxta,jj) <= isc(inode(lun)) ) .and. ( inodtco(idxta,jj) < node ) )  nodflw = inodtco(idxta,jj)
      enddo
      if ( nodflw == 0 ) then
        if ( imrkopr(tag(node)) == 1 ) then
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
      do while ( ( jj <= ntc ) .and. ( inodtco(idxta,jj) >= inode(lun) ) .and. &
        ( inodtco(idxta,jj) <= isc(inode(lun)) ) .and. ( nodbmap == 0 ) )
        if ( ctco(idxta,jj) == '236000' ) then
          nodl236 = inodtco(idxta,jj)
          if ( inodtco(idxta,jj) == nodflw ) nodbmap = nodflw
        else if ( ( ctco(idxta,jj) == '235000' ) .or. ( ctco(idxta,jj) == '237255' ) ) then
          nodl236 = 0
        else if ( ( ctco(idxta,jj) == '237000' ) .and. ( inodtco(idxta,jj) == nodflw ) .and. ( nodl236 /= 0 ) ) then
          nodbmap = nodl236
        end if
        jj = jj + 1
      end do
      if ( nodbmap == 0 ) then
        ! There was no valid bitmap indicator, so we'll just look for a bitmap after the "follow" indicator.
        nodbmap = nodflw
      end if
      ! Find the corresponding bitmap.
      nn = 1
      idxbtm = 0
      do while ( ( idxbtm == 0 ) .and. ( nn <= nval(lun) ) )
        if ( inv( nn, lun ) > nodbmap ) then
          ii = 1
          do while ( ( idxbtm == 0 ) .and. ( ii <= nbtm ) )
            if ( nn == istbtm(ii) ) then
              idxbtm = ii
            else
              ii = ii + 1
            end if
          end do
        end if
        nn = nn + 1
      end do
      if ( idxbtm == 0 ) then
        if ( imrkopr(tag(node)) == 1 ) then
          write(bort_str,'("BUFRLB: IGETRFEL - UNABLE TO FIND BITMAP FOR MARKER OPERATOR ",A)') tag(node)
          call bort(bort_str)
        endif
        return
      end if
      ! Use the bitmap to find the previous element in the subset that is referenced by the current element.
      ! Search backwards from the start of the bitmap, but make sure not to cross a 2-35-000 operator.
      if ( lstnodct > nbtmse(idxbtm) ) then
        if ( imrkopr(tag(node)) == 1 ) call bort( bort_str_mrkopr // tag(node) )
        return
      end if
      iemrk = iszbtm(idxbtm) - ibtmse(idxbtm,lstnodct) + 1
      iect = 0
      do while ( ( nn >= 1 ) .and. ( iret == 0 ) )
        nodnn = inv( nn, lun )
        if ( nodnn <= nodbmap ) then
          do jj = 1, ntc
            if ( ( nodnn == inodtco(idxta,jj) ) .and. ( ctco(idxta,jj) == '235000' ) ) then
              if ( imrkopr(tag(node)) == 1 ) call bort( bort_str_mrkopr // tag(node) )
              return
            end if
          end do
          if ( itp(nodnn) > 1 ) then
            iect = iect + 1
            if ( iect == iemrk ) iret = nn
          end if
        end if
        nn = nn - 1
      end do
      if ( iret == 0 ) then
        if ( imrkopr(tag(node)) == 1 ) call bort( bort_str_mrkopr // tag(node) )
        return
      end if
      if ( imrkopr(tag(node)) == 1 ) then
        ! This element is a marker operator, so set the scale, reference value and bit width accordingly based on
        ! those of the previous referenced element.
        nodrfe = inv( iret, lun )
        isc(node) = isc(nodrfe)
        if ( tag(node)(1:3) == '225' ) then
          ibt(node) = ibt(nodrfe) + 1
          irf(node) = -1 * (2 ** ibt(nodrfe))
        else
          ibt(node) = ibt(nodrfe)
          irf(node) = irf(nodrfe)
          if ( nnrv > 0 ) then
            do ii = 1, nnrv
              if ( ( nodrfe /= inodnrv(ii) ) .and. ( tag(nodrfe)(1:8) == tagnrv(ii) ) .and. &
                ( nodrfe >= isnrv(ii) ) .and. ( nodrfe <= ienrv(ii) ) ) then
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

  if (len(nemo)<6) then
    iret = 0
  else if ( ( nemo(4:6)=='255' ) .and. &
    ( ( nemo(1:3)=='223' ) .or. ( nemo(1:3)=='224' ) .or. ( nemo(1:3)=='225' ) .or. ( nemo(1:3)=='232' ) ) ) then
    iret = 1
  else
    iret = 0
  endif

  return
end function imrkopr
