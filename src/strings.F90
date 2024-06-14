!> @file
!> @brief Process mnemonic strings
!>
!> @author Woollen @date 1994-01-06

!> Check whether a string is in the internal mnemonic string cache
!>
!> Check to see if a user-specified character string is already in the internal cache (arrays in common
!> blocks /stcach/ and /stords/).  If not, then call subroutine parusr() to perform the task of separating
!> and checking the individual mnemonics so that they can then be added to the cache.
!>
!> The mnemonic string cache is a performance enhancing device which saves
!> time when the same mnemonic strings are repeatedly encountered within
!> an application program.
!>
!> @param str - String of blank-separated mnemonics
!> @param lun - File ID
!> @param i1 - A number greater than or equal to the number of blank-separated mnemonics in str
!> @param io - Status indicator for BUFR file associated with lun:
!> - 0 = Input file
!> - 1 = Output file
!>
!> @author Woollen @date 1994-01-06
subroutine string(str,lun,i1,io)

  use modv_vars, only: mxs

  use moda_msgcwd

  implicit none

  character*(*), intent(in) :: str
  character*128 bort_str1, bort_str2
  character*80 usr, ust

  integer, intent(in) :: lun, i1, io
  integer, parameter :: jcons = 52
  integer mstr, nstr, lstr, lux, icon, jcon, iord, iorx, nxt, ind, j, n

  logical incache

  ! Note that lstr, mstr and nstr were initialized via a prior call to subroutine strcln(), which itself was called by
  ! subroutine makestab().
  common /stcach/ mstr, nstr, lstr, lux(mxs,2), usr(mxs), icon(jcons,mxs)
  common /usrstr/ jcon(jcons)
  common /stords/ iord(mxs), iorx(mxs)

  nxt = 0
  ust = str
  ind = inode(lun)
  if(len(str)>80) then
    write(bort_str1,'("BUFRLIB: STRING - INPUT STRING (",A,") HAS")') str
    write(bort_str2,'(18X,"LENGTH (",I4,"), > LIMIT OF 80 CHAR.")') len(str)
    call bort2(bort_str1,bort_str2)
  endif

  !  See if the string is already in the cache

  incache = .false.
  do n=1,nstr
    if(lux(iord(n),2)==ind) then
      iorx(nxt+1) = iord(n)
      nxt = nxt+1
    endif
  enddo
  do n=1,nxt
    if(ust==usr(iorx(n))) then

      ! Yes, so copy parameters from the cache

      incache = .true.
      do j=1,jcons
        jcon(j) = icon(j,iorx(n))
      enddo
      exit
    endif
  enddo
  if (.not.incache) then

    ! No, so add it to the cache

    call parusr(str,lun,i1,io)
    lstr = max(mod(lstr+1,mstr+1),1)
    nstr = min(nstr+1,mstr)
    lux(lstr,1) = lun
    lux(lstr,2) = ind
    usr(lstr) = str
    do j=1,jcons
      icon(j,lstr) = jcon(j)
    enddo

    ! Rearrange the cache order following this update

    do n=nstr,2,-1
      iord(n) = iord(n-1)
    enddo
    iord(1) = lstr
  endif

  if(jcon(1)>i1) then
    write(bort_str1,'("BUFRLIB: STRING - INPUT STRING (",A,")")') str
    write(bort_str2,'(18X,"HAS",I5," STORE NODES (MNEMONICS) - THE LIMIT (THIRD INPUT ARGUMENT) IS",I5)') jcon(1), i1
    call bort2(bort_str1,bort_str2)
  endif

  return
end subroutine string

!> Reset the internal mnemonic string cache
!>
!> The string cache is a performance-enhancing feature which saves
!> processing time when the same mnemonics are encountered repeatedly.
!>
!> @author J. Woollen @date 1994-01-06
subroutine strcln

  use modv_vars, only: mxs

  implicit none

  integer mstr, nstr, lstr, luns, icon

  character*80 usrs

  common /stcach/ mstr, nstr, lstr, luns(mxs,2), usrs(mxs), icon(52,mxs)

  mstr = mxs
  nstr = 0
  lstr = 0

  return
end subroutine strcln

!> Initiate the process to parse out mnemonics from a user-specified character string, then separate them
!> into store and condition nodes.
!>
!> Information about the string mnemonics is stored in arrays in common block /usrstr/. Condition nodes are
!> sorted in the order expected in the internal jump/link tables and several checks are performed on them.
!>
!> @param str - String of blank-separated mnemonics
!> @param lun - File ID
!> @param i1 - A number greater than or equal to the number of blank-separated mnemonics in str
!> @param iO - Status indicator for BUFR file associated with lun:
!> - 0 = Input file
!> - 1 = Output file
!>
!> @author Woollen @date 1994-01-06
subroutine parusr(str,lun,i1,io)

  use modv_vars, only: iac

  implicit none

  integer, intent(in) :: lun, i1, io
  integer, parameter :: maxusr = 30, maxnod = 20, maxcon = 10
  integer nnod, ncon, nods, nodc, ivls, kons, i, j, n, ntot, nod, kon, irpc, lstjpb

  character*(*), intent(in) :: str
  character*128 bort_str1, bort_str2
  character*80 ust
  character*20 utg(maxusr)

  real val

  logical bump

  common /usrstr/ nnod, ncon, nods(maxnod), nodc(maxcon), ivls(maxcon), kons(maxcon)

  ust  = str
  if(len(str)>80) then
    write(bort_str1,'("BUFRLIB: PARUSR - INPUT STRING (",A,") HAS ")') str
    write(bort_str2,'(18X,"LENGTH (",I4,"), > LIMIT OF 80 CHAR.")') len(str)
    call bort2(bort_str1,bort_str2)
  endif

  ncon = 0
  nnod = 0

  ! Parse the string

  call parstr(ust,utg,maxusr,ntot,' ',.true.)

  do n=1,ntot
    ! For each mnemonic, determine if it's a condition node or a store node
    call parutg(lun,io,utg(n),nod,kon,val)
    if(kon/=0) then
      ! It's a condition node
      ncon = ncon+1
      if(ncon>maxcon) then
        write(bort_str1,'("BUFRLIB: PARUSR - THE NUMBER OF CONDITION NODES IN INPUT STRING")')
        write(bort_str2,'(18X,A,") EXCEEDS THE MAXIMUM (",I3,")")') str,maxcon
        call bort2(bort_str1,bort_str2)
      endif
      nodc(ncon) = nod
      kons(ncon) = kon
      ivls(ncon) = nint(val)
    else
      ! It's a store node
      nnod = nnod+1
      if(nnod>maxnod) then
        write(bort_str1,'("BUFRLIB: PARUSR - THE NUMBER OF STORE NODES IN INPUT STRING")')
        write(bort_str2,'(18X,A,") EXCEEDS THE MAXIMUM (",I3,")")') str,maxnod
        call bort2(bort_str1,bort_str2)
      endif
      nods(nnod) = nod
    endif
  enddo

  ! Sort condition nodes in jump/link table order

  do i=1,ncon
    do j=i+1,ncon
      if(nodc(i)>nodc(j)) then
        nod = nodc(i)
        nodc(i) = nodc(j)
        nodc(j) = nod
        kon = kons(i)
        kons(i) = kons(j)
        kons(j) = kon
        val = ivls(i)
        ivls(i) = ivls(j)
        ivls(j) = nint(val)
      endif
    enddo
  enddo

  ! Check on special rules for conditional nodes that are bump nodes

  bump = .false.
  do n=1,ncon
    if(kons(n)==5) then
      if(io==0) then
        write(bort_str1,'("BUFRLIB: PARUSR - BUMP NODE (^ IN INPUT STRING ",A)') str
        write(bort_str2,'(18X,"IS SPECIFIED FOR A BUFR FILE OPEN FOR INPUT, THE BUFR FILE MUST BE OPEN FOR OUTPUT")')
        call bort2(bort_str1,bort_str2)
      endif
      if(n/=ncon) then
        write(bort_str1,'("BUFRLIB: PARUSR - INPUT STRING (",A,") CONTAINS")') str
        write(bort_str2,'(18X,"CONDITIONAL NODES IN ADDITION TO BUMP NODE - THE BUMP MUST BE ON THE INNER NODE")')
        call bort2(bort_str1,bort_str2)
      endif
      bump = .true.
    endif
  enddo

  ! Check store node count and alignment

  if(.not.bump .and. nnod==0) then
    write(bort_str1,'("BUFRLIB: PARUSR - INPUT STRING (",A,") HAS")') str
    write(bort_str2,'(18X,"NO STORE NODES")')
    call bort2(bort_str1,bort_str2)
  endif
  if(nnod>i1) then
    write(bort_str1,'("BUFRLIB: PARUSR - INPUT STRING (",A,")")') str
    write(bort_str2,'(18X,"HAS",I5," STORE NODES (MNEMONICS) - THE LIMIT {THIRD (INPUT) ARGUMENT} IS",I5)') nnod,i1
    call bort2(bort_str1,bort_str2)
  endif

  irpc = -1
  do i=1,nnod
    if(nods(i)>0) then
      if(irpc<0) irpc = lstjpb(nods(i),lun,'RPC')
      if(irpc/=lstjpb(nods(i),lun,'RPC').and.iac==0) then
        write(bort_str1,'("BUFRLIB: PARUSR - INPUT STRING (",A,") CONTAINS")') str
        write(bort_str2,'(18X,"STORE NODES (MNEMONICS) THAT ARE IN MORE THAN ONE REPLICATION GROUP")')
        call bort2(bort_str1,bort_str2)
      endif
    endif
  enddo

  return
end subroutine parusr

!> Parse a mnemonic from a character string.
!>
!> Parse a user-specified tag (mnemonic)
!> utg that represents a value either being decoded from a BUFR file
!> (if it is being read) or encoded into a BUFR file (if it is being
!> written). This subroutine first checks to see if the tag contains
!> a condition character ('=', '!', '<', '>', '^' or '#'). If it does
!> not, nothing happens at this point. If it does, then the type of
!> condition character is noted and the tag is stripped of all
!> characters at and beyond the condition character. In either event,
!> the resultant tag is checked against those in the internal jump/
!> link subset table (in module @ref moda_tables). If found, the node
!> associated with the tag is returned (and it is either a "condition"
!> node or a "store" node depending of the presence or absence of a
!> condition character in utg). Otherwise the node is returned as
!> zero. If the tag represents a condition node, then the condition
!> value (numeric characters beyond the condition character in the
!> user-specified tag input here) is returned.
!>
!> As an example of condition character usage, consider the following
!> example of a call to ufbint():
!>
!> @code
!>      real*8 usr(4,50)
!>             ....
!>             ....
!>      call ufbint(lunin,usr,4,50,iret,'PRLC<50000 TMDB WDIR WSPD')
!> @endcode
!>
!> Assuming that lunin points to a BUFR file open for input (reading),
!> then the usr array now contains iret levels of data (up to a maximum
!> of 50) where the value of PRLC is/was less than 50000, along with
!> the corresponding values for TMDB, WDIR and WSPD at those levels.
!>
!> As another example, consider the following example of a call to
!> readlc() for a long character string:
!>
!> @code
!>      character*200 lchr
!>             ....
!>             ....
!>      call readlc(lunin,lchr,'NUMID#3')
!> @endcode
!>
!> Assuming that lunin points to a BUFR file open for input (reading),
!> then the lchr string now contains the value corresponding to the
!> third occurrence of NUMID within the current subset.
!>
!> Valid condition codes include:
!> - '<' - less than
!> - '>' - greater than
!> - '=' - equal to
!> - '!' - not equal to
!> - '#' - ordinal identifier for a particular occurrence of a long character string
!>
!> @param lun - File ID
!> @param io - Status indicator for BUFR file associated with lun:
!> - 0 = Input file
!> - 1 = Output file
!> @param utg - User-supplied tag representing a value to be encoded/decoded to/from BUFR file
!> @param nod - Positional index in internal jump/link subset table for utg:
!> - 0 = utg not found in table
!> @param kon - Indicator for type of condition character found in UTG:
!> - 0 = No condition character found (nod is a store node)
!> - 1 = Character '=' found (nod is a condition node)
!> - 2 = Character '!' found (nod is a condition node)
!> - 3 = Character '<' found (nod is a condition node)
!> - 4 = Character '>' found (nod is a condition node)
!> - 5 = Character '^' found (nod is a condition node; specifically, a "bump" node)
!> - 6 = Character '#' found (nod is a condition node)
!> @param val - Condition value associated with condition character found in utg:
!> - 0 = utg does not have a condition character
!>
!> @author Woollen @date 1994-01-06
subroutine parutg(lun,io,utg,nod,kon,val)

  use moda_msgcwd
  use moda_tables

  implicit none

  integer, intent(in) :: lun, io
  integer, intent(out) :: nod, kon
  integer, parameter :: nchk = 8, ncond = 6
  integer, parameter :: iok(nchk) = (/-1, -1, -1, -1, -1, 0, 0, 0/)
  integer ltg, icv, inod, i, j, num, ier

  character*(*), intent(in) :: utg
  character*128 bort_str1, bort_str2
  character*20 atag
  character*3 atyp
  character*3, parameter :: btyp(nchk) = (/'SUB','SEQ','REP','RPC','RPS','DRB','DRP','DRS'/)
  character, parameter :: cond(ncond) = (/'=', '!', '<', '>', '^', '#'/)

  real, intent(out) :: val

  ! For now, set picky (see below) to always be .false.
  logical, parameter :: picky = .false.

  atag = ' '
  atyp = ' '
  kon = 0
  nod = 0
  val = 0
  ltg = min(20,len(utg))

  ! Parse utg, saving into atag only the characters prior to any condition character.

  ! But first, take care of the special case where utg denotes the short (i.e. 1-bit) delayed replication of a Table D
  ! mnemonic.  This will prevent confusion later on since '<' and '>' are each also valid as condition characters.
  if((utg(1:1)=='<').and.(index(utg(3:),'>')/=0)) then
    atag = utg
  else
    outer: do i=1,ltg
      if(utg(i:i)==' ') exit
      do j=1,ncond
        if(utg(i:i)==cond(j)) then
          kon = j
          icv = i+1
          exit outer
        endif
      enddo
      atag(i:i) = utg(i:i)
    enddo outer
  endif

  ! Find the node associated with atag in the subset table

  inod = inode(lun)
  do nod=inod,isc(inod)
    if(atag==tag(nod)) then
      ! We found it, now make sure it has a valid node type
      if(kon==5) then
        ! Condition char "^" must be associated with a delayed replication sequence (this is a "bump" node).  This is an
        ! obsolete feature but remains in the library for compatibility with older application programs.
        if(typ(nod-1)/='DRP' .and. typ(nod-1)/='DRS') then
          write(bort_str1,'("BUFRLIB: PARUTG - BUMP NODE (MNEMONIC ",A,")'// &
            ' MUST REFER TO A DELAYED REPLICATION SEQUENCE, HERE TYPE IS ",A)') atag,typ(nod-1)
          call bort(bort_str1)
        endif
      elseif(kon/=6) then
        ! Allow reading (but not writing) of delayed replication factors.
        atyp = typ(nod)
        do i=1,nchk
          if(atyp==btyp(i) .and. io>iok(i)) then
            write(bort_str1,'("BUFRLIB: PARUTG - ILLEGAL NODE TYPE: ",A," FOR MNEMONIC ",A)') atyp,atag
            call bort(bort_str1)
          endif
        enddo
      endif
      ! If it's a condition node, then get the condition value which is a number following it
      if(kon/=0) then
        call strnum(utg(icv:ltg),num,ier)
        if(ier<0) then
          write(bort_str1,'("BUFRLIB: PARUTG - CONDITION VALUE IN MNEMONIC ",A," CONTAINS NON-NUMERIC CHARACTERS")') utg
          call bort(bort_str1)
        endif
        val = num
      endif
      return
    endif
  enddo

  ! atag was not found in the subset table

  ! So what do we want to do?  We could be "picky" and abort right here, or we could allow for the possibility that, e.g. a
  ! user application has been streamlined to always call subroutine ufbint() with the same str, even though some of the
  ! mnemonics contained within that str may not exist within the sequence definition of every possible type/subtype that is
  ! being written by the application.  In such cases, by not being "picky", we could just allow the library to subsequently
  ! (and quietly, if iprt happened to be set to -1 in common /quiet/) not actually store the value corresponding to such
  ! mnemonics, rather than loudly complaining and aborting.

  if(kon==0 .and. (io==0 .or. atag=='NUL' .or. .not.picky)) then
    nod = 0
  else
    write(bort_str1,'("BUFRLIB: PARUTG - TRYING TO WRITE A MNEMONIC'// &
      ' (",A,") WHICH DOES NOT EXIST IN SUBSET TABLE")') atag
    write(bort_str2,'(18X,"(UPON INPUT, IT CONTAINED THE CONDITION CHARACTER ",A,")")') utg(icv-1:icv-1)
    call bort2(bort_str1,bort_str2)
  endif

  return
end subroutine parutg

!> Parse a string containing one or more substrings into an array of substrings.
!>
!> The separator for the substrings is specified during input, and multiple adjacent
!> occurrences of this character will be treated as a single occurrence when the string is actually parsed.
!>
!> @param str - String
!> @param tags - Array of substrings
!> @param mtag - Dimensioned size of tags within calling program; used by the subroutine to make sure it doesn't
!> overflow the tags array
!> @param ntag - Number of substrings returned in tags
!> @param sep - Separator character for substrings
!> @param limit80 - .true. if an abort should occur when str is longer than 80 characters
!>
!> @author J. Ator @date 2007-01-19
subroutine parstr(str,tags,mtag,ntag,sep,limit80)

  implicit none

  integer, intent(in) :: mtag
  integer, intent(out) :: ntag
  integer i, lstr, ltag, nchr

  character*(*), intent(in) :: str
  character*(*), intent(out) :: tags(mtag)
  character, intent(in) :: sep
  character*128 bort_str1, bort_str2

  logical, intent(in) :: limit80
  logical substr

  lstr = len(str)
  ltag = len(tags(1))
  if( limit80 .and. (lstr>80) ) then
    write(bort_str1,'("BUFRLIB: PARSTR - INPUT STRING (",A,") HAS ")') str
    write(bort_str2,'(18X,"LENGTH (",I4,"), > LIMIT OF 80 CHAR.")') lstr
    call bort2(bort_str1,bort_str2)
  endif
  ntag = 0
  nchr = 0
  substr = .false.

  do i=1,lstr
    if( .not.substr .and. (str(i:i)/=sep) ) then
      ntag = ntag+1
      if(ntag>mtag) then
        write(bort_str1,'("BUFRLIB: PARSTR - INPUT STRING (",A,") CONTAINS",I4)') str,ntag
        write(bort_str2,'(18X,"SUBSTRINGS, EXCEEDING THE LIMIT {",I4," - THIRD (INPUT) ARGUMENT}")') mtag
        call bort2(bort_str1,bort_str2)
      endif
      tags(ntag) = ' '
    endif
    if( substr .and. (str(i:i)==sep) ) nchr = 0
    substr = str(i:i)/=sep
    if(substr) then
      nchr = nchr+1
      if(nchr>ltag) then
        write(bort_str1,'("BUFRLIB: PARSTR - INPUT STRING (",A,") ")') str
        write(bort_str2,'(18X,"CONTAINS A PARSED SUBSTRING WITH LENGTH EXCEEDING THE MAXIMUM OF",I4," CHARACTERS")') ltag
        call bort2(bort_str1,bort_str2)
      endif
      tags(ntag)(nchr:nchr) = str(i:i)
    endif
  enddo

  return
end subroutine parstr
