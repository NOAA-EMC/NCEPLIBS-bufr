!> @file
!> @brief Module that contains functions that execute a query
!>
!> @author Ronald Mclaren
!> @date 2021-05-11
!>

module modq_query
  use moda_tables
  use moda_msgcwd
  use moda_usrint

  use modq_list
  use modq_query_set
  use modq_result_set
  use modq_query_parser

  implicit none


  !> @author Ronald Mclaren
  !> @date 2021-05-11
  !>
  !> @brief Data structure that holds all the information about a specific target
  !>        node derived from a query string.
  !>
  type, private :: Target
    !> @brief The name assinged to the target node
    character(len=:), allocatable :: name
    !> @brief The query string for this target node
    character(len=:), allocatable :: query_str
    !> @brief Does this target point to a string or real field.
    logical :: is_string
    !> @brief The list of sequences leading to this target
    integer, allocatable :: seq_path(:)
    !> @brief The list of node ID's associated with this target
    integer, allocatable :: node_ids(:)
  end type Target

  !> @author Ronald Mclaren
  !> @date 2021-05-24
  !>
  !> @brief Object that is used to count the number of repeats for a series of
  !>        nested sequences. Always starts at 1 for the root "subset" sequence.
  !>        Each new sequence is associated with a list of counts one for each
  !>        time the sequence is encountered and numbering the number of repeats
  !>        for that encounter. In this way it builds up a picture or tree for
  !>        the heirarchy of sequence repetitions.
  !>        
  type, private :: SeqCounter
    !> @brief The list of sequences. Starts at the root sequence and goes down from there.
    type(IntList) :: seqs
    !> @brief The list of counts for each sequence in the seqs list.
    type(IntList), allocatable :: counts_list(:)

    contains
      procedure, public :: cnts_for_seq => seq_counter__cnts_for_seq
      procedure, public :: add_cnt_for_seq => seq_counter__add_cnt_for_seq
      procedure, public :: inc_last_cnt_for_seq => seq_counter__inc_last_cnt_for_seq
      procedure, public :: dec_last_cnt_for_seq => seq_counter__dec_last_cnt_for_seq
      procedure, public :: delete => seq_counter__delete
  end type SeqCounter

  interface SeqCounter
    module procedure :: init__seq_counter
  end interface SeqCounter

  private
  character(len=3), parameter :: Subset = 'SUB'
  character(len=3), parameter :: DelayedRep = 'DRP'
  character(len=3), parameter :: DelayedRepStacked = 'DRS'
  character(len=3), parameter :: DelayedBinary = 'DRB'
  character(len=3), parameter :: Sequence = 'SEQ'
  character(len=3), parameter :: FixedRep = 'REP'
  public::query

contains


!> @author Ronald Mclaren
!> @date 2021-06-07
!>
!> @brief Executes a query
!>
!> @param[in] lunit - integer: The file unit number
!> @param[in] current_subset - type(String): The current subset string
!> @param[in] query_set - type(QuerySet): The query set
!> @param[inout] result_set - type(ResultSet): The result set to add to
!>
  subroutine query(lunit, current_subset, query_set, result_set)
    integer, intent(in) :: lunit
    type(String), intent(in) :: current_subset
    type(QuerySet), intent(in) :: query_set
    type(ResultSet), intent(inout) :: result_set

    integer :: lun, il, im
    type(Target), allocatable :: targets(:)

    call status(lunit, lun, il, im)

    targets = find_targets(lun, current_subset, query_set)
    call collect_data(lun, targets, result_set)

  end subroutine query


  !> @author Ronald Mclaren
  !> @date 2021-06-08
  !>
  !> @brief Finds the targets for all the queries in a query set 
  !> in the query set
  !>
  !> @param[in] lun - integer: Unit number for the BUFR file
  !> @param[in] current_subset - type(String): The current subset string
  !> @param[in] query_set - type(QuerySet): The query set
  !> @return targets - type(Target)(:): The list of targets
  !>
  function find_targets(lun, current_subset, query_set) result(targets)
    integer, intent(in) :: lun
    type(String), intent(in) :: current_subset
    type(QuerySet), intent(in) :: query_set
    type(Target), allocatable :: targets(:)

    integer :: q_idx, target_idx
    character(len=:), allocatable :: name
    type(String) :: query_str
    type(String), allocatable :: query_strs(:)
    type(Target), allocatable :: tmp_targets(:)
    type(Target) :: targ
    logical :: found_target

    allocate(targets(0))
    do target_idx = 1,query_set%count()
      name = query_set%get_query_name(target_idx)
      query_strs = split_into_subqueries(query_set%get_query_str(target_idx))

      found_target = .false.
      do q_idx = 1, size(query_strs)
        query_str = query_strs(q_idx)
        targ = find_target(lun, current_subset, name, query_str%chars())

        if (size(targ%node_ids) /= 0) then
          ! Fortran runtime does not deallocate memory correctly if you do
          ! targets = [targets, find_target(lun, name, query_str)]
          allocate(tmp_targets(size(targets) + 1))
          tmp_targets(1:size(targets)) = targets(1:size(targets))
          tmp_targets(size(tmp_targets)) = targ
          deallocate(targets)
          call move_alloc(tmp_targets, targets)

          found_target = .true.
          exit
        end if
      end do

      if (.not. found_target) then
        ! Add the last missing target to the list
        allocate(tmp_targets(size(targets) + 1))
        tmp_targets(1:size(targets)) = targets(1:size(targets))
        tmp_targets(size(tmp_targets)) = targ
        deallocate(targets)
        call move_alloc(tmp_targets, targets)

        print *, "Warning: Query String "  &
                 // query_set%get_query_str(target_idx) &
                 // " didn't apply to subset " &
                 // current_subset%chars()
      end if
    end do
  end function


  !> @author Ronald Mclaren
  !> @date 2021-06-08
  !>
  !> @brief Finds the node inidices and other applicable data for a specific 
  !>        query string and returns the info in a target object.
  !>
  !> @param[in] lun - integer: Unit number for the BUFR file
  !> @param[in] current_subset - type(String): The current subset string
  !> @param[in] name - character(:): The name of the target
  !> @param[in] query_str - character(:): The query string
  !> @return targ - type(Target): The target
  !>
  type(Target) function find_target(lun, current_subset, name, query_str) result(targ)
    integer, intent(in) :: lun
    type(String), intent(in) :: current_subset
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: query_str

    integer, allocatable :: target_nodes(:)
    logical :: is_string = .false.
    integer :: node_idx, path_idx, rewind_idx
    integer :: index
    integer :: table_cursor, mnemonic_cursor
    character(len=10) :: subset
    character(len=10), allocatable :: mnemonics(:)
    integer, allocatable :: branches(:)
    logical :: is_missing_target
    type(IntList) :: seq_path

    call split_query_str(query_str, subset, mnemonics, index)

    is_missing_target = .false.
    ! Ignore targets with the wrong subset ID
    if (subset /= "*") then
      if (subset /= current_subset%chars()) then
        is_missing_target = .true.

        allocate(branches(0))
        allocate(target_nodes(0))
      end if
    end if

    if (.not. is_missing_target) then
      allocate(branches(size(mnemonics) - 1))
      allocate(target_nodes(0))

      seq_path = IntList()
      call seq_path%push(inode(lun))  ! Add root node id

      node_idx = 1
      table_cursor = 0
      mnemonic_cursor = 0
      do node_idx = inode(lun), isc(inode(lun))
        if (typ(node_idx) == DelayedRep .or. &
            typ(node_idx) == FixedRep .or. &
            typ(node_idx) == DelayedRepStacked .or. &
            typ(node_idx) == DelayedBinary) then
          ! Enter the sequence
          if (tag(node_idx + 1) == mnemonics(mnemonic_cursor + 1) .and. &
              table_cursor == mnemonic_cursor) then

            mnemonic_cursor = mnemonic_cursor + 1
            branches(mnemonic_cursor) = node_idx
          end if

          call seq_path%push(node_idx + 1)
          table_cursor = table_cursor + 1

        else if (mnemonic_cursor == size(mnemonics) - 1 .and. &
          table_cursor == mnemonic_cursor .and. &
          tag(node_idx) == mnemonics(size(mnemonics))) then

          ! We found a target
          target_nodes = [target_nodes, node_idx]
          is_string = (itp(node_idx) == 3)

          ! Neccessary cause Fortran handles .and. in if statements in a strange way
          if (seq_path%length() > 1) then
            ! Peak ahead to see if the next node is inside one of the containing sequences
            ! then go back up the approptiate number of sequences. You may have to exit several
            ! sequences in a row if the current sequence is the last element in the containing
            ! sequence.
            do path_idx = seq_path%length() - 1, 1, -1
              if (seq_path%at(path_idx) == jmpb(node_idx + 1)) then
                do rewind_idx = 1, seq_path%length() - path_idx

                  ! Exit the sequence
                  if (mnemonic_cursor > 0 .and. table_cursor == mnemonic_cursor) then
                    if (link(branches(mnemonic_cursor)) == node_idx) then
                      mnemonic_cursor = mnemonic_cursor - 1
                    end if
                  end if

                  call seq_path%pop()
                  table_cursor = table_cursor - 1
                end do
                exit
              end if
            end do
          end if

        else if (seq_path%length() > 1) then
          ! Peak ahead to see if the next node is inside one of the containing sequences
          ! then go back up the approptiate number of sequences. You may have to exit several
          ! sequences in a row if the current sequence is the last element in the containing
          ! sequence.
          do path_idx = seq_path%length() - 1, 1, -1
            if (seq_path%at(path_idx) == jmpb(node_idx + 1)) then
              do rewind_idx = 1, seq_path%length() - path_idx

                ! Exit the sequence
                if (mnemonic_cursor > 0 .and. table_cursor == mnemonic_cursor) then
                  if (link(branches(mnemonic_cursor)) == node_idx) then
                    mnemonic_cursor = mnemonic_cursor - 1
                  end if
                end if

                call seq_path%pop()
                table_cursor = table_cursor - 1
              end do
              exit
            end if
          end do
        end if
      end do

      if (index > 0 .and. index <= size(target_nodes)) then
        if (index > size(target_nodes)) then
          call bort('Invalid index in query str ' // query_str // '.')
        end if

        target_nodes = [target_nodes(index)]
      end if

      if (size(target_nodes) > 1) then
        call bort('Query string must return 1 target. Are you missing an index? ' // query_str // '.')
      end if
    end if

    targ = Target(name, query_str, is_string, branches, target_nodes)

    deallocate(mnemonics)
  end function


  !> @author Ronald Mclaren
  !> @date 2021-06-08
  !>
  !> @brief Collects the data for the targets and adds them to the result set.
  !>
  !> @param[in] lun - integer: Unit number for the BUFR file
  !> @param[in] targets - type(Target)(:): Array of targets
  !> @param[in] result_set - type(ResultSet): The result set
  !>
  subroutine collect_data(lun, targets, result_set)
    integer, intent(in) :: lun
    type(Target), target, intent(in) :: targets(:)
    type(ResultSet), intent(inout) :: result_set

    real(kind=8), allocatable :: dat(:)
    integer :: dims(2)
    integer :: node_idx, path_idx, rep_node_idx, target_idx
    integer :: current_sequence
    integer :: data_cursor, path_cursor
    integer :: collected_data_cursor
    integer :: target_node
    integer, allocatable :: rep_node_idxs(:)
    type(DataField) :: data_field
    type(DataFrame) :: data_frame
    type(SeqCounter) :: seq_counter
    type(Target), pointer :: targ

    do target_idx = 1, size(targets)
      targ => targets(target_idx)
      
      if (allocated(dat)) then
        deallocate(dat)
      end if
      if (allocated(rep_node_idxs)) then
        deallocate(rep_node_idxs)
      end if

      ! Ignore targets with the wrong subset ID
      if (size(targ%node_ids) == 0) then
        allocate(dat(1))
        dat(1) = MissingValue

        data_field = DataField()
        data_field%name = String(targ%name)
        data_field%query_str = String(targ%query_str)
        data_field%data = dat
        data_field%missing = .true.
        call data_frame%add(data_field)
        cycle
      end if

      dims = result_shape(lun, targ%node_ids)

      allocate(dat(dims(1)))
      allocate(rep_node_idxs, source=targ%seq_path)

      path_cursor = 0
      current_sequence = 1
      target_node = targ%node_ids(1)
      rep_node_idxs = rep_node_idxs + 1 ! Rep node always one after the seq node
      collected_data_cursor = 1

      seq_counter = SeqCounter()
      call seq_counter%add_cnt_for_seq(1, 1)

      do data_cursor = 1, nval(lun)
        node_idx = inv(data_cursor, lun)

        ! Collect the data
        if (node_idx == target_node) then
          dat(collected_data_cursor)  = val(data_cursor, lun)
          collected_data_cursor = collected_data_cursor + 1
        end if

        ! Update sequence count
        if (path_cursor > 0) then
          rep_node_idx = rep_node_idxs(path_cursor)

          if (node_idx == rep_node_idx) then
            call seq_counter%inc_last_cnt_for_seq(rep_node_idx)
          else if (node_idx == link(targ%seq_path(path_cursor))) then
            call seq_counter%dec_last_cnt_for_seq(rep_node_idx)
            path_cursor = path_cursor - 1
          else if (path_cursor == size(targ%seq_path)) then
            continue
          else if (node_idx == targ%seq_path(path_cursor + 1)) then
            path_cursor = path_cursor + 1
            rep_node_idx = rep_node_idxs(path_cursor)
            call seq_counter%add_cnt_for_seq(rep_node_idx, 0)
          end if
        else if (size(targ%seq_path) >= path_cursor + 1) then
          if (node_idx == targ%seq_path(path_cursor + 1)) then
            path_cursor = path_cursor + 1
            rep_node_idx = rep_node_idxs(path_cursor)
            call seq_counter%add_cnt_for_seq(rep_node_idx, 0)
          end if
        end if
      end do

      data_field = DataField()
      data_field%name = String(targ%name)
      data_field%query_str = String(targ%query_str)
      data_field%is_string = targ%is_string
      data_field%data = dat

      if (allocated(data_field%seq_path)) then
        deallocate(data_field%seq_path)
      end if

      allocate(data_field%seq_path, source=seq_counter%seqs%array())

      if (allocated(data_field%seq_counts)) then
        deallocate(data_field%seq_counts)
      end if

      allocate(data_field%seq_counts(size(data_field%seq_path)))
      do path_idx = 1, size(data_field%seq_path)
        allocate(data_field%seq_counts(path_idx)%counts, &
                 source=seq_counter%counts_list(path_idx)%array())
      end do

      call seq_counter%delete()
      call data_frame%add(data_field)
    end do

    call result_set%add(data_frame)
  end subroutine


  !> @author Ronald Mclaren
  !> @date 2021-05-11
  !>
  !> @brief Computes the shape of the resulting data given target node ids.
  !>
  !> @param[in] lun - integer: Unit number for the BUFR file
  !> @param[in] target_nodes - integer(:): Array of target node ids
  !>
  function result_shape(lun, target_nodes) result(dims)
    integer, intent(in) :: lun
    integer, intent(in) :: target_nodes(:)
    integer :: dims(2)

    integer :: data_cursor

    dims = 0
    do data_cursor = 1, nval(lun)
      if (inv(data_cursor, lun) == target_nodes(1)) then
        dims(1) = dims(1) + 1
      end if
    end do

    dims(2) = size(target_nodes)
  end function


  ! SeqCounter methods

  !> @author Ronald Mclaren
  !> @date 2021-05-24
  !>
  !> @brief Initializes a new SeqCounter object.
  !>
  !> @return seq_counter - type(SeqCounter): The initialized SeqCounter object
  !>
  type(SeqCounter) function init__seq_counter() result(seq_counter)
    seq_counter = SeqCounter(IntList(), null())
    allocate(seq_counter%counts_list(0))
  end function init__seq_counter


  !> @author Ronald Mclaren
  !> @date 2021-05-24
  !>
  !> @brief Gets the counts for a given sequence.
  !>
  !> @param[in] self - class(SeqCounter): The SeqCounter instance
  !> @param[in] seq - integer: The sequence index
  !> @return counts - type(IntList): The counts for the given sequence
  !>
  type(IntList) function seq_counter__cnts_for_seq(self, seq) result(counts)
    class(SeqCounter), intent(in) :: self
    integer, intent(in) :: seq
    integer :: idx
    logical :: found

    found = .false.
    do idx = 1, self%seqs%length()
      if (seq == self%seqs%at(idx)) then
        counts = self%counts_list(idx)
        found = .true.
        exit
      end if
    end do

    if (.not. found) then
      call bort("Getting counts for unkown sequence.")
    end if

  end function seq_counter__cnts_for_seq


  !> @author Ronald Mclaren
  !> @date 2021-05-24
  !>
  !> @brief Adds a count of repeats for a given sequence.
  !>
  !> @param[in] self - class(SeqCounter): The SeqCounter instance
  !> @param[in] seq - integer: The sequence index
  !> @param[in] count - integer: The count (number of repeats) to add
  !>
  subroutine seq_counter__add_cnt_for_seq(self, seq, cnt)
    class(SeqCounter), intent(inout) :: self
    integer, intent(in) :: seq
    integer, intent(in) :: cnt

    logical :: found
    integer :: idx
    type(IntList), allocatable :: tmp_counts_list(:)

    found = .false.
    do idx = 1, self%seqs%length()
      if (seq == self%seqs%at(idx)) then
        found = .true.
        call self%counts_list(idx)%push(cnt)
      end if
    end do

    if (.not. found) then
      call self%seqs%push(seq)

      ! Fortran runtime has problems with custom types in
      ! self%counts_list = [self%counts_list, IntList((/cnt/))]
      allocate(tmp_counts_list(size(self%counts_list) + 1))
      tmp_counts_list(1:size(self%counts_list)) = self%counts_list(1:size(self%counts_list))
      tmp_counts_list(size(tmp_counts_list)) = IntList((/cnt/))
      deallocate(self%counts_list)
      call move_alloc(tmp_counts_list, self%counts_list)
    end if

  end subroutine seq_counter__add_cnt_for_seq


  !> @author Ronald Mclaren
  !> @date 2021-05-24
  !>
  !> @brief Increments the last count for a given sequence by 1.
  !>
  !> @param[in] self - class(SeqCounter): The SeqCounter instance
  !> @param[in] seq - integer: The sequence index
  !>
  subroutine seq_counter__inc_last_cnt_for_seq(self, seq)
    class(SeqCounter), intent(inout) :: self
    integer, intent(in) :: seq
    logical :: found
    integer :: idx

    found = .false.
    do idx = 1, self%seqs%length()
      if (seq == self%seqs%at(idx)) then
        found = .true.

        self%counts_list(idx)%at(self%counts_list(idx)%length()) = &
          self%counts_list(idx)%at(self%counts_list(idx)%length()) + 1
        exit
      end if
    end do

    if (.not. found) then
      call bort("SeqCounter: Trying to increment unknown sequence.")
    end if

  end subroutine seq_counter__inc_last_cnt_for_seq


  !> @author Ronald Mclaren
  !> @date 2021-05-24
  !>
  !> @brief Decrements the last count for a given sequence.
  !>
  !> @param[in] self - class(SeqCounter): The SeqCounter instance
  !> @param[in] seq - integer: The sequence index
  !>
  subroutine seq_counter__dec_last_cnt_for_seq(self, seq)
    class(SeqCounter), intent(inout) :: self
    integer, intent(in) :: seq
    logical :: found
    integer :: idx

    found = .false.
    do idx = 1, self%seqs%length()
      if (seq == self%seqs%at(idx)) then
        found = .true.
        self%counts_list(idx)%at(self%counts_list(idx)%length()) = &
          self%counts_list(idx)%at(self%counts_list(idx)%length()) - 1
        exit
      end if
    end do

    if (.not. found) then
      call bort("SeqCounter: Trying to increment unknown sequence.")
    end if
  end subroutine seq_counter__dec_last_cnt_for_seq


  !> @author Ronald Mclaren
  !> @date 2021-06-01
  !>
  !> @brief Deletes the sequence counter from memory.
  !>
  !> @param[in] self - class(SeqCounter): The SeqCounter instance
  !>
  subroutine seq_counter__delete(self)
    class(SeqCounter), intent(inout) :: self

! GNU does not finalize this object properly
#ifdef GNU
    block
      integer :: idx

      do idx = 1, size(self%counts_list)
        call self%counts_list(idx)%delete()
      end do

      deallocate(self%counts_list)
    end block
#endif

  end subroutine seq_counter__delete
end module modq_query
