

module modq_query
  use moda_tables
  use moda_msgcwd
  use moda_usrint

  use modq_list
  use modq_query_set
  use modq_result_set
  use modq_query_parser

  implicit none

  type, private :: Target
    character(len=:), allocatable :: name
    integer, allocatable :: seq_path(:)
    integer, allocatable :: node_ids(:)
  end type Target

  type, private :: SeqCounter
    type(IntList) :: seqs
    type(IntList), allocatable :: counts_list(:)

    contains
      procedure, public :: cnts_for_seq => seq_counter__cnts_for_seq
      procedure, public :: add_cnt_for_seq => seq_counter__add_cnt_for_seq
      procedure, public :: inc_last_cnt_for_seq => seq_counter__inc_last_cnt_for_seq
      procedure, public :: dec_last_cnt_for_seq => seq_counter__dec_last_cnt_for_seq
  end type SeqCounter

  interface SeqCounter
    module procedure :: init__seq_counter
    module procedure :: init__seq_counter_w_seq
  end interface SeqCounter

  private
  character(len=3), parameter :: Subset = 'SUB'
  character(len=3), parameter :: DelayedRep = 'DRP'
  character(len=3), parameter :: Sequence = 'SEQ'
  character(len=3), parameter :: FixedRep = 'REP'
  character(len=3), parameter :: Value = 'NUM'
  public::query

contains
!
  subroutine query(lunit, query_set, result_set)
    integer, intent(in) :: lunit
    type(QuerySet), intent(in) :: query_set
    type(ResultSet), intent(inout) :: result_set

    integer :: lun, il, im
    type(SeqCounter) :: seq_counter

    character(len=10), allocatable :: mnemonics(:)
    integer :: index, idx, idx2
    type(Target), allocatable :: targets(:)

    call status(lunit, lun, il, im)

    targets = find_targets(lun, query_set)
    call collect_data(lun, targets, result_set)

!    do idx = 1, seq_counter%seqs%count()
!      print *, "Seq: ", seq_counter%seqs%at(idx)
!
!      do idx2 = 1, seq_counter%counts_list(idx)%count()
!        print *, " ", seq_counter%counts_list(idx)%count(), seq_counter%counts_list(idx)%at(idx2)
!      end do
!
!      print *, " "
!    end do

  end subroutine query


  function find_targets(lun, query_set) result(targets)
    integer, intent(in) :: lun
    type(QuerySet), intent(in) :: query_set
    type(Target), allocatable :: targets(:)

    integer :: target_idx
    character(len=:), allocatable :: name
    character(len=:), allocatable :: query_str

    allocate(targets(0))
    do target_idx = 1,query_set%count()
      name = query_set%get_query_name(target_idx)
      query_str = query_set%get_query_str(target_idx)

      targets = [targets, find_target(lun, name, query_str)]
    end do
  end function


  type(Target) function find_target(lun, name, query_str) result(targ)
    integer, intent(in) :: lun
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: query_str

    integer, allocatable :: target_nodes(:)
    integer :: current_sequence
    integer :: node_idx
    integer :: index
    integer :: table_cursor, mnemonic_cursor
    character(len=10), allocatable :: mnemonics(:)
    integer :: target_idx

    integer, allocatable :: branches(:)

    call split_query_str(query_str, mnemonics, index)

    allocate(branches(size(mnemonics) - 1))
    allocate(target_nodes(0))

    node_idx = 1
    table_cursor = 0
    mnemonic_cursor = 0
    current_sequence = 1
    do node_idx = inode(lun), isc(inode(lun))
      if (mnemonic_cursor == size(mnemonics) - 1 .and. &
          table_cursor == mnemonic_cursor .and. &
          tag(node_idx) == mnemonics(size(mnemonics))) then

        ! We found a target
        target_nodes = [target_nodes, node_idx]

      else if (typ(node_idx) == DelayedRep .or. typ(node_idx) == FixedRep) then
        ! Enter the sequence
        if (tag(node_idx + 1) == mnemonics(mnemonic_cursor + 1) .and. &
            table_cursor == mnemonic_cursor) then

          mnemonic_cursor = mnemonic_cursor + 1
          branches(mnemonic_cursor) = node_idx
        end if

        current_sequence = node_idx
        table_cursor = table_cursor + 1

      else if (link(current_sequence) == node_idx) then
        ! Exit sequence
        if (mnemonic_cursor > 0 .and. &
            table_cursor == mnemonic_cursor .and. &
            link(branches(mnemonic_cursor)) == node_idx) then

          mnemonic_cursor = mnemonic_cursor - 1
        end if

        table_cursor = table_cursor - 1
        current_sequence = jmpb(node_idx - 1)
      end if
    end do

    targ = Target(name, branches, target_nodes)

    deallocate(mnemonics)

    if (index > 0 .and. index <= size(target_nodes)) then
      if (index > size(target_nodes)) then
        error stop 'Invalid index in query str ' // query_str // '.'
      end if

      target_nodes = [target_nodes(index)]
    end if

    if (size(target_nodes) == 0) then
      error stop 'Could not find the target node for ' // query_str // '.'
    end if
  end function


  subroutine collect_data(lun, targets, result_set)
    integer, intent(in) :: lun
    type(Target), intent(in) :: targets(:)
    type(ResultSet), intent(inout) :: result_set

    real(kind=8), allocatable :: dat(:,:)
    integer :: dims(2)
    integer :: col_idx, node_idx, path_idx
    integer :: current_sequence
    integer :: data_cursor, path_cursor
    integer, allocatable :: collected_data_cursor(:)
    logical :: target_found
    integer, allocatable :: target_nodes(:)
    type(SeqCounter) :: seq_counter

    integer :: target_idx
    type(DataField) :: data_field
    type(DataFrame) :: data_frame

    do target_idx = 1, size(targets)
      dims = result_shape(lun, targets(target_idx)%node_ids)

      if (allocated(dat)) then
        deallocate(dat)
      end if

      allocate(dat(dims(1), dims(2)))

      path_cursor = 0
      current_sequence = 1
      target_nodes = targets(target_idx)%node_ids
!      seq_counter = SeqCounter()
      allocate(collected_data_cursor(dims(2)))

      collected_data_cursor = 1
      do data_cursor = 1, nval(lun)
        node_idx = inv(data_cursor, lun)
        target_found = .false.

        do col_idx = 1, dims(2)
          ! Found the target element, record it
          if (node_idx == target_nodes(col_idx)) then

            dat(collected_data_cursor(col_idx), col_idx)  = val(data_cursor, lun)
            collected_data_cursor(col_idx) = collected_data_cursor(col_idx) + 1

            target_found = .true.
          end if
        end do

!        if (.not. target_found) then
!          ! Found a path sequence, go down the tree
!          if (typ(node_idx) == DelayedRep .or. typ(node_idx) == FixedRep) then
!
!            path_cursor = path_cursor + 1
!            current_sequence = node_idx
!
!            if (size(seq_path) < path_cursor) then
!              current_seq_path = [current_seq_path, node_idx]
!            else
!              current_seq_path(path_cursor) = node_idx
!            end if
!
!            ! Found the end of the sequence, go up the tree
!          else if (link(current_sequence) == node_idx) then
!
!            path_cursor = path_cursor - 1
!            current_sequence = jmpb(node_idx - 1)
!            current_seq_path(path_cursor) = -1
!          end if
!        end if
      end do

      seq_counter = get_counts(lun, targets(target_idx))

      data_field = DataField()
      data_field%name = String(targets(target_idx)%name)
      data_field%node_id = target_nodes(1)
      data_field%data = dat

      if (allocated(data_field%seq_path)) then
        deallocate(data_field%seq_path)
      end if

      allocate(data_field%seq_path, source=seq_counter%seqs%get_array())

      if (allocated(data_field%seq_counts)) then
        deallocate(data_field%seq_counts)
      end if

      allocate(data_field%seq_counts(size(data_field%seq_path)))
      do path_idx = 1, size(data_field%seq_path)
        allocate(data_field%seq_counts(path_idx)%counts, source=seq_counter%counts_list(path_idx)%get_array())
      end do

      call data_frame%add(data_field)

      deallocate(collected_data_cursor)
    end do

    call result_set%add(data_frame)
  end subroutine


  type(SeqCounter) function get_counts(lun, targ) result(seq_counter)
    integer, intent(in) :: lun
    type(Target), intent(in) :: targ

    integer :: path_cursor
    integer :: data_cursor
    integer :: target_idx, node_idx, rep_node_idx
    integer, allocatable :: rep_node_idxs(:)
    integer, allocatable :: seq_node_idxs(:)

    allocate(rep_node_idxs, source=targ%seq_path)
    rep_node_idxs = rep_node_idxs + 1 ! Rep node always one after the seq node

    seq_counter = SeqCounter()
    call seq_counter%add_cnt_for_seq(1, 1)

!    dims = 0
    path_cursor = 0
    do data_cursor = 1, nval(lun)
      node_idx = inv(data_cursor, lun)

      if (path_cursor > 0) then
        rep_node_idx = rep_node_idxs(path_cursor)

        if (node_idx == targ%seq_path(path_cursor + 1)) then
          path_cursor = path_cursor + 1
          rep_node_idx = rep_node_idxs(path_cursor)
          call seq_counter%add_cnt_for_seq(rep_node_idx, 0)
        else if (node_idx == rep_node_idx) then
          call seq_counter%inc_last_cnt_for_seq(rep_node_idx)
        else if (node_idx == link(targ%seq_path(path_cursor))) then
          call seq_counter%dec_last_cnt_for_seq(rep_node_idx)
          path_cursor = path_cursor - 1
        end if
      else
        if (node_idx == targ%seq_path(path_cursor + 1)) then
          path_cursor = path_cursor + 1
          rep_node_idx = rep_node_idxs(path_cursor)
          call seq_counter%add_cnt_for_seq(rep_node_idx, 0)
        end if
      end if
    end do
  end function


  function result_shape(lun, target_nodes) result(dims)
    integer, intent(in) :: lun
    integer, intent(in) :: target_nodes(:)
    integer :: dims(2)

    integer :: data_cursor
    integer :: target_idx, node_idx

    dims = 0
    do data_cursor = 1, nval(lun)
      if (inv(data_cursor, lun) == target_nodes(1)) then
        dims(1) = dims(1) + 1
      end if
    end do

    dims(2) = size(target_nodes)
  end function


  ! SeqCounter methods
  type(SeqCounter) function init__seq_counter() result(seq_counter)
    seq_counter = SeqCounter(IntList(), null())
    allocate(seq_counter%counts_list(0))
  end function init__seq_counter


  type(SeqCounter) function init__seq_counter_w_seq(seqs) result(seq_counter)
    type(IntList), intent(in) :: seqs
    type(IntList), allocatable :: int_lists(:)

    allocate(int_lists(seqs%count()))
    seq_counter = SeqCounter(seqs, int_lists)
  end function init__seq_counter_w_seq


  type(IntList) function seq_counter__cnts_for_seq(self, seq) result(counts)
    class(SeqCounter), intent(in) :: self
    integer, intent(in) :: seq
    integer :: idx
    logical :: found

    found = .false.
    do idx = 1, self%seqs%count()
      if (seq == self%seqs%at(idx)) then
        counts = self%counts_list(idx)
        found = .true.
        exit
      end if
    end do

    if (.not. found) then
      error stop "Getting counts for unkown sequence."
    end if

  end function seq_counter__cnts_for_seq


  subroutine seq_counter__add_cnt_for_seq(self, seq, cnt)
    class(SeqCounter), intent(inout) :: self
    integer, intent(in) :: seq
    integer, intent(in) :: cnt

    logical :: found
    integer :: idx
    type(IntList) :: counts

    found = .false.
    do idx = 1, self%seqs%count()
      if (seq == self%seqs%at(idx)) then
        found = .true.
        call self%counts_list(idx)%append(cnt)
      end if
    end do

    if (.not. found) then
      call self%seqs%append(seq)
      self%counts_list = [self%counts_list, IntList((/cnt/))]
    end if

  end subroutine seq_counter__add_cnt_for_seq


  subroutine seq_counter__inc_last_cnt_for_seq(self, seq)
    class(SeqCounter), intent(inout) :: self
    integer, intent(in) :: seq
    logical :: found
    integer :: idx

    found = .false.
    do idx = 1, self%seqs%count()
      if (seq == self%seqs%at(idx)) then
        found = .true.
        call self%counts_list(idx)%set(self%counts_list(idx)%count(), &
                                       self%counts_list(idx)%at(self%counts_list(idx)%count()) + 1)
        exit
      end if
    end do

    if (.not. found) then
      error stop "SeqCounter: Trying to increment unknown sequence."
    end if

  end subroutine seq_counter__inc_last_cnt_for_seq


  subroutine seq_counter__dec_last_cnt_for_seq(self, seq)
    class(SeqCounter), intent(inout) :: self
    integer, intent(in) :: seq
    logical :: found
    integer :: idx
    type(IntList) :: list

    found = .false.
    do idx = 1, self%seqs%count()
      if (seq == self%seqs%at(idx)) then
        found = .true.
        call self%counts_list(idx)%set(self%counts_list(idx)%count(), &
                                       self%counts_list(idx)%at(self%counts_list(idx)%count()) - 1)
        exit
      end if
    end do

    if (.not. found) then
      error stop "SeqCounter: Trying to increment unknown sequence."
    end if


  end subroutine seq_counter__dec_last_cnt_for_seq
end module modq_query