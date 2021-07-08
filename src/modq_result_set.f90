

module modq_result_set
  use modq_string
  implicit none

  private

  real(kind=8), public, parameter :: MissingValue = 10e10_8

  type, public :: SeqCounts
    integer, allocatable :: counts(:)
  end type

  interface SeqCounts
    module procedure :: initialize__seq_counts
  end interface SeqCounts

  type, public :: DataField
    type(String) :: name
    type(String) :: query_str
    logical :: missing = .false.
    real(kind=8), allocatable :: data(:)
    integer, allocatable :: seq_path(:)
    type(SeqCounts), allocatable :: seq_counts(:)

  contains
    final :: data_field__delete
  end type

  interface DataField
    module procedure :: initialize__data_field
  end interface DataField

  type, public :: DataFrame
    type(DataField), allocatable :: data_fields(:)

  contains
    procedure :: add => data_frame__add
    procedure :: field_for_node_named => data_frame__field_for_node_named
    final ::  data_frame__delete
  end type

  interface DataFrame
    module procedure :: initialize__data_frame
  end interface DataFrame

  type, public :: ResultSet

    type(DataFrame), allocatable :: data_frames(:)
    type(String), allocatable :: names(:)

    contains
      procedure :: get => result_set__get
      procedure :: rep_counts => result_set__rep_counts
      procedure :: get_counts => result_set__get_counts
      procedure :: add => result_set__add
      final :: result_set__delete
  end type ResultSet

  interface ResultSet
    module procedure :: initialize__result_set
  end interface ResultSet

contains
  ! Sequence Counts
  type(SeqCounts) function initialize__seq_counts() result(seq_counts)
    seq_counts = SeqCounts(null())
    allocate(seq_counts%counts(0))
  end function

  ! Data Field Procedures
  type(DataField) function initialize__data_field() result(data_field)
    ! Needed because of gfortran bug
    data_field = DataField(String(""), String(""), .false., null(), null(), null())

    allocate(data_field%data(0))
    allocate(data_field%seq_path(0))
  end function initialize__data_field

  subroutine data_field__delete(self)
    type(DataField), intent(inout) :: self

    if (allocated(self%data)) then
      deallocate(self%data)
    end if

    if (allocated(self%seq_path)) then
      deallocate(self%seq_path)
    end if

    if (allocated(self%seq_counts)) then
      deallocate(self%seq_counts)
    end if

  end subroutine data_field__delete

  ! Data Frame Procedures

  type(DataFrame) function initialize__data_frame() result(data_frame)
    data_frame = DataFrame(null())  ! Needed because of gfortran bug
  end function initialize__data_frame
!
  subroutine data_frame__add(self, data_field)
    class(DataFrame), intent(inout) :: self
    type(DataField), intent(in) :: data_field

    type(DataField), allocatable :: tmp_data_field(:)

    if (.not. allocated(self%data_fields)) then
      allocate(self%data_fields(0))
    end if

    allocate(tmp_data_field(size(self%data_fields) + 1))
    tmp_data_field(1:size(self%data_fields)) = self%data_fields
    tmp_data_field(size(tmp_data_field)) = data_field
    call move_alloc(tmp_data_field, self%data_fields) 

  end subroutine data_frame__add


  function data_frame__field_for_node_named(self, name) result(field)
    class(DataFrame), intent(in) :: self
    type(String), intent(in) :: name

    integer :: field_idx
    logical :: field_found
    type(DataField), allocatable :: field

    field_found = .false.
    do field_idx = 1, size(self%data_fields)
      if (self%data_fields(field_idx)%name == name) then
        field = self%data_fields(field_idx)
        field_found = .true.
        exit
      end if
    end do

    if (.not. field_found) then
      call bort("Using unknown field named " // name%chars())
    end if
  end function data_frame__field_for_node_named


  subroutine data_frame__delete(self)
    type(DataFrame), intent(inout) :: self

    if (allocated(self%data_fields)) then
      deallocate(self%data_fields)
    end if
  end subroutine data_frame__delete

  ! Result Set Procedures

  type(ResultSet) function initialize__result_set() result(result_set)
    result_set = ResultSet(null(), null())   ! Needed because of gfortran bug

    allocate(result_set%data_frames(0))
    allocate(result_set%names(0))
  end function initialize__result_set


  function result_set__get(self, field_name, for) result(data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: for
    real(kind=8), allocatable :: data(:)

    integer :: frame_idx, data_idx, rep_idx
    type(DataField), allocatable :: target_field, for_field

    integer, allocatable :: rep_counts(:)
    real(kind=8), allocatable :: field_data(:)
    type(DataFrame), allocatable :: df

    allocate(data(0))

    do frame_idx = 1, size(self%data_frames)
      df = self%data_frames(frame_idx)
      target_field = df%field_for_node_named(String(field_name))

      if (.not. target_field%missing) then
        if (present(for) .and. for /= "") then
          for_field = self%data_frames(frame_idx)%field_for_node_named(String(for))
          rep_counts = self%rep_counts(target_field, for_field)

          allocate(field_data(sum(rep_counts)))

          do data_idx = 1, size(rep_counts)
            do rep_idx = 1, rep_counts(data_idx)
              field_data(sum(rep_counts(1:data_idx - 1)) + rep_idx) = target_field%data(data_idx)
            end do
          end do

          data = [data, field_data]
          deallocate(field_data)
        else
          data = [data, target_field%data]
        end if
      else
        if (present(for)) then
          for_field = self%data_frames(frame_idx)%field_for_node_named(String(for))
          allocate(field_data(size(for_field%data)))
          field_data = MissingValue
          data = [data, field_data]
          deallocate(field_data)
        else
          data = [data, real(MissingValue, 8)]
        end if
      end if

      if (allocated(target_field)) then
        deallocate(target_field)
      end if

      if (allocated(for_field)) then
        deallocate(for_field)
      end if

      if (allocated(df)) then
        deallocate(df)
      end if
    end do
  end function

  function result_set__rep_counts(self, target_field, for_field) result(counts)
    class(ResultSet), intent(in) :: self
    type(DataField), intent(in) :: target_field
    type(DataField), intent(in) :: for_field

    integer, allocatable :: counts(:)
    integer :: seq_idx, rep_idx
    integer :: target_count
    integer :: count

    do seq_idx = 1, size(target_field%seq_path)
      if (target_field%seq_path(seq_idx) /= for_field%seq_path(seq_idx)) then
        call bort("The target field " // target_field%name%chars() // " and the for field " &
                  // for_field%name%chars() // " don't occur along the same path.")
      end if
    end do

    allocate(counts(0))
    target_count =  sum(target_field%seq_counts(size(target_field%seq_counts))%counts)
    if (target_count > 0) then
      do rep_idx = 1, target_count
        count = self%get_counts(for_field, &
                                size(target_field%seq_counts) + 1, &
                                1, &
                                rep_idx - 1)

        counts = [counts, count]
      end do
    end if
  end function result_set__rep_counts

  recursive function result_set__get_counts(self, for_field, seq_idx, last_count, offset) result(count)
    class(ResultSet), intent(in) :: self
    type(DataField), target, intent(in) :: for_field
    integer, intent(in) :: seq_idx
    integer, intent(in) :: last_count
    integer, intent(in) :: offset
    integer :: count

    integer, pointer :: seq_counts(:)
    integer :: cnt_idx

    count = 0
    if (seq_idx == size(for_field%seq_path)) then
      count = sum(for_field%seq_counts(seq_idx)%counts(offset + 1:offset + last_count))
    else
      seq_counts => for_field%seq_counts(seq_idx)%counts
      do cnt_idx = offset + 1, offset + last_count
        count = count + self%get_counts(for_field, &
                                        seq_idx + 1, &
                                        seq_counts(cnt_idx), &
                                        sum(seq_counts(1:cnt_idx - 1)))
      end do
    end if
  end function result_set__get_counts

  subroutine result_set__add(self, data_frame)
    class(ResultSet), intent(inout) :: self
    type(DataFrame), intent(in) :: data_frame

    integer :: field_idx, name_idx
    logical :: name_found
    type(DataField) :: field
    type(DataFrame), allocatable :: tmp_data_frames(:)
    type(String), allocatable :: tmp_names(:)

    if (.not. allocated(self%data_frames)) then
      allocate(self%data_frames(0))
    end if

    do field_idx = 1, size(data_frame%data_fields)
      field = data_frame%data_fields(field_idx)

      name_found = .false.
      do name_idx = 1, size(self%names)
        if (field%name == self%names(name_idx)) then
          name_found = .true.
          exit
        end if
      end do

      if (.not. name_found) then
        allocate(tmp_names(size(self%names) + 1))
        tmp_names(1:size(self%names)) = self%names
        tmp_names(size(tmp_names)) = field%name
        call move_alloc(tmp_names, self%names)
      end if
    end do

    allocate(tmp_data_frames(size(self%data_frames) + 1))
    tmp_data_frames(1:size(self%data_frames)) = self%data_frames
    tmp_data_frames(size(tmp_data_frames)) = data_frame
    call move_alloc(tmp_data_frames, self%data_frames)

  end subroutine result_set__add

  subroutine result_set__delete(self)
    type(ResultSet), intent(inout) :: self

    if (allocated(self%names)) then
      deallocate(self%names)
    end if

    if (allocated(self%data_frames)) then
      deallocate(self%data_frames)
    end if
  end subroutine result_set__delete

end module modq_result_set
