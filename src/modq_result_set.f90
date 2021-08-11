

module modq_result_set
  use modq_string
  implicit none

  private

  real(kind=8), public, parameter :: MissingValue = 10e10_8
  integer, parameter :: DataFrameResizeSize = 50000

  type, public :: SeqCounts
    integer, allocatable :: counts(:)
  end type

  interface SeqCounts
    module procedure :: initialize__seq_counts
  end interface SeqCounts

  type, public :: DataField
    type(String) :: name
    type(String) :: query_str
    logical :: is_string
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
    integer :: data_frames_size = 0
    integer, allocatable :: field_widths(:)

    contains
      procedure :: get => result_set__get
      procedure :: get_as_chars => result_set__get_as_chars
      procedure :: get_raw_values => result_set__get_raw_values
      procedure :: is_string => result_set__is_string
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
    data_field = DataField(String(""), String(""), .false., .false., null(), null(), null())

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
    result_set = ResultSet(null(), null(), 0, null())   ! Needed because of gfortran bug

    allocate(result_set%data_frames(0))
    allocate(result_set%names(0))
    allocate(result_set%field_widths(0))
  end function initialize__result_set


  function result_set__get(self, field_name, for) result(data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: for
    real(kind=8), allocatable :: data(:, :, :)

    block ! Check data type of field
      type(DataField), allocatable :: target_field

      target_field = self%data_frames(1)%field_for_node_named(String(field_name))
      if (target_field%is_string) then
        call bort(field_name // " is a string field. Use get_as_string to get its value")
      end if
    end block ! Check data type of field

    data = self%get_raw_values(field_name, for)
  end function result_set__get


  function result_set__get_as_chars(self, field_name, for) result(char_data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: for
    character(:), allocatable :: char_data(:)

    real(kind=8), allocatable :: dat(:, :, :)
    integer :: data_shape(3)

    block ! Check data type of field
      type(DataField), allocatable :: target_field

      target_field = self%data_frames(1)%field_for_node_named(String(field_name))
      if (.not. target_field%is_string) then
        call bort(field_name // " is a number field. Use get_as_number to get its value")
      end if
    end block ! Check data type of field

    dat = self%get_raw_values(field_name, for)
    data_shape = shape(dat)

    allocate(character(data_shape(2) * 8) :: char_data(data_shape(1)))
    char_data = ""

    ! Manually copy each element in the data array
    block ! Move data into char_data)
      integer :: data_row_idx, data_col_idx, char_idx
      integer :: char_cursor_pos
      integer(kind=8) :: data_int_rep

      do data_row_idx = 1, data_shape(1)
        char_cursor_pos = 1
        do data_col_idx = 1, data_shape(2)
          do char_idx = 0, 7
            data_int_rep = transfer(dat(data_row_idx, data_col_idx, 1), data_int_rep)
            char_data(data_row_idx)(char_cursor_pos:char_cursor_pos) = &
                    transfer(ibits(data_int_rep, char_idx*8, 8), "a")
            char_cursor_pos = char_cursor_pos + 1
          end do
        end do
      end do

    end block ! Move data into char_data
  end function result_set__get_as_chars


  function result_set__get_raw_values(self, field_name, for) result(data)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name
    character(len=*), intent(in), optional :: for
    real(kind=8), allocatable :: data(:, :, :)

    
    type ResultField
      real(kind=8), allocatable :: data(:, :)
    end type ResultField
    type(ResultField), allocatable :: result_fields(:)
    integer :: total_rows, total_cols


    block  ! Get Result Fields
      integer :: frame_idx, data_idx, rep_idx, row_idx, col_idx
      integer :: num_rows, num_cols
      type(DataField), allocatable :: target_field, for_field

      integer, allocatable :: rep_counts(:, :)

      allocate(result_fields(self%data_frames_size))

      total_rows = 1
      total_cols = 1
      do frame_idx = 1, self%data_frames_size
        target_field = self%data_frames(frame_idx)%field_for_node_named(String(field_name))

        if (.not. target_field%missing) then
          if (present(for) .and. for /= "") then
            for_field = self%data_frames(frame_idx)%field_for_node_named(String(for))
            rep_counts = self%rep_counts(target_field, for_field)

            num_rows = sum(rep_counts(2, :))
            num_cols = maxval(rep_counts(1, :))
            total_cols = max(num_cols, total_cols)

            allocate(result_fields(frame_idx)%data(num_rows, num_cols))
            result_fields(frame_idx)%data = MissingValue

            ! If there is a single column it means the data is target field
            ! is shallower than the for field. In this case we need to
            ! copy the data for the number of for field repeats. The result 
            ! is that the field_data is a single column whose length is the
            ! sum of all the for field repeats.
            if (num_cols == 1) then
              do data_idx = 1, size(rep_counts(1, :))  
                do rep_idx = 1, rep_counts(2, data_idx)
                  result_fields(frame_idx)%data(sum(rep_counts(2, 1:data_idx - 1)) + rep_idx, 1) = &
                    target_field%data(data_idx)
                end do
              end do

            ! If there is a single row per target element it means the target 
            ! field is deeper than the for field. In this case the for field data 
            ! ends up being a 2d array. The number of columns is the number of 
            ! repeats of the target field associated one of the for field elements.
            ! The number of rows is consistent with the number of for field elements. 
            else if (all(rep_counts(2, :) == 1)) then
              do row_idx = 1, num_rows
                result_fields(frame_idx)%data(row_idx, 1:rep_counts(1, row_idx)) = &
                  target_field%data(1:rep_counts(1, row_idx))
              end do
            end if

            ! data(data_row_idx : data_row_idx + num_rows, 1:num_cols, 1) = field_data
            total_rows = total_rows + num_rows

          else
            allocate(result_fields(frame_idx)%data(1, size(target_field%data)))
            result_fields(frame_idx)%data(1, :) = target_field%data

            ! data(frame_idx, 1:size(target_field%data), 1) = target_field%data
          end if
        end if

        if (allocated(target_field)) deallocate(target_field)
        if (allocated(for_field)) deallocate(for_field)
      end do
    end block  ! Get Result Fields

    block  ! Make Output Data
      integer :: data_row_idx
      integer :: field_idx
      integer :: data_shape(2)

      allocate(data(total_rows, total_cols, 1))

      data_row_idx = 1
      do field_idx = 1, size(result_fields)
        data_shape = shape(result_fields(field_idx)%data)
        data(data_row_idx:data_row_idx + data_shape(1), 1:data_shape(2), 1) = &
            result_fields(field_idx)%data

        data_row_idx = data_row_idx + data_shape(1)
      end do
    end block  ! Make Output Data
  end function result_set__get_raw_values
  
  
  ! Check if the field is a string
  function result_set__is_string(self, field_name) result(is_string)
    class(ResultSet), intent(in) :: self
    character(len=*), intent(in) :: field_name

    type(DataField), allocatable :: target_field
    logical :: is_string
    is_string = .false.
      
    target_field = self%data_frames(1)%field_for_node_named(String(field_name))
    is_string = target_field%is_string
  end function result_set__is_string


  ! @brief Compute the total number of elements that exist in the target field for a "for" field.
  !        Example 1:
  !          Lets say we have a field with the following sequence counts:
  !            root:                 1
  !            seq_1 (for field):    3
  !            seq_2:                1, 2, 1
  !            seq_3 (target field): 2, 2, 1, 2
  !
  !          In this case the target field is deeper than the for field. So we want to group the
  !          target field by the for field that the target corresponds to. The total number of 
  !          target values is 7 (sum of target field counts), while there are 3 for field values. 
  !          The sequence count table indicates that for the first for field value there are 2
  !          target field values (1 -> 3 -> 1 -> 2). The second for field value corresponds the next 
  !          3 target field values (1 -> 3 -> 2 -> (2, 1)). The third for field value corresponds to 
  !          the last 2 target field value, and so on (1 -> 3 -> 1 -> 2).
  !
  !          Writing the result in table form we can represent the result as follows:
  !
  !                                 Target Counts | For Counts
  !                                 ---------------------------
  !                                             2 | 1
  !                                             3 | 1
  !                                             2 | 1
  !            
  !        Example 2:
  !          Lets say we have a field with the following sequence counts:
  !            root:                 1
  !            seq_1 (target field): 3
  !            seq_2:                1, 2, 1
  !            seq_3 (for field):    2, 2, 1, 2
  !          
  !          In this case the target field is shallower than the for field. Basically what this
  !          means is that each field in the target field will be repeated by the number of times
  !          indicated by the for field. So in the example above there are a total number of 3
  !          target field values (sum of target_field counts). The number of counts in the for field
  !          corresponding to the first target value is 2, corresponding to the first for field
  !          count. The number of counts of the second target value is 2, corresponding to the
  !          second for field count, and so on.
  !
  !          Writing the result in table form we can represent the result as follows:
  !
  !                                 Target Counts | For Counts
  !                                 ---------------------------
  !                                             1 | 2
  !                                             1 | 3
  !                                             1 | 2
  !
  ! @param[in] self - class(ResultSet) : the class instance.
  ! @param[in] target_field - type(DataField) : the target
  ! @param[in] for_field - type(DataField) : the "for" field
  ! @return counts - integer(2, :) : The relative counts between the target and for fields (see 
  !                                  brief).
  !
  function result_set__rep_counts(self, target_field, for_field) result(counts)
    class(ResultSet), intent(in) :: self
    type(DataField), intent(in) :: target_field
    type(DataField), intent(in) :: for_field

    integer, allocatable :: counts(:, :)
    integer :: seq_idx, rep_idx
    integer :: target_count
    integer :: count

    do seq_idx = 1, min(size(target_field%seq_path), size(for_field%seq_path))
      if (target_field%seq_path(seq_idx) /= for_field%seq_path(seq_idx)) then
        call bort("The target field " // target_field%name%chars() // " and the for field " &
                  // for_field%name%chars() // " don't occur along the same path.")
      end if
    end do

    ! The target is deeper than the for field
    if (size(target_field%seq_path) > size(for_field%seq_path)) then
      target_count =  sum(for_field%seq_counts(size(for_field%seq_counts))%counts)
      allocate(counts(2, target_count))

      do rep_idx = 1, target_count
        count = self%get_counts(target_field, &
                                size(for_field%seq_counts) + 1, &
                                1, &
                                rep_idx - 1)

        counts(1, rep_idx) = count
        counts(2, rep_idx) = 1
      end do

    ! The target is at the same level as the for field
    else if (size(target_field%seq_path) == size(for_field%seq_path)) then
      target_count =  sum(target_field%seq_counts(size(target_field%seq_counts))%counts)
      allocate(counts(2, target_count))

      counts = 1

    ! The target is shallower than the for field
    else
      target_count =  sum(target_field%seq_counts(size(target_field%seq_counts))%counts)
      allocate(counts(2, target_count))

      do rep_idx = 1, target_count
        count = self%get_counts(for_field, &
                                size(target_field%seq_counts) + 1, &
                                1, &
                                rep_idx - 1)

        counts(1, rep_idx) = 1
        counts(2, rep_idx) = count
      end do
    end if
  end function result_set__rep_counts


  ! @brief Recursively count the number of elements associated with a field coming from a specific
  !        sequence instance.
  !
  ! @param[in] self - class(ResultSet) : the class instance.
  ! @param[in] field - type(DataField) : the field whose reps we want to count to.
  ! @param[in] seq_idx - integer : the index of the sequence we are counting from.
  ! @param[in] last_count - integer : the count from the previous sequence.
  ! param[in] offset - integer : the offset inside the current sequence count list.
  ! @return count - integer : the number of reps
  !
  recursive function result_set__get_counts(self, field, seq_idx, last_count, offset) result(count)
    class(ResultSet), intent(in) :: self
    type(DataField), target, intent(in) :: field
    integer, intent(in) :: seq_idx
    integer, intent(in) :: last_count
    integer, intent(in) :: offset
    integer :: count

    integer, pointer :: seq_counts(:)
    integer :: cnt_idx

    count = 0
    if (seq_idx == size(field%seq_path)) then
      count = sum(field%seq_counts(seq_idx)%counts(offset + 1:offset + last_count))
    else
      seq_counts => field%seq_counts(seq_idx)%counts
      do cnt_idx = offset + 1, offset + last_count
        count = count + self%get_counts(field, &
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
          self%field_widths(name_idx) = max(size(field%data), self%field_widths(name_idx))
          name_found = .true.
          exit
        end if
      end do

      if (.not. name_found) then
        allocate(tmp_names(size(self%names) + 1))
        tmp_names(1:size(self%names)) = self%names
        tmp_names(size(tmp_names)) = field%name
        call move_alloc(tmp_names, self%names)

        self%field_widths = [self%field_widths, size(field%data)]
      end if
    end do

    if (self%data_frames_size >= size(self%data_frames)) then
      allocate(tmp_data_frames(self%data_frames_size + DataFrameResizeSize))
      tmp_data_frames(1:self%data_frames_size) = self%data_frames
      call move_alloc(tmp_data_frames, self%data_frames)
    end if

    self%data_frames_size = self%data_frames_size + 1
    self%data_frames(self%data_frames_size) = data_frame

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
