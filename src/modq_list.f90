module modq_list
  use modq_string
  implicit none

  integer, parameter :: ResizeSizes(*) = (/ 10, 100, 1000, 10000, 100000 /)

  type, public:: IntList
    private
    integer, pointer :: values(:) => null()
    integer :: size
    integer :: resize_idx

  contains
    procedure, public :: push => int_list__push
    procedure, public :: pop => int_list__pop
    procedure, public :: at => int_list__at
    procedure, public :: array => int_list__array
    procedure, public :: length => int_list__length
    procedure, public :: resize => int_list__resize
    procedure, public :: delete => int_list__delete
    procedure, public :: print => int_list__print
    final :: int_list__final
  end type IntList

  interface IntList
    module procedure :: initialize__int_list
    module procedure :: initialize__int_list_w_ints
  end interface IntList

  contains

    ! Int List Class methods
    type(IntList) function initialize__int_list() result(new_list)
      new_list = IntList(null(), 0, 0)
    end function initialize__int_list


    type(IntList) function initialize__int_list_w_ints(ints) result(new_list)
      integer :: ints(:)
      integer :: idx

      new_list = IntList(null(), 0, 0)
      do idx = 1, size(ints)
        call new_list%push(ints(idx))
      end do
    end function initialize__int_list_w_ints


    subroutine int_list__push(self, value)
      class(IntList), intent(inout) :: self
      integer, intent(in) :: value

      if (.not. associated(self%values)) then
        self%resize_idx = self%resize_idx + 1
        allocate(self%values(ResizeSizes(self%resize_idx)))
      else if (size(self%values) == self%size) then
        if (self%resize_idx < size(ResizeSizes)) then
          self%resize_idx = self%resize_idx + 1
        end if

        call self%resize(ResizeSizes(self%resize_idx) + size(self%values))
      end if

      self%values(self%size + 1) = value
      self%size = self%size + 1
    end subroutine int_list__push


    subroutine int_list__pop(self, value)
      class(IntList), intent(inout) :: self
      integer, optional, pointer, intent(inout) :: value

      if (self%size > 0) then
        if (present(value)) then
          value => self%values(self%size)
        end if

        self%size = self%size - 1
      else if (present(value)) then
        value => null()
      end if

    end subroutine int_list__pop


    function int_list__at(self, idx) result(val)
      class(IntList), intent(in) :: self
      integer, intent(in) :: idx
      integer, pointer :: val

      if (sizeof(self%values) < idx) then
        call bort("IntList: Index out of range.")
      end if

      val => self%values(idx)
    end function int_list__at


    function int_list__array(self) result(values)
      class(IntList), intent(in) :: self
      integer, pointer :: values(:)
      values => self%values(1:self%size)
    end function int_list__array


    integer function int_list__length(self) result(length)
      class(IntList), intent(in) :: self
      length = self%size
    end function int_list__length


    subroutine int_list__resize(self, new_size)
      class(IntList), intent(inout) :: self
      integer, intent(in) :: new_size
      integer, pointer :: tmp(:)

      if (new_size < self%size) then
        call bort("IntList: Truncating Data during resize.")
      end if

      allocate(tmp(new_size))

      if(associated(self%values)) then
        tmp(1:self%size) = self%values(1:self%size)
        deallocate(self%values)
        self%values => null()
      end if

      self%values => tmp
    end subroutine

    subroutine int_list__delete(self)
      class(IntList), intent(inout) :: self

      if (associated(self%values)) then
        deallocate(self%values)
        nullify(self%values)
      end if
    end subroutine int_list__delete


    subroutine int_list__final(self)
      type(IntList), intent(inout) :: self

      if (associated(self%values)) then
        deallocate(self%values)
        nullify(self%values)
      end if
    end subroutine int_list__final


    subroutine int_list__print(self)
        class(IntList), intent(in) :: self

        integer :: idx

        do idx = 1, self%length()
          print *, self%at(idx)
        end do
    end subroutine

end module modq_list
