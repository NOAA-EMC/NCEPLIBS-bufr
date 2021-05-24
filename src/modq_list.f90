module modq_list
  use modq_string
  implicit none

  type :: Container
    class(*), allocatable :: item
  end type

  type, abstract :: ListBase
    !    private
    type(Container), allocatable :: data(:)
    integer :: size = 0

  contains
    procedure, public :: count => list_base__count
    procedure, private :: append_base => list_base__append
    procedure, private :: at_base => list_base__at
    procedure, private :: set_base => list_base__set
    procedure, private :: delete => list_base__delete
  end type ListBase


  type, extends(ListBase) :: List
  contains
    procedure, public :: append => generic_list__append
    procedure, public :: at => generic_list__at
    procedure, public :: set => generic_list__set
    final :: generic__delete
  end type List

  interface GenericList
    module procedure :: initialize__generic_list
  end interface GenericList


  type, extends(ListBase) :: IntList
  contains
    procedure, public :: append => int_list__append
    procedure, public :: at => int_list__at
    procedure, public :: set => int_list__set
    final :: int_list__delete
  end type IntList

  interface IntList
    module procedure :: initialize__int_list
    module procedure :: initialize__int_list_w_ints
  end interface IntList


  type, extends(ListBase) :: IntListList
  contains
    procedure, public :: append => int_list_list__append
    procedure, public :: at => int_list_list__at
    procedure, public :: set => int_list_list__set
    final :: int_list_list__delete
  end type IntListList

  interface IntListList
    module procedure :: initialize__int_list_list
  end interface IntListList


  type, extends(ListBase) :: StringList
  contains
    procedure, public :: append => string_list__append
    procedure, public :: at => string_list__at
    procedure, public :: set => string_list__set
    final :: string_list__delete
  end type StringList

  interface StringList
    module procedure :: initialize__string_list
  end interface StringList

contains

  ! List Base Class methods
  subroutine list_base__append(self, item)
    class(ListBase), intent(inout) :: self
    class(*), intent(in) :: item

    type(Container) :: new_container

    if (.not. allocated(self%data)) then
      allocate(self%data(1000))
    end if

    self%size = self%size + 1
    allocate(new_container%item, source=item)
    self%data(self%size) = new_container
  end subroutine list_base__append


  function list_base__at(self, idx) result(item)
    class(ListBase), intent(in) :: self
    integer, intent(in) :: idx
    class(*), allocatable :: item
    item = self%data(idx)%item
  end function list_base__at


  subroutine list_base__set(self, idx, val)
    class(ListBase), intent(inout) :: self
    integer, intent(in) :: idx
    class(*), intent(in) :: val

    if (size(self%data) < idx) then
      error stop "ListBase: Index out of range."
    end if

    deallocate(self%data(idx)%item)
    allocate(self%data(idx)%item, source=val)
  end subroutine list_base__set


  integer function list_base__count(self) result(count)
    class(ListBase), intent(in) :: self
    count = self%size
  end function list_base__count


  subroutine list_base__delete(self)
    class(ListBase), intent(inout) :: self
    integer :: data_idx

    do data_idx = 1, self%size
      if (allocated(self%data(data_idx)%item)) then
        deallocate(self%data(data_idx)%item)
      end if
    end do

    if (allocated(self%data)) then
      deallocate(self%data)
    end if
  end subroutine list_base__delete


  ! Generic List Class methods
  type(List) function initialize__generic_list() result(new_list)
    new_list = List(null(), 0)
  end function initialize__generic_list


  subroutine generic_list__append(self, item)
    class(List), intent(inout) :: self
    class(*), intent(in) :: item

    call self%append_base(item)
  end subroutine generic_list__append


  function generic_list__at(self, idx) result(item)
    class(List), intent(inout) :: self
    integer, intent(in) :: idx
    class(*), allocatable :: item

    item = self%at_base(idx)
  end function generic_list__at


  subroutine generic_list__set(self, idx, val)
    class(List), intent(inout) :: self
    integer, intent(in) :: idx
    class(*) :: val

    call self%set_base(idx, val)
  end subroutine generic_list__set


  subroutine generic__delete(self)
    type(List), intent(inout) :: self

    call self%delete()
  end subroutine generic__delete


  ! Int List Class methods
  type(IntList) function initialize__int_list() result(new_list)
    new_list = IntList(null(), 0)
  end function initialize__int_list

  type(IntList) function initialize__int_list_w_ints(ints) result(new_list)
    integer :: ints(:)

    integer :: idx

    new_list = IntList(null(), 0)
    do idx = 1, size(ints)
      call new_list%append(ints(idx))
    end do
  end function initialize__int_list_w_ints


  subroutine int_list__append(self, item)
    class(IntList), intent(inout) :: self
    integer, intent(in) :: item

    call self%append_base(item)
  end subroutine int_list__append


  function int_list__at(self, idx) result(item)
    class(IntList), intent(in) :: self
    integer, intent(in) :: idx
    integer, allocatable :: item

    item = transfer(self%at_base(idx), item)
  end function int_list__at


  subroutine int_list__set(self, idx, val)
    class(IntList), intent(inout) :: self
    integer, intent(in) :: idx
    integer :: val

    if (size(self%data) < idx) then
      error stop "IntList: Index out of range."
    end if

    self%data(idx)%item = val ! avoid the manual copy of value type
  end subroutine int_list__set


  subroutine int_list__delete(self)
    type(IntList), intent(inout) :: self

    call self%delete()
  end subroutine int_list__delete


  ! Int List List Class methods
  type(IntListList) function initialize__int_list_list() result(new_list)
    new_list = IntListList(null(), 0)
  end function initialize__int_list_list


  subroutine int_list_list__append(self, item)
    class(IntListList), intent(inout) :: self
    type(IntList), intent(in) :: item

    call self%append_base(item)
  end subroutine int_list_list__append


  function int_list_list__at(self, idx) result(item)
    class(IntListList), intent(in) :: self
    integer, intent(in) :: idx
    type(IntList), allocatable :: item

    item = transfer(self%at_base(idx), item)
  end function int_list_list__at


  subroutine int_list_list__set(self, idx, val)
    class(IntListList), intent(inout) :: self
    integer, intent(in) :: idx
    type(IntList) :: val

    call self%set_base(idx, val)
  end subroutine int_list_list__set


  subroutine int_list_list__delete(self)
    type(IntListList), intent(inout) :: self

    call self%delete()
  end subroutine int_list_list__delete


  ! String List Class methods
  type(StringList) function initialize__string_list() result(new_list)
    new_list = StringList(null(), 0)
  end function initialize__string_list


  subroutine string_list__append(self, item)
    class(StringList), intent(inout) :: self
    type(String), intent(in) :: item

    call self%append_base(item)
  end subroutine string_list__append


  function string_list__at(self, idx) result(item)
    class(StringList), intent(in) :: self
    integer, intent(in) :: idx
    type(String), allocatable :: item

    item = transfer(self%at_base(idx), item)
  end function string_list__at


  subroutine string_list__set(self, idx, val)
    class(StringList), intent(inout) :: self
    integer, intent(in) :: idx
    type(String) :: val

    call self%set_base(idx, val)
  end subroutine string_list__set


  subroutine string_list__delete(self)
    type(StringList), intent(inout) :: self

    call self%delete()
  end subroutine string_list__delete

end module modq_list