module modq_string
  type String
    character(len=:), allocatable :: chars

  contains
!    procedure :: delete => string__delete
  end type String

  contains

!  subroutine string__delete(self)
!    type(String), intent(inout) :: self
!
!    deallocate(self%chars)
!  end subroutine string__delete
end module modq_string
