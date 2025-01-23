module m_assert

  implicit none
  private

  public :: assert

contains

#ifdef DEBUG_MODE
  subroutine assert(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
    if (.not. condition) then
      error stop trim(message)
    endif
  end subroutine assert
#else
  subroutine assert(condition, message)
    implicit none
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message
  end subroutine assert
#endif

end module m_assert
