module reverse_string
  implicit none
contains
  function reverse(str)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: reverse
    integer :: i, s

    s = len(str)
    do i = 1, s
      reverse(i:i) = str(s-i+1:s-i+1)
    end do
  end function reverse
end module reverse_string
