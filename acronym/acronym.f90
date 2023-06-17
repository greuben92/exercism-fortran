
module acronym
  implicit none
contains

  function abbreviate(s)
    character(len=*), intent(in) :: s
    character(len=len_trim(s)) :: abbreviate
    integer :: i, j, c

    abbreviate=s(1:1)
    j = 2
    do i = 2, len_trim(s)
      if (s(i:i) == ' ' .or. s(i:i) == '-' .or. s(i:i) == '_') then
        c = iand(ichar(s(i+1:i+1)), NOT(32))
        if (c >= 65 .and. c <= 91) then
          abbreviate(j:j) = char(c)
          j = j + 1
        end if
      end if
    end  do
  end function

end module
