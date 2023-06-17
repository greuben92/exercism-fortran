module bob
  implicit none
contains

  function yelling(statement)
    logical :: yelling
    character(len=*), intent(in) :: statement
    integer :: i

    yelling = .false.
    do i = 1, len(statement)
      if (statement(i:i) >= 'A' .and. statement(i:i) <= 'Z') then
        yelling = .true.
      else if (statement(i:i) >= 'a' .and. statement(i:i) <= 'z') then
        yelling = .false.
        exit
      end if
    end do

  end function yelling

  function hey(statement)
    character(100) :: hey
    character(len=*), intent(in) :: statement
    integer :: l

    l = len_trim(statement)
    if (l .eq. 0) then
      hey = "Fine. Be that way!"
      return
    end if

    if (statement(l:l) == '?') then
      if (yelling(statement)) then
        hey = "Calm down, I know what I'm doing!"
      else
        hey = "Sure."
      end if
    else
      if (yelling(statement)) then
        hey = "Whoa, chill out!"
      else
        hey = "Whatever."
      end if
    end if

  end function hey

end module bob
