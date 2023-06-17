
module armstrong_numbers
  implicit none
contains

  logical function isArmstrongNumber(i)
    integer, intent(in) :: i
    integer :: digits, rem, tmp, n

    if (i < 10) then
      isArmstrongNumber = .true.
      return
    end if

    digits = int(log10(real(i))) + 1

    tmp = i
    n = 0
    do while(tmp /= 0)
      rem = mod(tmp, 10)
      n = n + rem ** digits
      tmp = tmp / 10
    end do

    isArmstrongNumber = n == i

  end function

end module
