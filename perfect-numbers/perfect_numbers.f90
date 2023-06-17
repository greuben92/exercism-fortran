
module perfect_numbers
  implicit none

contains

  character(len=9) function classify(num)
    integer, intent(in) :: num
    integer :: i, aliquot

    if (num < 1) then
      classify = "ERROR"
      return
    end if

    if (num == 1) then
      classify = "deficient"
      return
    end if

    aliquot = 1
    do i = 2, num/2
      if (mod(num, i) == 0) aliquot = aliquot + i
    end do

    if (aliquot == num) then
      classify = "perfect"
    else if (aliquot > num) then
      classify = "abundant"
    else if (aliquot < num) then
      classify = "deficient"
    else
      classify = "ERROR"
    end if
  end function

end module
