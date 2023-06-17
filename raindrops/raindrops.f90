module raindrops
  implicit none
contains

  function convert(i)
    integer :: i
    character(20) :: convert

    if (mod(i, 105) == 0) then
      convert = "PlingPlangPlong"
    else if (mod(i, 35) == 0) then
      convert = "PlangPlong"
    else if (mod(i, 21) == 0) then
      convert = "PlingPlong"
    else if (mod(i, 15) == 0) then
      convert = "PlingPlang"
    else if (mod(i, 7) == 0) then
      convert = "Plong"
    else if (mod(i, 5) == 0) then
      convert = "Plang"
    else if (mod(i, 3) == 0) then
      convert = "Pling"
    else
      write(convert, '(I0)') i
    endif
  end function convert

end module raindrops
