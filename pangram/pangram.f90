module pangram
  implicit none
contains

  logical function is_pangram(sentance)
    character(*) :: sentance
    integer :: i, a
    logical :: alphabets(26)

    alphabets(:) = .false.
    do i = 1, len(sentance)
      a = ior(ichar(sentance(i:i)), 32) - 96
      if (a > 0 .and. a < 27) then
        alphabets(a) = .true.
      end if
    end do

    is_pangram = all(alphabets .eqv. .true.)

   end function is_pangram

end module pangram
