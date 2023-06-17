module hamming
  implicit none
contains

  function compute(strand1, strand2, distance)
      character(*) :: strand1, strand2
      integer :: distance
      logical :: compute
      integer :: l, i

      distance = 0
      compute = .false.
      l = len(strand1)

      if (l /= len(strand2)) return

      compute = .true.
      do i = 1, l
        if (strand1(i:i) /= strand2(i:i)) distance = distance + 1
      end do

  end function compute

end module hamming
