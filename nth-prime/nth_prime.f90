module nth_prime
  implicit none
contains

  ! get nth prime
  integer function prime(n)
    integer, intent(in) :: n
    integer, dimension(n) :: primes
    integer :: i, count

    if (n < 1) then
      prime = -1
      return
    end if

    primes(1:n) = 2
    i = 1
    count = 1

    do while(count < n)
      i = i + 2
      if (any(mod(i, primes) == 0)) cycle
      count = count + 1
      primes(count) = i
    end do

    prime = primes(n)
  end function

end module
