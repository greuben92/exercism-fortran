
module rational_numbers
  implicit none
contains

  function add(r1,r2)
    integer,dimension(2) :: add, r1,r2
    add = reduce([r1(1) * r2(2) + r2(1) * r1(2), r1(2) * r2(2)])
  end function

  function sub(r1,r2)
    integer,dimension(2) :: sub, r1,r2
    sub = reduce([r1(1) * r2(2) - r2(1) * r1(2), r1(2) * r2(2)])
  end function

  function mul(r1,r2)
    integer,dimension(2) :: mul, r1,r2
    mul = reduce(r1 * r2)
  end function

  function div(r1,r2)
    integer,dimension(2) :: div, r1,r2
    div = reduce([r1(1) * r2(2), r2(1) * r1(2)])
  end function

  function rational_abs(r1)
    integer,dimension(2) :: rational_abs, r1
    rational_abs = abs(r1)
  end function

  function rational_to_pow(r1, ex)
    integer,dimension(2) :: rational_to_pow, r1
    integer :: ex
    rational_to_pow = r1 ** ex
  end function

  function real_to_rational_pow(ex,r1)
    integer,dimension(2) ::  r1
    real :: real_to_rational_pow,ex,m
    real_to_rational_pow = (ex**r1(1))**(1./r1(2))
  end function

  function reduce(r1)
    integer,dimension(2) :: reduce, r1
    integer :: a, b, c

    if (r1(1) == 0) then
      reduce = [0, 1]
      return
    end if

    a = maxval(abs(r1))
    b = minval(abs(r1))
    c = b

    do
      c = mod(a, b)
      if (c == 0) exit
      a = b
      b = c
    end do

    if (all(mod(r1, b) == 0)) then
      reduce = r1 / b * sign(1,r1(2))
    else
      reduce = r1
    end if
  end function

end module
