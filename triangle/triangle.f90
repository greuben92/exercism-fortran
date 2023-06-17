
module triangle
  implicit none

  interface equilateral
    module procedure equilateral_real
    module procedure equilateral_int
  end interface

  interface scalene
    module procedure scalene_real
    module procedure scalene_int
  end interface

  interface isosceles
    module procedure isosceles_real
    module procedure isosceles_int
  end interface

 contains

  logical function valid_triangle_real(edges)
    real,dimension(3) :: edges
    valid_triangle_real = sum(edges) - maxval(edges) > maxval(edges)
  end function

  logical function valid_triangle_int(edges)
    integer,dimension(3) :: edges
    valid_triangle_int = sum(edges) - maxval(edges) > maxval(edges)
  end function

  logical function equilateral_real(edges)
    real,dimension(3) :: edges
    equilateral_real = valid_triangle_real(edges) .and. all(edges == edges(1))
  end function

  logical function equilateral_int(edges)
    integer,dimension(3) :: edges
    equilateral_int = valid_triangle_int(edges) .and. all(edges == edges(1))
  end function

  logical function isosceles_real(edges)
    real,dimension(3) :: edges
    isosceles_real = valid_triangle_real(edges) .and. &
      (count(edges == edges(1)) >= 2 .or. count(edges == edges(2)) >= 2)
  end function

  logical function isosceles_int(edges)
    integer,dimension(3) :: edges
    isosceles_int = valid_triangle_int(edges) .and. &
      (count(edges == edges(1)) >= 2 .or. count(edges == edges(2)) >= 2)
  end function


  logical function scalene_real(edges)
    real,dimension(3) :: edges
    scalene_real = valid_triangle_real(edges) .and. &
      .not. isosceles_real(edges)
  end function

  logical function scalene_int(edges)
    integer,dimension(3) :: edges
    scalene_int = valid_triangle_int(edges) .and. &
      .not. isosceles_int(edges)
  end function

end module
