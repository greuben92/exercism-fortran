
module space_age
  implicit none
contains

  double precision function age_in_years(planet, seconds)
    character(len=*), intent(in) :: planet
    double precision, intent(in) :: seconds
    double precision :: p

    select case(planet)
      case ("Mercury")
        p = 0.2408467d0
      case ("Venus")
        p = 0.61519726d0
      case("Earth")
        p = 1.0d0
      case ("Mars")
        p = 1.8808158d0
      case ("Jupiter")
        p = 11.862615d0
      case ("Saturn")
        p = 29.447498d0
      case ("Uranus")
        p = 84.016846d0
      case ("Neptune")
        p = 164.79132d0
    end select

    age_in_years = seconds / (365.25 * 24 * 60 * 60 * p)
  end function

end module
