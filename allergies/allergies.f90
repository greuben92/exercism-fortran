
module allergies
  implicit none

  character(len=12), dimension(8) :: allergens = [ character(len=12) :: &
      "eggs", "peanuts", "shellfish", &
      "strawberries", "tomatoes", &
      "chocolate", "pollen", "cats" &
    ]

contains

  logical function allergicTo(allergy_str, allergy_key)
    character(len=*), intent(in) :: allergy_str
    integer, intent(in) :: allergy_key
    integer :: i

    i = 2 ** (findloc(allergens, allergy_str, dim=1)-1)
    allergicTo = iand(allergy_key, i) == i
  end function

  function allergicList(allergy_key)
    integer, intent(in) :: allergy_key
    character(len=100) :: allergicList
    integer :: i

    allergicList = ' '
    do i = 1, size(allergens)
      if (allergicTo(trim(allergens(i)), allergy_key)) then
        allergicList(len_trim(allergicList)+2:) = allergens(i)
      end if
    end do
    allergicList = allergicList(2:)
  end function

end module
