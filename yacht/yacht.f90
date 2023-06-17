
module yacht
  implicit none

  ! | Ones | 1 × number of ones | Any combination	| 1 1 1 4 5 scores 3 |
  ! | Twos | 2 × number of twos | Any combination | 2 2 3 4 5 scores 4 |
  ! | Threes | 3 × number of threes | Any combination | 3 3 3 3 3 scores 15 |
  ! | Fours | 4 × number of fours | Any combination | 1 2 3 3 5 scores 0 |
  ! | Fives | 5 × number of fives| Any combination | 5 1 5 2 5 scores 15 |
  ! | Sixes | 6 × number of sixes | Any combination | 2 3 4 5 6 scores 6 |
  ! | Full House | Total of the dice | Three of one number and two of another | 3 3 3 5 5 scores 19 |
  ! | Four of a Kind | Total of the four dice | At least four dice showing the same face | 4 4 4 4 6 scores 16 |
  ! | Little Straight |  30 points | 1-2-3-4-5 | 1 2 3 4 5 scores 30 |
  ! | Big Straight | 30 points | 2-3-4-5-6 | 2 3 4 5 6 scores 30 |
  ! | Choice | Sum of the dice | Any combination | 2 3 3 4 6 scores 18 |
  ! | Yacht | 50 points | All five dice showing the same face | 4 4 4 4 4 scores 50 |


contains

  integer function score(dice, yacht_type)
    integer, dimension(5), intent(in) :: dice
    character(len=*), intent(in) :: yacht_type
    integer :: i, j

    score=0

    select case(yacht_type)
      case ("ones")
        score = count(dice == 1)
      case ("twos")
        score = 2 * count(dice == 2)
      case ("threes")
        score = 3 * count(dice == 3)
      case ("fours")
        score = 4 * count(dice == 4)
      case ("fives")
        score = 5 * count(dice == 5)
      case ("sixes")
        score = 6 * count(dice == 6)
      case ("full house")
        outer: do i = 1, 6
          if (count(dice == i) == 3) then
            do j = 1, 6
              if (count(dice == j) == 2) then
                score = 3 * i + 2 * j
                exit outer
              end if
            end do
            exit outer
          end if
        end do outer
      case ("four of a kind")
        do i = 1, 6
          if (count(dice == i) >= 4) then
            score = 4 * i
            exit
          end if
        end do
      case ("little straight")
        if ( &
          any(dice == 1) &
          .and. any(dice == 2) &
          .and. any(dice == 3) &
          .and. any(dice == 4) &
          .and. any(dice == 5) &
        ) then
          score = 30
        end if
      case ("big straight")
        if ( &
          any(dice == 2) &
          .and. any(dice == 3) &
          .and. any(dice == 4) &
          .and. any(dice == 5) &
          .and. any(dice == 6) &
        ) then
          score = 30
        end if
      case ("choice")
        score = sum(dice)
      case ("yacht")
        if (all(dice == dice(1))) then
          score = 50
        end if
    end select
  end function


end module
