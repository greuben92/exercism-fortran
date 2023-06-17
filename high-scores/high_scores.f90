
module high_scores
  implicit none
contains

  function scores(score_list)
    integer, dimension(:) :: score_list
    integer, allocatable :: scores(:)
    scores = score_list
  end function

  integer function latest(score_list)
    integer, dimension(:) :: score_list
    latest = score_list(size(score_list))
  end function latest

  integer function personalBest(score_list)
    integer, dimension(:) :: score_list
    personalBest = maxval(score_list)
  end function personalBest

  function personalTopThree(score_list)
    integer, dimension(:) :: score_list
    integer, allocatable :: sorted(:)
    integer, dimension(3) :: personalTopThree
    integer :: l

    l = size(score_list)
    sorted = score_list
    call quickSort(sorted, 1, l)
    if (l >= 3) then
      personalTopThree = sorted(1:3)
    else
      personalTopThree = sorted
      personalTopThree(l+1:) = 0
    end if
  end function personalTopThree

  recursive subroutine quickSort(arr, low, high)
    integer, dimension(:), intent(inout) :: arr
    integer :: low, high, p

    if (low < high) then
      p = partition(arr, low, high)
      call quickSort(arr, low, p - 1)
      call quickSort(arr, p + 1, high)
    end if
  end subroutine quickSort

  integer function partition(arr, low, high)
    integer, dimension(:), intent(inout) :: arr
    integer :: low, high, pivot, i, j

    pivot = arr(high)
    i = low - 1
    do j = low, high - 1
      if (arr(j) > pivot) then
        i = i + 1
        call swap(arr, i, j)
      end if
    end do

    i = i + 1
    call swap(arr, i, high)
    partition = i
  end function partition

  subroutine swap(arr, i, j)
    integer, dimension(:), intent(inout) :: arr
    integer :: i, j, tmp
      tmp = arr(i)
      arr(i) = arr(j)
      arr(j) = tmp
  end subroutine swap
  
end module
