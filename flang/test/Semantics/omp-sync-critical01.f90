! RUN: %S/test_errors.sh %s %t %f18 -fopenmp
use omp_lib
  implicit none
  integer :: maxival  = -100, i, j
  integer :: sharedarray(10000) = (/ (j, j=1, 10000) /)
  !$omp parallel do num_threads(4)
  do i=1, size(sharedarray), 1
  !$omp critical
    ! Performance loss
    if (sharedarray(i) .gt. maxival) then
      maxival = sharedarray(i)
    end if
  !$omp end critical
  end do
  !$omp end parallel do
  print *, "Max val in array is = ", maxival

  !$omp parallel do num_threads(4)
  do i=1, size(sharedarray), 2
  !$omp critical (somename)
    if (sharedarray(i) .gt. maxival) then
      maxival = sharedarray(i)
    end if
  !ERROR: CRITICAL directive name required but missing
  !$omp end critical
  end do
  !$omp end parallel do
  print *, "Max val in array is = ", maxival

  !$omp parallel do num_threads(4)
  do i=1, size(sharedarray), 3
  !$omp critical
    if (sharedarray(i) .gt. maxival) then
      maxival = sharedarray(i)
    end if
  !ERROR: CRITICAL directive name unexpected
  !$omp end critical (somename)
  end do
  !$omp end parallel do
  print *, "Max val in array is = ", maxival

  !$omp parallel do num_threads(4)
  do i=1, size(sharedarray), 4
  !$omp critical(somename)
    if (sharedarray(i) .gt. maxival) then
      maxival = sharedarray(i)
    end if
  !$omp end critical(somename)
  end do
  !$omp end parallel do
  print *, "Max val in array is = ", maxival

  !$omp parallel do num_threads(4)
  do i=1, size(sharedarray), 5
  !$omp critical(somename)
    if (sharedarray(i) .gt. maxival) then
      maxival = sharedarray(i)
    end if
  !ERROR: CRITICAL directive name mismatch
  !$omp end critical(othername)
    end do
  !$omp end parallel do
  print *, "Max val in array is = ", maxival

  !Unless the effect is as if hint(omp_sync_hint_none) was specified, the
  !critical construct must specify a name.
  !$omp critical(foo) hint(omp_sync_hint_none)
  !$omp end critical(foo)

  !ERROR: CRITICAL construct must specify a name expect unless the effect is equivalent to specifying a HINT(omp_sync_hint_none)
  !$omp critical hint(omp_sync_hint_uncontended)
  !$omp end critical

  !$omp critical(foo) hint(omp_sync_hint_uncontended)
  !$omp end critical(foo)

  !$omp critical(foo)
  !$omp end critical(foo)

  !$omp critical(foo) hint(omp_sync_hint_none) hint(omp_sync_hint_none)
  !$omp end critical(foo)

  !ERROR: PRIVATE clause is not allowed on the END CRITICAL(FOO) directive
  !$omp critical(foo) hint(omp_sync_hint_none) private(i)
  !$omp end critical(foo)

end program
