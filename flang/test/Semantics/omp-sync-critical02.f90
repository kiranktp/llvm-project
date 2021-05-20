! RUN: %S/test_errors.sh %s %t %f18 -fopenmp
! see https://bugs.llvm.org/show_bug.cgi?id=48145

integer function timer_tick_sec()
  implicit none
  integer t

  !$OMP CRITICAL (foo)
  t = t + 1
  !$OMP END CRITICAL (foo)

  timer_tick_sec = t
  return

end function timer_tick_sec
