Program test_timers

  USE ISO_FORTRAN_ENV, WP=>REAL64 
  USE timers

#define __JMAX__ 10000
#define __IMAX__ 10000

  Implicit NONE

  Real(WP) :: time_s, time_e, sum, delta

  Integer :: i, j

  Print *,'' 
  Print *,' *** Test of Fortran and C timer routines ***'
  Print *,'' 
  Print *,' C gettimeofday'
  Print *,'' 
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_gtd(time_s)
  Do j=1, __JMAX__ 
    Do i=1, __IMAX__
      sum = sum + REAL(i+j,WP) 
    End Do 
  End Do 
  Call timer_gtd(time_e)
  delta = time_e - time_s
  Write(*,'( " delta time C ctd                 = ", g0.15)') delta 

  Print *,'' 
  Print *,' C clock_gettime with MONOTONIC clock'
  Print *,'' 
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_cgt(time_s, CLOCK_MONOTONIC)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + REAL(i+j,WP) 
    End Do 
  End Do 
  Call timer_cgt(time_e, CLOCK_MONOTONIC)
  delta = time_e - time_s
  Write(*,'( " delta time C cgt monotonic       = ", g0.15)') delta 

  Print *,'' 
  Print *,' C clock_gettime with REALTIME clock'
  Print *,'' 
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_cgt(time_s, CLOCK_REALTIME)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + REAL(i+j,WP) 
    End Do 
  End Do 
  Call timer_cgt(time_e, CLOCK_REALTIME)
  delta = time_e - time_s
  Write(*,'( " delta time C cgt realtime        = ", g0.15)') delta 
#ifdef __linux__ 
  Print *,'' 
  Print *,' C clock_gettime with CLOCK_THREAD_CPUTIME_ID clock (Linux only)'
  Print *,'' 
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_cgt(time_s, CLOCK_THREAD_CPUTIME_ID)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + REAL(i+j,WP) 
    End Do 
  End Do 
  Call timer_cgt(time_e, CLOCK_THREAD_CPUTIME_ID)
  delta = time_e - time_s
  Write(*,'( " delta time C cgt thread cputime  = ", g0.15)') delta
#endif
 
  Print *,'' 
  Print *,' DATE_AND_TIME'
  Print *,'' 
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_dt(time_s)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + REAL(i+j,WP) 
    End Do 
  End Do 
  Call timer_dt(time_e)
  delta = time_e - time_s
  Write(*,'( " delta time DT                    = ", g0.15)') delta 

  Print *,'' 
  Print *,' CPU_TIME'
  Print *,'' 
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call CPU_TIME(time_s)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + REAL(i+j,WP) 
    End Do 
  End Do 
  Call CPU_TIME(time_e)
  delta = time_e - time_s
  Write(*,'( " delta time CPU                   = ", g0.15)') delta 

  Print *,'' 
  Print *,' SYSTEM_CLOCK'
  Print *,'' 
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_sc(time_s)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + REAL(i+j,WP) 
    End Do 
  End Do 
  Call timer_sc(time_e)
  delta = time_e - time_s
  Write(*,'( " delta time SC                    = ", g0.15)') delta
  Print *,''

  Print *,''
  Print *,' DATE_AND_TIME increment'
  Print *,''
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_dt(time_s)
  Do
    Call timer_dt(time_e)
    If (time_e > time_s) EXIT
  End Do
  delta = time_e - time_s
  Write(*,'( " delta time DT increment                    = ", g0.15)') delta
  Print *,''
  Print *,' SYSTEM CLOCK increment'
  Print *,''
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_sc(time_s)
  Do
    Call timer_sc(time_e)
    If (time_e > time_s) EXIT
  End Do
  delta = time_e - time_s
  Write(*,'( " delta time SC increment                    = ", g0.15)') delta
 
  Stop

End Program test_timers
