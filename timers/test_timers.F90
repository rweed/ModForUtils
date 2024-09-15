Program test_timers

#ifndef __JMAX__
#define __JMAX__ 10000
#endif
#ifndef __IMAX__
#define __IMAX__ 10000
#endif

  USE ISO_FORTRAN_ENV, WP=>REAL64 
  USE timers

  Implicit NONE

  Integer  :: niters
  Real(WP) :: time_s, time_e, sum, delta

  Character(22) :: buf

  Integer :: i, j

  niters = (__IMAX__)*(__JMAX__)

  Print *,'' 
  Print *,' *** Test of Fortran and C timer routines ***'
  Print *,''
  Write (*, '(" Summation loops elapsed times for ",i0," iterations")') niters 
  Print *,''
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_dt(time_s)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + Real(i+j,WP) 
    End Do 
  End Do 
  Call timer_dt(time_e)
  delta = time_e - time_s
  Write(*,'( " DATE_AND_TIME                    = ", 1PE22.15)') delta 
  Write(buf,'(1PE22.15)') sum

  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call CPU_TIME(time_s)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + Real(i+j,WP) 
    End Do 
  End Do 
  Call CPU_TIME(time_e)
  delta = time_e - time_s
  Write(*,'( " CPU_TIME                         = ", 1PE22.15)') delta 
  Write(buf,'(1PE22.15)') sum

  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_sc(time_s)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + Real(i+j,WP) 
    End Do 
  End Do 
  Call timer_sc(time_e)
  delta = time_e - time_s
  Write(*,'( " SYSTEM_CLOCK                     = ", 1PE22.15)') delta
  Write(buf,'(1PE22.15)') sum

  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_gtd(time_s)
  Do j=1, __JMAX__ 
    Do i=1, __IMAX__
      sum = sum + Real(i+j,WP) 
    End Do 
  End Do 
  Call timer_gtd(time_e)
  delta = time_e - time_s
  Write(*,'( " C gettimeofday                   = ", 1PE22.15)') delta 
  Write(buf,'(1PE22.15)') sum

  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_cgt(time_s, CLOCK_MONOTONIC)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + Real(i+j,WP) 
    End Do 
  End Do 
  Call timer_cgt(time_e, CLOCK_MONOTONIC)
  delta = time_e - time_s
  Write(*,'( " C clock_gettime (MONOTONIC)      = ", 1PE22.15)') delta 
  Write(buf,'(1PE22.15)') sum

  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_cgt(time_s, CLOCK_REALTIME)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + Real(i+j,WP) 
    End Do 
  End Do 
  Call timer_cgt(time_e, CLOCK_REALTIME)
  delta = time_e - time_s
  Write(*,'( " C clock_gettime (REALTIME)       = ", 1PE22.15)') delta 
  Write(buf,'(1PE22.15)') sum

#ifdef __linux__ 
  sum    = 0.0_WP
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_cgt(time_s, CLOCK_THREAD_CPUTIME_ID)
  Do j=1, __JMAX__
    Do i=1, __IMAX__
      sum = sum + Real(i+j,WP) 
    End Do 
  End Do 
  Call timer_cgt(time_e, CLOCK_THREAD_CPUTIME_ID)
  delta = time_e - time_s
  Write(*,'( " C clock_gettime (THREAD_CPUTIME) = ", 1PE22.15," Linux only")') delta 
  Write(buf,'(1PE22.15)') sum
#endif
 
  Print *,''
  Print *,'DATE_AND_TIME, CPU_TIME and SYSTEM_CLOCK increments'
  Print *,''
  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_dt(time_s)
  Do
    Call timer_dt(time_e)
    If (time_e > time_s) EXIT
  End Do
  delta = time_e - time_s
  Write(*,'( " DATE_AND_TIME increment          = ", 1PE22.15)') delta

  time_e = 0.0_WP
  time_s = 0.0_WP
  Call CPU_TIME(time_s)
  Do
    Call CPU_TIME(time_e)
    If (time_e > time_s) EXIT
  End Do
  delta = time_e - time_s
  Write(*,'( " CPU_TIME increment               = ", 1PE22.15)') delta

  time_e = 0.0_WP
  time_s = 0.0_WP
  Call timer_sc(time_s)
  Do
    Call timer_sc(time_e)
    If (time_e > time_s) EXIT
  End Do
  delta = time_e - time_s
  Write(*,'( " SYSTEM_CLOCK increment           = ", 1PE22.15)') delta
  Print *,'' 

  Stop

End Program test_timers
