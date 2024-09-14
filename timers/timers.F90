Module timers

  USE ISO_FORTRAN_ENV, WP=>REAL64
  USE ISO_C_BINDING,   ONLY: C_INT, C_LONG, C_PTR, C_NULL_PTR, C_DOUBLE

  Implicit NONE
  PRIVATE
! Define some clock ids for C clock_gettime function

  Integer(C_INT), Parameter :: CLOCK_REALTIME           = 0_C_INT
  Integer(C_INT), Parameter :: CLOCK_MONOTONIC          = 1_C_INT
! These are Linux only
#ifdef __linux__
  Integer(C_INT), Parameter :: CLOCK_PROCESS_CPUTIME_ID = 2_C_INT
  Integer(C_INT), Parameter :: CLOCK_THREAD_CPUTIME_ID  = 3_C_INT
  Integer(C_INT), Parameter :: CLOCK_TAI                = 11_C_INT
#endif

! Define a C Interoperable versions of Linux timeval struct for gettimeofday

  Type, BIND(C)     :: timeval_c
    Integer(C_INT)  :: tv_sec
    Integer(C_LONG) :: tv_usec
  End Type

! Define a C Interoperable versions of Linux timespec struct for clock_gettime 

  Type, BIND(C)     :: timespec_c
    Integer(C_INT)  :: tv_sec
    Integer(C_LONG) :: tv_nsec
  End Type

! Define interface to Linux/Posix gettimeofday procedure

  Interface
    Function gettimeofday_c(timeval, timezone) BIND(C, NAME="gettimeofday")

      Import                         :: timeval_c, C_PTR, C_INT
      Type(timeval_c), Intent(INOUT) :: timeval
      Type(C_PTR),     Intent(IN)    :: timezone
      Integer(C_INT)                 :: gettimeofday_c 
    End Function gettimeofday_c
  End Interface

  Interface 
    Function clock_gettime_c(clockid, tp) BIND(C,NAME="clock_gettime")

      Import                                :: timespec_c, C_INT

      Integer(C_INT),   VALUE               :: clockid
      Type(timespec_c),       Intent(INOUT) :: tp
      Integer(C_INT)                        :: clock_gettime_c

    End Function clock_gettime_c
  End Interface

  Public :: timer_dt, timer_gtd, timer_cgt, timer_sc
  Public :: CLOCK_REALTIME
  Public :: CLOCK_MONOTONIC
! These are Linux only
#ifdef __linux__
  Public :: CLOCK_PROCESS_CPUTIME_ID
  Public :: CLOCK_THREAD_CPUTIME_ID
  Public :: CLOCK_TAI
#endif

Contains

  Subroutine timer_dt(time)

! Time using Fortran intrinsic DATE_AND_TIME

    Real(WP), Intent(OUT) :: time

    Integer(INT64) :: values(8)

    Call DATE_AND_TIME(VALUES=values)

    time = Real(values(3),WP)*86400.0_WP +                                  &
           Real(values(5),WP)*3600.0_WP  +                                  &
           Real(values(6),WP)*60.0_WP    +                                  &
           Real(values(7),WP)            +                                  &
           Real(values(8),WP)*1.0E-3_WP

  End Subroutine timer_dt

  Subroutine timer_gtd(time)

! Wrap call to C gettimeofday and return time since EPOCH as a REAL64

    Real(WP), Intent(OUT) :: time

    Type(timeval_c) :: timeval 
    Integer(C_INT)  :: err

    err  = gettimeofday_c(timeval, C_NULL_PTR)
    time = Real(timeval%tv_sec, WP) + Real(timeval%tv_usec, WP)*1.0E-6_WP

  End Subroutine timer_gtd

  Subroutine timer_cgt(time, clockid)

! Wrap call to C clock_gettime and return time in secs and nsecs
! depending on clockid. Realative clock CLOCK_MONOTONIC is default 

    Real(WP),                 Intent(OUT) :: time
    Integer(C_INT), Optional, Intent(IN)  :: clockid
    Type(timespec_c) :: td

    Integer(C_INT) :: cid, err

    cid = CLOCK_MONOTONIC

    If (PRESENT(clockid)) cid = clockid
 
    err = clock_gettime_c(cid, td)

    time = Real(td%tv_sec, WP) + Real(td%tv_nsec, WP)*1.0E-9_WP

  End Subroutine timer_cgt

  Subroutine timer_sc(time)

! Time using Fortran intrinsic SYSTEM_CLOCK 

    Real(WP), Intent(OUT) :: time

    Integer(INT64)  :: COUNTS
    Real(WP)        :: COUNT_RATE
    Integer(INT64)  :: COUNT_MAX

    Call SYSTEM_CLOCK(COUNTS, COUNT_RATE, COUNT_MAX)
    time = Real(COUNTS,WP)/COUNT_RATE

  End Subroutine timer_sc
      
End Module timers
