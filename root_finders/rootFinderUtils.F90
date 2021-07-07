   
!  Copyright (C) 2021 Richard Weed.
!  All rights reserved.
  
!  Redistribution and use in source and binary forms, with or without 
!  modification, are permitted provided that the following conditions are met:
  
!  1. Redistributions of source code, in whole or in part, must retain the  
!  above copyright notice, this list of conditions and the following 
!  disclaimer.
  
!  2. Redistributions in binary form, in whole or in part, must reproduce the 
!  above copyright notice, this list of conditions and the following disclaimer 
!  in the documentation and/or other materials provided with the distribution.
  
!  3. The names of the contributors may not be used to endorse or promote from 
!  products derived from this software without specific prior written 
!  permission.

!  4. Redistributions of this software, in whole or in part, in any form, 
!  must be freely available and licensed under this original License. The 
!  U.S. Government may add additional restrictions to their modified and 
!  redistributed software as required by Law. However, these restrictions 
!  do not apply to the original software distribution.
 
!  5. Redistribution of this source code, including any modifications, may 
!  not be intentionally obfuscated.
  
!  6. Other code may make use of this software, in whole or in part, without 
!  restriction, provided that it does not apply any restriction to this 
!  software other than outlined above.
  
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
!  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
!  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND
!  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
!  EXEMPLARARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
!  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
!  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
!  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
!  OTHERWISE), ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
!  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Module rootFinderUtils

! Implementations of Brent's and T.R. Chandrupatla' root finding algorithms
! for univariate functions

! Written by: Richard Weed, Ph.D.
!             rweedmsu@gmail.com

! Version 1: Jan. 2020

  USE ISO_FORTRAN_ENV, WP=>REAL64

  Implicit NONE

  PRIVATE

  Type, ABSTRACT :: univariate_function

    Integer :: nevals = 0

  Contains

    Procedure (fceval), Deferred :: eval

  End Type

  ABSTRACT Interface
    Impure Function fceval(this, x) Result(fval)

      IMPORT :: WP, univariate_function
      Implicit NONE
      Class(univariate_function), Intent(INOUT) :: this
      Real(WP), Intent(IN)                      :: x
      Real(WP)                                  :: fval
    End Function fceval

  End Interface

  ABSTRACT Interface
    Impure Function feval(x) Result(fval)

      IMPORT :: WP
      Implicit NONE
      Real(WP), Intent(IN)                      :: x
      Real(WP)                                  :: fval
    End Function feval
  
  End Interface
 
  Interface brentZero
    Module Procedure brentZeroFunc
    Module Procedure brentZeroFun
  End Interface

  Interface trcZero
    Module Procedure trcZeroFunc
    Module Procedure trcZeroFun
  End Interface

  PUBLIC :: brentZero, trcZero, univariate_function, feval

CONTAINS

  Impure Subroutine brentZerofunc(f, ax, bx, xzero, fzero, ierr, epsr,       &
                                  epsa, max_iters)

!-----------------------------------------------------------------------
 
! This subroutine solves for a zero of the function f(x) in the
! interval [ax,bx] using Richard Brents algorithm. The original spaghetti
! code has been refactored into a pasta free form that contains no GO TO's
! or other programming crimes. 
 
!  input..
 
!  ax  - left endpoint of initial interval
!  bx  - right endpoint of initial interval
!  f   - polymorphic class extended from univariate_class type that contains 
!        an eval function. (See abstract class above). We require this to
!        allow extra information needed to evaluate the function to be
!        defined in the extended class 
!  epsr   - (optional) relative error (default is machine EPSILON if not 
!                      present)
!  epsa   - (optional) absolute error (default is 2*EPSILON if non present) 
!  max_iters - (optional) user defined maximum number of iterations. Default
!               is 200

 
!  output..
 
!  xzero -  abcissa approximating a zero of f in the interval ax,bx
!  fzero - function value at fzero 
!  ierr  - error code to signal max iterations exceeded
 
!  It is assumed that f(ax) and f(bx) have opposite signs without
!  a check. zeroin returns a zero x in the given interval [ax,bx]
!  to within a tolerance  2*EPSILON*ABS(x) + tol/2, where EPSILON 
!  is the relative machine precision (ie the smallest number that
!  can be added to a number of the same precision without changing
!  that number's value).

!  This function subprogram is a translation to modern Fortran of
!  Ralph Carmichael's Fortran 90 routine on his PDAS site which is 
!  based on code from the book by Forsythe, Malcolm, and Moler. That 
!  code is based on the algol 60 procedure zero given in Richard Brent,
!  Algorithms for Minimization Without Derivatives, Prentice - Hall, inc.
!  (1973).
 
! Refactored by: Richard Weed, Ph.D.
!                rweedmsu@gmail.com
!-----------------------------------------------------------------------

! WP is a KIND parameter set using either SELECTED_REAL_KIND or one of
! the intrinsic parameters define in the ISO_FORTRAN_ENV module (REAL32, 
! REAL64, or (if supported) REAL128)

    Class(univariate_function),           Intent(INOUT) :: f
    Real(WP),                             Intent(IN)    :: ax, bx 
    Real(WP),                             Intent(INOUT) :: xzero, fzero 
    Integer,                              Intent(OUT)   :: ierr 
    Real(WP),                   Optional, Intent(IN)    :: epsr, epsa
    Integer,                    Optional, Intent(IN)    :: max_iters 

    Integer  :: iters, maxits
    Real(WP) :: a, b, c, d, e, eps, fa, fb, fc, tol1, xm, p, q, r, s, tol

! initialization

    maxits = 500 
    eps    = EPSILON(1.0_WP)
    tol    = 2.0_WP*eps
    If (PRESENT(epsr))      eps    = epsr
    If (PRESENT(epsa))      tol    = epsa
    If (PRESENT(max_iters)) maxits = max_iters
    iters = 0
    ierr  = 0
    a       = ax
    b       = bx
    fa      = f%eval(a)
    fb      = f%eval(b)
    c       = a
    fc      = fa
    d       = b - a
    e       = d

! begin step

    LOOP30: Do
      iters = iters + 1
      If (iters > maxits) Then
        ierr = 1
        EXIT LOOP30
      EndIf
      If ( ABS(fc) <  ABS(fb)) Then 
        a  = b
        b  = c
        c  = a
        fa = fb
        fb = fc
        fc = fa
      EndIf
 
! convergence test
 
      tol1 = 2.0_WP*eps*ABS(b) + 0.5_WP*tol
      xm   = 0.5_WP*(c - b)

      If (ABS(xm) <= tol1) EXIT LOOP30 
      If (fb == 0.0_WP)    EXIT LOOP30 
 
! is bisection necessary?
 
      If (ABS(e) >= tol1 .AND. ABS(fa) > ABS(fb)) Then  
 
! is quadratic interpolation possible?
 
        If (a /= c) Then 
 
! inverse quadratic interpolation
 
          q = fa/fc
          r = fb/fc
          s = fb/fa
          p = s*(2.0_WP*xm*q*(q - r) - (b - a)*(r - 1.0_WP))
          q = (q - 1.0_WP)*(r - 1.0_WP)*(s - 1.0_WP)

        Else
 
! linear interpolation
 
          s = fb/fa
          p = 2.0_WP*xm*s
          q = 1.0_WP - s

        EndIf
 
! adjust signs
 
        If (p > 0.0_WP) q = -q
        p = ABS(p)
 
! is interpolation acceptable?
 
        If ((2.0_WP*p)<(3.0_WP*xm*q - ABS(tol1*q)) .AND. (p<ABS(0.5_WP*e*q))) Then  
          e = d
          d = p/q
        Else
          d = xm
          e = d
        EndIf
 
! bisection

      Else 
        d = xm
        e = d
      EndIf
 
! complete step
 
      a  = b
      fa = fb
      If (ABS(d) > tol1)  b = b + d
      If (ABS(d) <= tol1) b = b + SIGN(tol1, xm)
      fb = f%eval(b)
      If ((fb*(fc/ABS(fc))) > 0.0_WP) Then 
        c  = a
        fc = fa
        d  = b - a
        e  = d
      EndIf

    EndDo LOOP30 
 
! done
 
    xzero = b
    fzero = fb

  End Subroutine brentZerofunc

  Impure Subroutine trcZerofunc(f, xa, xb, xzero, fzero, ierr, epsr, epsa,    &
                                max_iters)

! T.R. Chandrupatla's algorithm for finding the root (zero) of a univariate 
! non linear function without derivatives. It uses a mix of inverse 
! quadratic interpolation and bisection similar to Brent's version of
! Dekkers ZEROIN code but with new constraints that improve its performance
! for functions with repeated roots over Brents code. It is also simpler 
! to implement than Brents code. The improvement in performance is due to
! the elimination of many unneeded bisection evaluations. 

! This routine is a Fortran implementation of the BASIC code and theory given
! in Chandrupatla's journal article: 
  
! T.R. Chandrupatla, " A new hybrid quadratic/bisection algorithm for
! finding the zero of a nonlinear function without derivatives," Advances in
! Engineering Software, Vol 28, 1997, pp. 145-149. 

! Input:

!  f      - a function routine with interface consistent with feval. Must
!           be in a module or have an explicit interface visible to calling
!           routine 
!  xa, xb - upper and lower bounds of the interval bounding the root
!  epsr   - (optional) relative error (default is machine EPSILON if not 
!                      present)
!  epsa   - (optional) absolute error (default is 2*EPSILON if non present) 
!  max_iters - (optional) user defined maximum number of iterations. Default
!               is 500

! Output:

!  xzero  - x location of zero root
!  fzero  - function value at fzero 
!  ierr   - error code to signal max iterations exceeded
     
! Written by: Richard Weed, Ph.D.
!             rweedmsu@gmail.com

! Version 1: Jan. 2020

    Implicit NONE

    Class(univariate_function),           Intent(INOUT) :: f
    Real(WP),                             Intent(IN)    :: xa, xb 
    Real(WP),                             Intent(OUT)   :: xzero
    Real(WP),                             Intent(OUT)   :: fzero
    Integer,                              Intent(OUT)   :: ierr 
    Real(WP),                   Optional, Intent(IN)    :: epsr, epsa
    Integer,                    Optional, Intent(IN)    :: max_iters
 
!   Local variables

    Real(WP) :: xt, x1, x2, x3, t, fx1, fx2, fx3, fxt, a, b, c, d, al, phi,   &
                fl, fh, tl, tol, xi, eps, delt, sfx1, sfx2, sft
    Integer  :: maxits, iters

! set default values for maxits, eps, and delt. Override if optional arguments
! are present

    ierr   = 0
    maxits = 500
    eps    = EPSILON(1.0_WP)
    delt   = 2.0_WP*eps
    If (PRESENT(epsr))      eps    = epsr
    If (PRESENT(epsa))      delt   = epsa
    If (PRESENT(max_iters)) maxits = max_iters 
    x1 = xa
    x2 = xb
    x3 = xb
  
    fx1 = f%eval(x1)
    fx2 = f%eval(x2)
    fx3 = fx2

! check sign of fxa and fxb to make sure we are bracketing a zero root
! error stop if we don't see a change in sign for the function values
! at fx1 and fx2

    sfx1 = SIGN(1.0_WP, fx1)
    sfx2 = SIGN(1.0_WP, fx2)
    If (sfx1*sfx2 > 0.0_WP) Then
      ERROR STOP "ERROR in subroutine chandrupatla : interval (xa,xb) does not contain a zero root"
    EndIf
   
    t     = 0.5_WP
    iters = 0 

! Iterate until we find a root or maxits exceeded. Logic will select between
! inverse quadratic interpolation and bisection

    ITERATION_LOOP: Do

! Use linear interpolation to get estimate of x at t in current interval
 
      xt   = x1 + t*(x2-x1)
      fxt  = f%eval(xt)
      sft  = SIGN(1.0_WP, fxt)
      sfx1 = SIGN(1.0_WP, fx1)
 
! update intervals based on sign of new estimate of xt and current left
! and right interval boundary.
 
      If (sft == sfx1) Then 
        x3  = x1
        fx3 = fx1
      Else
        x3  = x2
        x2  = x1
        fx3 = fx2
        fx2 = fx1
      EndIf 
      x1  = xt
      fx1 = fxt

! set xzero and fzero to values on one of the current interval boundaries

      If (ABS(fx2) < ABS(fx1)) Then
        xzero = x2
        fzero = fx2
      Else
        xzero = x1
        fzero = fx1
      EndIf

! check to see if xzero is on boundary of current interval and exit
! otherwise check to see if xzero is within tolerance for a zero

      If (ABS(fzero) == 0.0_WP) EXIT ITERATION_LOOP

      tol = 2.0_WP*eps*ABS(xzero) + 0.5_WP*delt
      tl  = tol/ABS(x2-x1)
      If (tl > 0.5_WP) EXIT ITERATION_LOOP

! test for valid inverse quadratic interpolation. Parameters in BASIC version
! that test for bounds on phi parameter are replaced by the alternate values
! for bounds on xi parameter given in Chandrupatla's paper in section 3 to 
! remove dependence on square roots. If inverse interpolation fails then the
! iteration switches to using bisection. Original tests with square roots are
! shown below as comments.
       
      xi  = (x1-x2)/(x3-x2)
      phi = (fx1-fx2)/(fx3-fx2)

!      fl = 1.0_WP - SQRT(1.0_WP-xi)
!      fh = SQRT(xi)
!      If (fl < phi .AND. phi < fh) Then

      fl  = (1.0_WP-phi)**2 
      fh  = phi**2
      If (fh<xi .AND. (fl<(1.0_WP-xi))) Then
        al = (x3-x1)/(x2-x1)
        a  = fx1/(fx2-fx1)
        b  = fx3/(fx2-fx3)
        c  = fx1/(fx3-fx1)
        d  = fx2/(fx3-fx2)
        t  = a*b + c*d*al
      Else
        t  = 0.5_WP
      EndIf

! Adjust t away from interval boundary. Set t to either tl or 1-tl

      If (t < tl ) Then
        t = tl
      ElseIf(t > (1.0_WP-tl)) Then
        t = 1.0_WP-tl
      EndIf

! check to see if max iterations exceeded.

      iters = iters+1
      If (iters > maxits) Then
        ierr = 1
        EXIT ITERATION_LOOP 
      EndIf

    EndDo ITERATION_LOOP

  End Subroutine trcZerofunc

  Impure Subroutine brentZerofun(f, ax, bx, xzero, fzero, ierr, epsr,         &
                                 epsa, max_iters)

!-----------------------------------------------------------------------
 
! This subroutine solves for a zero of the function f(x) in the
! interval [ax,bx] using Richard Brents algorithm. The original spaghetti
! code has been refactored into a pasta free form that contains no GO TO's
! or other programming crimes. 
 
!  input..
 
!  ax  - left endpoint of initial interval
!  bx  - right endpoint of initial interval
!  f   - a function routine with interface consistent with feval. Must
!        be in a module or have an explicit interface visible to calling
!        routine
!  epsr   - (optional) relative error (default is machine EPSILON if not 
!                      present)
!  epsa   - (optional) absolute error (default is 2*EPSILON if non present) 
!  max_iters - (optional) user defined maximum number of iterations. Default
!               is 500

!  output..
 
!  xzero -  abcissa approximating a zero of f in the interval ax,bx
!  fzero -  abcissa approximating a zero of f in the interval ax,bx
!  ierr  - error flag that signals max iterations exceeded
 
!  It is assumed that f(ax) and f(bx) have opposite signs without
!  a check. zeroin returns a zero x in the given interval [ax,bx]
!  to within a tolerance  2*EPSILON*ABS(x) + tol/2, where EPSILON 
!  is the relative machine precision (ie the smallest number that
!  can be added to a number of the same precision without changing
!  that number's value).

!  This function subprogram is a translation to modern Fortran of
!  Ralph Carmichael's Fortran 90 routine on his PDAS site which is 
!  based on code from the book by Forsythe, Malcolm, and Moler. That 
!  code is based on the algol 60 procedure zero given in Richard Brent,
!  Algorithms for Minimization Without Derivatives, Prentice - Hall, inc.
!  (1973).

! Refactored by: Richard Weed, Ph.D.
!                rweedmsu@gmail.com
 
!-----------------------------------------------------------------------

! WP is a KIND parameter set using either SELECTED_REAL_KIND or one of
! the intrinsic parameters define in the ISO_FORTRAN_ENV module (REAL32, 
! REAL64, or (if supported) REAL128)

    Procedure(feval)                         :: f
    Real(WP),                  Intent(IN)    :: ax, bx 
    Real(WP),                  Intent(INOUT) :: xzero, fzero 
    Integer,                   Intent(OUT)   :: ierr 
    Real(WP),        Optional, Intent(IN)    :: epsr, epsa
    Integer,         Optional, Intent(IN)    :: max_iters 

    Integer  :: iters, maxits
    Real(WP) :: a, b, c, d, e, eps, fa, fb, fc, tol1, xm, p, q, r, s, tol

! initialization

    maxits = 500 
    eps    = EPSILON(1.0_WP)
    tol    = 2.0_WP*eps
    If (PRESENT(epsr))      eps    = epsr
    If (PRESENT(epsa))      tol    = epsa
    If (PRESENT(max_iters)) maxits = max_iters
    iters = 0
    ierr  = 0
    a       = ax
    b       = bx
    fa      = f(a)
    fb      = f(b)
    c       = a
    fc      = fa
    d       = b - a
    e       = d

! begin step

    LOOP30: Do
      iters = iters + 1
      If (iters > maxits) Then
        ierr = 1
        EXIT LOOP30
      EndIf
      If ( ABS(fc) <  ABS(fb)) Then 
        a  = b
        b  = c
        c  = a
        fa = fb
        fb = fc
        fc = fa
      EndIf
 
! convergence test
 
      tol1 = 2.0_WP*eps*ABS(b) + 0.5_WP*tol
      xm   = 0.5_WP*(c - b)

      If (ABS(xm) <= tol1) EXIT LOOP30 
      If (fb == 0.0_WP)    EXIT LOOP30 
 
! is bisection necessary?
 
      If (ABS(e) >= tol1 .AND. ABS(fa) > ABS(fb)) Then  
 
! is quadratic interpolation possible?
 
        If (a /= c) Then 
 
! inverse quadratic interpolation
 
          q = fa/fc
          r = fb/fc
          s = fb/fa
          p = s*(2.0_WP*xm*q*(q - r) - (b - a)*(r - 1.0_WP))
          q = (q - 1.0_WP)*(r - 1.0_WP)*(s - 1.0_WP)

        Else
 
! linear interpolation
 
          s = fb/fa
          p = 2.0_WP*xm*s
          q = 1.0_WP - s

        EndIf
 
! adjust signs
 
        If (p > 0.0_WP) q = -q
        p = ABS(p)
 
! is interpolation acceptable?
 
        If ((2.0_WP*p)<(3.0_WP*xm*q - ABS(tol1*q)) .AND. (p<ABS(0.5_WP*e*q))) Then  
          e = d
          d = p/q
        Else
          d = xm
          e = d
        EndIf
 
! bisection

      Else 
        d = xm
        e = d
      EndIf
 
! complete step
 
      a  = b
      fa = fb
      If (ABS(d) > tol1)  b = b + d
      If (ABS(d) <= tol1) b = b + SIGN(tol1, xm)
      fb = f(b)
      If ((fb*(fc/ABS(fc))) > 0.0_WP) Then 
        c  = a
        fc = fa
        d  = b - a
        e  = d
      EndIf

    EndDo LOOP30 
 
! done
 
    xzero = b
    fzero = fb

  End Subroutine brentZerofun

  Impure Subroutine trcZerofun(f, xa, xb, xzero, fzero, ierr, epsr, epsa,   &
                                max_iters)

! T.R. Chandrupatla's algorithm for finding the root (zero) of a univariate 
! non linear function without derivatives. It uses a mix of inverse 
! quadratic interpolation and bisection similar to Brent's version of
! Dekkers ZEROIN code but with new constraints that improve its performance
! for functions with repeated roots over Brents code. It is also simpler 
! to implement than Brents code. The improvement in performance is due to
! the elimination of many unneeded bisection evaluations. 

! This routine is a Fortran implementation of the BASIC code and theory given
! in Chandrupatla's journal article: 
  
! T.R. Chandrupatla, " A new hybrid quadratic/bisection algorithm for
! finding the zero of a nonlinear function without derivatives," Advances in
! Engineering Software, Vol 28, 1997, pp. 145-149. 

! Input:

!  f      - a polymorphic derived class that defines the function to be
!           evaluated . User must provide an class function named eval that  
!           has a single REAL(WP) argument. The class must be an extension
!           of the univariate_function base class. The function reference is
!           then fa = f%eval(a). This approach was chosen instead of just
!           passing the function as a procedure or procedure pointer to allow
!           additional information needed to evaluate the function to be
!           passed to the eval routine without adding additional optional
!           arguments
!  xa, xb - upper and lower bounds of the interval bounding the root
!  epsr   - (optional) relative error (default is machine EPSILON if not 
!                      present)
!  epsa   - (optional) absolute error (default is 2*EPSILON if non present) 
!  max_iters - (optional) user defined maximum number of iterations. Default
!               is 200

! Output:

!  xzero  - x location of zero root
!  ierr   - error code to signal max iterations exceeded
     
! Written by: Richard Weed, Ph.D.
!             rweedmsu@gmail.com

! Version 1: Jan. 2020

    Implicit NONE

    Procedure(feval)                        :: f
    Real(WP),                 Intent(IN)    :: xa, xb 
    Real(WP),                 Intent(OUT)   :: xzero
    Real(WP),                 Intent(OUT)   :: fzero
    Integer,                  Intent(OUT)   :: ierr 
    Real(WP),       Optional, Intent(IN)    :: epsr, epsa
    Integer,        Optional, Intent(IN)    :: max_iters
 
!   Local variables

    Real(WP) :: xt, x1, x2, x3, t, fx1, fx2, fx3, fxt, a, b, c, d, al, phi,   &
                fl, fh, tl, tol, xi, eps, delt, sfx1, sfx2, sft
    Integer  :: maxits, iters

! set default values for maxits, eps, and delt. Override if optional arguments
! are present

    ierr   = 0
    maxits = 500
    eps    = EPSILON(1.0_WP)
    delt   = 2.0_WP*eps
    If (PRESENT(epsr))      eps    = epsr
    If (PRESENT(epsa))      delt   = epsa
    If (PRESENT(max_iters)) maxits = max_iters 
    x1 = xa
    x2 = xb
    x3 = xb
  
    fx1 = f(x1)
    fx2 = f(x2)
    fx3 = fx1

! check sign of fxa and fxb to make sure we are bracketing a zero root
! error stop if we don't see a change in sign for the function values
! at fx1 and fx2

    sfx1 = SIGN(1.0_WP, fx1)
    sfx2 = SIGN(1.0_WP, fx2)
    If (sfx1*sfx2 > 0.0_WP) Then
      ERROR STOP "ERROR in subroutine chandrupatla : interval (xa,xb) does not contain a zero root"
    EndIf
   
    t     = 0.5_WP
    iters = 0 

! Iterate until we find a root or maxits exceeded. Logic will select between
! inverse quadratic interpolation and bisection

    ITERATION_LOOP: Do

! Use linear interpolation to get estimate of x at t in current interval
 
      xt   = x1 + t*(x2-x1)
      fxt  = f(xt)
      sft  = SIGN(1.0_WP, fxt)
      sfx1 = SIGN(1.0_WP, fx1)
 
! update intervals based on sign of new estimate of xt and current left
! and right interval boundary.
 
      If (sft == sfx1) Then 
        x3  = x1
        fx3 = fx1
      Else
        x3  = x2
        x2  = x1
        fx3 = fx2
        fx2 = fx1
      EndIf 
      x1  = xt
      fx1 = fxt

! set xzero and fzero to values on one of the current interval boundaries

      If (ABS(fx2) < ABS(fx1)) Then
        xzero = x2
        fzero = fx2
      Else
        xzero = x1
        fzero = fx1
      EndIf

! check to see if xzero is on boundary of current interval and exit
! otherwise check to see if xzero is within tolerance for a zero

      If (ABS(fzero) == 0.0_WP) EXIT ITERATION_LOOP

      tol = 2.0_WP*eps*ABS(xzero) + 0.5_WP*delt
      tl  = tol/ABS(x2-x1)
      If (tl > 0.5_WP) EXIT ITERATION_LOOP

! test for valid inverse quadratic interpolation. Parameters in BASIC version
! that test for bounds on phi parameter are replaced by the alternate values
! for bounds on xi parameter given in Chandrupatla's paper in section 3 to 
! remove dependence on square roots. If inverse interpolation fails then the
! iteration switches to using bisection. Original tests with square roots
! are shown below as comments
       
      xi  = (x1-x2)/(x3-x2)
      phi = (fx1-fx2)/(fx3-fx2)

!      fl = 1.0_WP - SQRT(1.0_WP-xi)
!      fh = SQRT(xi)
!      If (fl < phi .AND. phi < fh) Then

      fl  = (1.0_WP-phi)**2 
      fh  = phi**2

      If (fh<xi .AND. (fl<(1.0_WP-xi))) Then
        al = (x3-x1)/(x2-x1)
        a  = fx1/(fx2-fx1)
        b  = fx3/(fx2-fx3)
        c  = fx1/(fx3-fx1)
        d  = fx2/(fx3-fx2)
        t  = a*b + c*d*al
      Else
        t  = 0.5_WP
      EndIf

! Adjust t away from interval boundary. Set t to either tl or 1-tl

      If (t < tl ) Then
        t = tl
      ElseIf(t > (1.0_WP-tl)) Then
        t = 1.0_WP-tl
      EndIf

! check to see if max iterations exceeded.

      iters = iters+1
      If (iters > maxits) Then
        ierr = 1
        EXIT ITERATION_LOOP 
      EndIf

    EndDo ITERATION_LOOP

  End Subroutine trcZerofun


End Module rootFinderUtils
