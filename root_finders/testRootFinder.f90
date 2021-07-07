
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

Program testRootFinder

! Test program for Brent and Chandrupatla's root finding routines

! Written by : Richard Weed, Ph.D.
!              rweedmsu@gmail.com

! Version 1. : Jan. 2020

  USE ISO_FORTRAN_ENV, WP=>REAL64
  USE rootFinderUtils
  Use testRootFuns

  Implicit NONE

  Real(WP) :: xa, xb, epsr, epsa, tol, xzero, fzero
  Integer  :: ierr

  Type(func1) :: fun1
  Type(func2) :: fun2
  Type(func3) :: fun3
  Type(func4) :: fun4
  Type(func5) :: fun5
  Type(func6) :: fun6
  Type(func7) :: fun7
  Type(func8) :: fun8
  Type(func9) :: fun9


! Relative and absolute error bounds

  epsr = 1.E-10_WP  !values from trc's paper
  epsa = 1.E-5_WP 

!  epsr = EPSILON(1._WP) ! gives close to machine precision but more evals
!  epsa = 2._WP*epsr

  Print *,' '
  Print *,' FUN1 - zero at X = 2.0945515'
  Print *,'' 
  Print *,' Interval = [2.0 3.0]'

  xa  = 2.0_WP
  xb  = 3.0_WP

  Call brentZero(fun1, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,''
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun1%nevals
  fun1%nevals = 0 
  
  Call trcZero(fun1, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun1%nevals

  Print *,'' 
  Print *,' Interval = [-1E10 1E10]'

  xa  = -1.0E10_WP
  xb  =  1.0E10_WP 

  Call brentZero(fun1, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,''
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun1%nevals
  fun1%nevals = 0 
  
  Call trcZero(fun1, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun1%nevals

  Print *,' '
  Print *,' FUN2 - zero at X = 1.0'
  Print *,' '
  Print *,' Interval [0.5 1.51]'

  xa = 0.5_WP
  xb = 1.51_WP

  Call brentZero(fun2, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun2%nevals
  fun2%nevals = 0 
 
  Call trcZero(fun2, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun2%nevals

  Print *,' '
  Print *,' Interval [1E-12 1E12]'

  xa =  1.0E-12_WP
  xb =  1.0E12_WP

  Call brentZero(fun2, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun2%nevals
  fun2%nevals = 0 

  Call trcZero(fun2, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun2%nevals

  Print *,' '
  Print *,' FUN3 - zero at X = 3.0'
  Print *,' '
  Print *,' Interval [0.0 5.0]'

  xa = 0.0_WP
  xb = 5.0_WP

  Call brentZero(fun3, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun3%nevals
  fun3%nevals = 0 

  Call trcZero(fun3, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun3%nevals

  Print *,' '
  Print *,' Interval [-1E10 1E10]'

  xa = -1.0E10_WP
  xb =  1.0E10_WP

  Call brentZero(fun3, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun3%nevals
  fun3%nevals = 0 

  Call trcZero(fun3, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun3%nevals

  Print *,' '
  Print *,' FUN4 - zero at X = 2.0'
  Print *,' '
  Print *,' Interval [0.0 5.0]'

  xa = 0.0_WP
  xb = 5.0_WP

  Call brentZero(fun4, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun4%nevals
  fun4%nevals = 0 

  Call trcZero(fun4, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun4%nevals

  Print *,' '
  Print *,' Interval [-1E10 1E10]'

  xa = -1.0E10_WP
  xb =  1.0E10_WP

  Call brentZero(fun4, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun4%nevals
  fun4%nevals = 0 

  Call trcZero(fun4, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun4%nevals

  Print *,' '
  Print *,' FUN5 - zero at X = 0.0'
  Print *,' '
  Print *,' Interval = [-1.0 4.0]'

  xa = -1.0_WP
  xb =  4.0_WP

  Call brentZero(fun5, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun5%nevals
  fun5%nevals = 0
 
  Call trcZero(fun5, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun5%nevals

  Print *,' '
  Print *,' Interval = [-10.0 100.0]'

  xa = -10.0_WP
  xb =  100.0_WP
  Call brentZero(fun5, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun5%nevals
  fun5%nevals = 0
 
  Call trcZero(fun5, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun5%nevals

  Print *,' '
  Print *,' FUN6 - zero at X = 0.0'
  Print *,' '
  Print *,' Interval [-1.0 4.0]'

  xa = -1.0_WP
  xb =  4.0_WP

  Call brentZero(fun6, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun6%nevals
  fun6%nevals = 0 

  Call trcZero(fun6, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun6%nevals

  Print *,' '
  Print *,' Interval [-10.0 100.0]'

  xa = -10.0_WP
  xb =  100.0_WP
  Call brentZero(fun6, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun6%nevals
  fun6%nevals = 0 

  Call trcZero(fun6, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun6%nevals

  Print *,' '
  Print *,' FUN7 - zero at X = 0.0'
  Print *,' '
  Print *,' Interval = [-1.0 4.0]'

  xa = -1.0_WP
  xb =  4.0_WP

  Call brentZero(fun7, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun7%nevals
  fun7%nevals = 0
 
  Call trcZero(fun7, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun7%nevals

  Print *,' '
  Print *,' Interval = [-10.0 100.0]'

  xa = -10.0_WP
  xb =  100.0_WP

  Call brentZero(fun7, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun7%nevals
  fun7%nevals = 0
 
  Call trcZero(fun7, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun7%nevals

  Print *,' '
  Print *,' FUN8 - zero at X = 1.037536'
  Print *,' '
  Print *,' Interval = [2.0E-4 2.0]'

  xa = 2.0E-4_WP
  xb = 2.0_WP

  Call brentZero(fun8, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun8%nevals
  fun8%nevals = 0 

  Call trcZero(fun8, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun8%nevals

  Print *,' '
  Print *,' Interval = [2.0E-4 81.0]'

  xa = 2.0E-4_WP
  xb = 81.0_WP

  Call brentZero(fun8, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun8%nevals
  fun8%nevals = 0 

  Call trcZero(fun8, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun8%nevals
  Print *,' '
  Print *,' FUN9  - zero at X= 0.7032'
  Print *,' '
  Print *,' Interval - [2.0E-4 1.0]'

  xa = 2.0E-4_WP
  xb = 1.0_WP

  Call brentZero(fun9, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun9%nevals
  fun9%nevals = 0 

  Call trcZero(fun9, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun9%nevals

  Print *,''
  Print *,' Interval - [2.0E-4 81]'
  xa = 2.0E-4_WP
  xb = 81.0_WP

  Call brentZero(fun9, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' Brent xzero = ', xzero 
  Print *,' Brent fzero = ', fzero
  Print *,' Brent nevals = ', fun9%nevals
  fun9%nevals = 0 

  Call trcZero(fun9, xa, xb, xzero, fzero, ierr, epsr, epsa)
  Print *,' '
  Print *,' trc xzero = ', xzero 
  Print *,' trc fzero = ', fzero
  Print *,' trc nevals = ', fun9%nevals

End Program testRootFinder
