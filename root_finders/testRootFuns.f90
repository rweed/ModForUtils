
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

Module testRootFuns

! Test functions for root finder utilities. These functions are defined in
! Chandrupatla's article in Advances in Engineering Software, Vol 28, 1997.

! Written by: Richard Weed, Ph.D.
!             rweedmsu@gmail.com

! Version 1 : Jan. 2020

  USE ISO_FORTRAN_ENV, WP=>REAL64
  USE rootFinderUtils, ONLY: univariate_function

  Implicit NONE

! define function classes for each test function

  Type, Extends(Univariate_function) :: func1

  Contains

    Procedure :: eval=>evalfunc1

  End Type

  Type, Extends(Univariate_function) :: func2

  Contains

    Procedure :: eval=>evalfunc2

  End Type

  Type, Extends(univariate_function) :: func3

  Contains

    Procedure :: eval=>evalfunc3

  End Type

  Type, Extends(univariate_function) :: func4

  Contains

    Procedure :: eval=>evalfunc4

  End Type

  Type, Extends(univariate_function) :: func5

  Contains

    Procedure :: eval=>evalfunc5

  End Type

  Type, Extends(univariate_function) :: func6

  Contains

    Procedure :: eval=>evalfunc6

  End Type

  Type, Extends(univariate_function) :: func7

  Contains

    Procedure :: eval=>evalfunc7

  End Type

  Type, Extends(univariate_function) :: func8

  Contains

    Procedure :: eval=>evalfunc8

  End Type
  Type, Extends(univariate_function) :: func9

  Contains

    Procedure :: eval=>evalfunc9

  End Type

Contains

! test function definitions

  Pure Function testfun1(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    fval = x**3 -2.0_WP*x - 5.0_WP

  End Function testfun1

  Pure Function testfun2(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    fval = 1.0_WP - 1.0_WP/(x**2) 

  End Function testfun2

  Pure Function testfun3(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    fval = (x-3.0_WP)**3 

  End Function testfun3

  Pure Function testfun4(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    fval = 6.0_WP*(x-2.0_WP)**5 

  End Function testfun4

  Pure Function testfun5(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    fval = x**9
 
  End Function testfun5

  Pure Function testfun6(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    fval = x**19
 
  End Function testfun6

  Pure Function testfun7(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    If (ABS(x) < 3.8E-4_WP) Then
      fval = 0.0_WP
    Else
      fval = x*EXP(-x**(-2))
    EndIf 
 
  End Function testfun7

  Pure Function testfun8(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    Real(WP) :: xi, t1, emx

    xi = 0.61489_WP
    t1 = 1.0_WP-xi
    emx = EXP(-x)
    fval = -(3062.0_WP*t1*emx)/(xi + t1*emx) - 1013.0_WP + 1628.0_WP/x 
 
  End Function testfun8

  Pure Function testfun9(x) Result(fval)

    Implicit NONE
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    Real(WP) :: ex

    ex = EXP(x)
    fval = ex - 2.0_WP - 0.01_WP/(x*x) + 2.E-6_WP/(x*x*x) 
 
  End Function testfun9

! test class eval functions
 
 Impure Function evalfunc1(this, x) Result(fval)

   Implicit NONE

    Class(func1), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun1(x) 

  End Function evalfunc1

  Impure Function evalfunc2(this, x) Result(fval)

    Implicit NONE

    Class(func2), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun2(x) 

  End Function evalfunc2

  Impure Function evalfunc3(this, x) Result(fval)

    Implicit NONE

    Class(func3), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun3(x) 

  End Function evalfunc3

  Impure Function evalfunc4(this, x) Result(fval)

    Implicit NONE

    Class(func4), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun4(x) 

  End Function evalfunc4

  Impure Function evalfunc5(this, x) Result(fval)

    Implicit NONE

    Class(func5), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun5(x) 

  End Function evalfunc5
  Impure Function evalfunc6(this, x) Result(fval)

    Implicit NONE

    Class(func6), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun6(x) 

  End Function evalfunc6

  Impure Function evalfunc7(this, x) Result(fval)

    Implicit NONE

    Class(func7), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun7(x) 

  End Function evalfunc7

  Impure Function evalfunc8(this, x) Result(fval)

    Implicit NONE

    Class(func8), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun8(x) 

  End Function evalfunc8

  Impure Function evalfunc9(this, x) Result(fval)

    Implicit NONE

    Class(func9), Intent(INOUT) :: this
    Real(WP),     Intent(IN)    :: x
    Real(WP)                    :: fval

    this%nevals = this%nevals+1
    fval = testfun9(x) 

  End Function evalfunc9


End Module testRootFuns
