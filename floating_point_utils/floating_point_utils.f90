
!Copyright (C) 2021 Richard Weed.
!All rights reserved.

!Redistribution and use in source and binary forms, with or without 
!modification, are permitted provided that the following conditions are met:

!1. Redistributions of source code, in whole or in part, must retain the  
!above copyright notice, this list of conditions and the following 
!disclaimer.

!2. Redistributions in binary form, in whole or in part, must reproduce the 
!above copyright notice, this list of conditions and the following disclaimer 
!in the documentation and/or other materials provided with the distribution.

!3. The names of the contributors may not be used to endorse or promote from 
!products derived from this software without specific prior written 
!permission.

!4. Redistributions of this software, in whole or in part, in any form, 
!must be freely available and licensed under this original License. The 
!U.S. Government may add additional restrictions to their modified and 
!redistributed software as required by Law. However, these restrictions 
!do not apply to the original software distribution.
 
!5. Redistribution of this source code, including any modifications, may 
!not be intentionally obfuscated.

!6. Other code may make use of this software, in whole or in part, without 
!restriction, provided that it does not apply any restriction to this 
!software other than outlined above.

!THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
!IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
!THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND
!CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
!EXEMPLARARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
!PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
!OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
!WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
!OTHERWISE), ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
!ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Module floating_point_utils 

! Some utilities for checking for floating point exceptions (NaN, and Infinity)
! along with utilities for checking for floating point "equality" to some
! prescribed tolerance.

USE ISO_FORTRAN_ENV, ONLY: DP=>REAL64, SP=>REAL32, QP=>REAL128, INT64
 
  Interface almostEqual
   Module Procedure almostEqualR32
   Module Procedure almostEqualR64
   Module Procedure almostEqualR128
  End Interface

  Interface OPERATOR(.AEQ.)
   Module Procedure almostEqualR32
   Module Procedure almostEqualR64
   Module Procedure almostEqualR128
  End Interface

  Interface almostZero
   Module Procedure almostZeroR32
   Module Procedure almostZeroR64
   Module Procedure almostZeroR128
  End Interface

  Interface AEQ0 
   Module Procedure almostZeroR32
   Module Procedure almostZeroR64
   Module Procedure almostZeroR128
  End Interface

  Interface deltaTol
   Module Procedure deltaTolR32
   Module Procedure deltaTolR64
   Module Procedure deltaTolR128
  End Interface

  Interface isNan
    Module Procedure isNanR32
    Module Procedure isNanR64
    Module Procedure isNanR128
  End Interface

  Interface isInfinite
    Module Procedure isInfiniteR32
    Module Procedure isInfiniteR64
    Module Procedure isInfiniteR128
  End Interface

  Interface isNaNInf 
    Module Procedure isNaNInfR32
    Module Procedure isInfiniteR64
    Module Procedure isNaNInfR128
  End Interface

  PRIVATE :: DP, INT64, SP, QP

Contains


!------------------------------------------------------------------------------

  Elemental Function almostEqualR32(a, b) Result(aeqb)

! Function to test if two SP floating point numbers 
! almost equal to close to machine precision 

! Code taken from post on Intel Fortran Forum 11/15/2016

    Implicit NONE

! Argument variables

    Real(SP), Intent(IN) :: a, b
    Logical              :: aeqb
 
    aeqb = ABS(a-b) < 5.0_SP*SPACING(MAX(ABS(a), ABS(b)))

  End Function almostEqualR32

  Elemental Function almostEqualR64(a, b) Result(aeqb)

! Function to test if two DP floating point numbers 
! almost equal to close to machine precision 

! Code taken from post on Intel Fortran Forum 11/15/2016

    Implicit NONE

! Argument variables

    Real(DP), Intent(IN) :: a, b
    Logical              :: aeqb
 
    aeqb = ABS(a-b) < 5.0_DP*SPACING(MAX(ABS(a), ABS(b)))

  End Function almostEqualR64

  Elemental Function almostEqualR128(a, b) Result(aeqb)

! Function to test if two QP floating point numbers 
! almost equal to close to machine precision 

! Code taken from post on Intel Fortran Forum 11/15/2016

    Implicit NONE

! Argument variables

    Real(QP), Intent(IN) :: a, b
    Logical              :: aeqb
 
    aeqb = ABS(a-b) < 5.0_QP*SPACING(MAX(ABS(a), ABS(b)))

  End Function almostEqualR128

!------------------------------------------------------------------------------

  Elemental Function deltaTolR32(a, b, tol) Result(abtol)

! Check to see if the delta between two SP numbers is less that
! some tolerance

    Real(SP), Intent(IN) :: a, b, tol
    Logical              :: abtol

    abtol = (ABS(a-b) < tol)

  End Function deltaTolR32

  Elemental Function deltaTolR64(a, b, tol) Result(abtol)

! Check to see if the delta between two DP numbers is less that
! some tolerance

    Real(DP), Intent(IN) :: a, b, tol
    Logical              :: abtol

    abtol = (ABS(a-b) < tol)

  End Function deltaTolR64

  Elemental Function deltaTolR128(a, b, tol) Result(abtol)

! Check to see if the delta between two QP numbers is less that
! some tolerance

    Real(QP), Intent(IN) :: a, b, tol
    Logical              :: abtol

    abtol = (ABS(a-b) < tol)

  End Function deltaTolR128

!------------------------------------------------------------------------------

  Elemental Function almostZeroR32(anum) Result(a0)

! Function to test if a SP floating point number is
! almost zero to close to machine precision

! Code modified from post on Intel Fortran Forum 11/15/2016

    Implicit NONE

! Argument variables

    Real(SP), Intent(IN)  :: anum
    Logical               :: a0
 
! Local variables

    a0 = (ABS(anum) < 5.0_SP*SPACING(0.0_SP))

  End Function almostZeroR32

  Elemental Function almostZeroR64(anum) Result(a0)

! Function to test if a DP floating point number is
! almost zero to close to machine precision

! Code modified from post on Intel Fortran Forum 11/15/2016

    Implicit NONE

! Argument variables

    Real(DP), Intent(IN)  :: anum
    Logical               :: a0
 
! Local variables

    a0 = (ABS(anum) < 5.0_DP*SPACING(0.0_DP))

  End Function almostZeroR64

  Elemental Function almostZeroR128(anum) Result(a0)

! Function to test if a QP floating point number is
! almost zero to close to machine precision

! Code modified from post on Intel Fortran Forum 11/15/2016

    Implicit NONE

! Argument variables

    Real(QP), Intent(IN)  :: anum
    Logical               :: a0
 
! Local variables

    a0 = (ABS(anum) < 5.0_QP*SPACING(0.0_QP))

  End Function almostZeroR128

  Elemental Function isNaNR32(num)

    USE ieee_arithmetic, ONLY: ieee_is_nan
    
    Implicit NONE

    Real(SP), Intent(IN) :: num
    Logical              :: isNanR32

    isNANR32 = ieee_is_nan(num)

  End Function isNaNR32

  Elemental Function isInfiniteR32(num) Result(isInfinite)
    
    USE ieee_arithmetic, ONLY: ieee_is_finite

    Implicit NONE

    Real(SP), Intent(IN) :: num
    Logical              :: isInfinite

    isInfinite = (ieee_is_finite(num) .EQV. .FALSE.)

  End Function isInfiniteR32

  Elemental Function isNanInfR32(num) Result(isNanInf)
    
     Implicit NONE

     Real(SP), Intent(IN) :: num

     Logical              :: isNanInf
   
     isNanInf = (isNan(num) .OR. isInfinite(num))
   
  End Function isNaNInfR32

  Elemental Function isNaNR64(num)

    USE ieee_arithmetic, ONLY: ieee_is_nan
    
    Implicit NONE

    Real(DP), Intent(IN) :: num
    Logical              :: isNanR64

    isNANR64 = ieee_is_nan(num)

  End Function isNaNR64

  Elemental Function isInfiniteR64(num) Result(isInfinite)
    
    USE ieee_arithmetic, ONLY: ieee_is_finite

    Implicit NONE

    Real(DP), Intent(IN) :: num
    Logical              :: isInfinite

    isInfinite = (ieee_is_finite(num) .EQV. .FALSE.)

  End Function isInfiniteR64

  Elemental Function isNanInfR64(num) Result(isNanInf)
    
     Implicit NONE

     Real(DP), Intent(IN) :: num

     Logical              :: isNanInf
   
     isNanInf = (isNan(num) .OR. isInfinite(num))
   
  End Function isNaNInfR64

  Elemental Function isNaNR128(num)

    USE ieee_arithmetic, ONLY: ieee_is_nan
    
    Implicit NONE

    Real(QP), Intent(IN) :: num
    Logical              :: isNanR128

    isNANR128 = ieee_is_nan(num)

  End Function isNaNR128

  Elemental Function isInfiniteR128(num) Result(isInfinite)
    
    USE ieee_arithmetic, ONLY: ieee_is_finite
    Implicit NONE

    Real(QP), Intent(IN) :: num
    Logical              :: isInfinite

    isInfinite = (ieee_is_finite(num) .EQV. .FALSE.)

  End Function isInfiniteR128

  Elemental Function isNanInfR128(num) Result(isNanInf)
    
    Implicit NONE

    Real(QP), Intent(IN) :: num

    Logical              :: isNanInf
   
    isNanInf = (isNan(num) .OR. isInfinite(num))
   
  End Function isNaNInfR128

End Module floating_point_utils 
