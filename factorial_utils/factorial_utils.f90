
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

Module factorial_utils 

! Define functions to compute factorials
 
USE ISO_FORTRAN_ENV, ONLY: DP=>REAL64, QP=>REAL128
 
! Define private array of factorial values up to 34 for Factorial function

Real(DP),  PRIVATE, SAVE :: factorialsTo34(0:34) =  REAL( [    &
                                     1.0                    ,   &
                                     1.00000000000000        ,    &
                                     2.00000000000000        ,    &
                                     6.00000000000000        ,    &
                                     24.0000000000000        ,    &
                                     120.000000000000        ,    &
                                     720.000000000000        ,    &
                                     5040.00000000000        ,    &
                                     40320.0000000000        ,    &
                                     362880.000000000        ,    &
                                     3628800.00000000        ,    &
                                     39916800.0000000        ,    &
                                     479001600.000000        ,    &
                                     6227020800.00000        ,    &
                                     87178291200.0000        ,    &
                                     1307674368000.00        ,    &
                                     20922789888000.0        ,    &
                                     355687428096000.        ,    &
                                     6.402373705728000E+015  ,    &
                                     1.216451004088320E+017  ,    &
                                     2.432902008176640E+018  ,    &
                                     5.109094217170944E+019  ,    &
                                     1.124000727777608E+021  ,    &
                                     2.585201673888498E+022  ,    &
                                     6.204484017332394E+023  ,    &
                                     1.551121004333099E+025  ,    &
                                     4.032914611266057E+026  ,    &
                                     1.088886945041835E+028  ,    &
                                     3.048883446117138E+029  ,    &
                                     8.841761993739701E+030  ,    &
                                     2.652528598121910E+032  ,    &
                                     8.222838654177922E+033  ,    &
                                     2.631308369336935E+035  ,    &
                                     8.683317618811886E+036  ,    &
                                     2.952327990396041E+038 ], DP) 

  PRIVATE :: DP, QP

  Interface factorial
    Module Procedure factorialR64
!    Module Procedure factorialR128
  End Interface

Contains

 Pure Recursive Function FactorialR64(n) RESULT(f)

! Recursive function to compute n!. For values of n <= 20 
! a table lookup is used instead of recursion

  Implicit NONE

! Argument variables

  Integer, Intent(IN) :: n
  Real(DP)            :: f

   If (n<35) Then
     f = factorialsTo34(n)
   Else
     f = REAL(n,DP)*FactorialR64(n-1)
   EndIf
 
  End Function FactorialR64

!Pure Recursive Function FactorialR128(n) RESULT(f)

!!Recursive function to compute n!. For values of n <= 20 
!!a table lookup is used instead of recursion

! Implicit NONE

!!Argument variables

! Integer, Intent(IN) :: n
! Real(QP)            :: f

!  If (n<35) Then
!    f = REAL(factorialsTo34(n),QP)
!  Else
!    f = REAL(n,QP)*FactorialR128(n-1)
!  EndIf
!
! End Function FactorialR128

End Module factorial_utils 
