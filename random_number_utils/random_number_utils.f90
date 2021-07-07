
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

Module random_number_utils 

! Define user procedures here. Currently contains factorial, binomial
! coefficients, gamma functions for non F2008 compilers, an l2norm
! wrapper to replace norm2, andfunctions for determining if
! two real values are numerically almost equal, within a tolerance
! or almost zero. 

  USE ISO_FORTRAN_ENV, ONLY: DP=>REAL64, INT32
 
  Interface randNum
    Module Procedure randNumBnds
    Module Procedure randNumBndsVec
    Module Procedure randNumMat
  End Interface

  Interface randN
    Module Procedure randNumN
    Module Procedure randNumNVec
    Module Procedure randNumNMat
  End Interface

  PRIVATE :: DP

Contains


  Subroutine setRandomSeed(seedVal)

    Implicit NONE

    Integer, Intent(IN), OPTIONAL :: seedVal(:)

    Integer              :: n, ns
    Integer              :: timeVals(8)
    Integer, ALLOCATABLE :: seed(:) 

    Call RANDOM_SEED(SIZE=n)

    If (PRESENT(seedVal)) Then ! user supplied seed

      ns = SIZE(seedVal)
      If (ns < n) Then
        ALLOCATE(seed(n), SOURCE=0_INT32)
        seed(1:ns) = seedVal(1:ns)
      Else
        ALLOCATE(seed(ns), SOURCE=seedVal)
      EndIf

    Else ! use current date/time to generate a seed

      Call DATE_AND_TIME(VALUES=timeVals)
      ns = 8
      If (ns < n) Then
        ALLOCATE(seed(n), SOURCE=0_INT32)
        seed(1:ns) = timeVals(ns:1:-1)
      Else
        ALLOCATE(seed(ns), SOURCE=0_INT32)
        seed=timeVals(ns:1:-1)
      EndIf
 
    EndIf
     
    Call RANDOM_SEED(PUT=seed)
 
  End Subroutine setRandomSeed

  Function randNumBnds(ab) Result(num)

! Generates a uniformly distributed random number
! between [a,b] if ab argument is present. [0,1] otherwise

    Implicit NONE

    Real(DP), Intent(IN), OPTIONAL :: ab(2)
    Real(DP)                       :: num
   
    Real(DP) :: num01

    Call RANDOM_NUMBER(num01)

    If (PRESENT(ab)) Then
      num = ab(1) + num01*(ab(2)-ab(1))
      If (num < ab(1)) num=ab(1)
      If (num > ab(2)) num=ab(2)
    Else
      num = num01
    EndIf

  End Function randNumBnds

  Function randNumBndsVec(n, ab) Result(num)

! Generates a vector of uniformly distributed random numbers
! between [a,b] if ab argument is present. [0,1] otherwise.
! ab vector can be all the same bounds if initialized that way or
! each bound can be different 

    Implicit NONE

    Integer,  Intent(IN)           :: n 
    Real(DP), Intent(IN), OPTIONAL :: ab(n,2)
    Real(DP)                       :: num(n)

    Integer  :: i
    Real(DP) :: num01(n)
 
    Call RANDOM_NUMBER(num01)

    If (PRESENT(ab)) Then
      Do i=1,n
        num(i) = ab(i,1) + num01(i)*(ab(i,2)-ab(i,1))
        If(num(i) < ab(i,1)) num(i) = ab(i,1)
        If(num(i) > ab(i,2)) num(i) = ab(i,2)
      EndDo
    Else
      num = num01
    EndIf

  End Function randNumBndsVec

  Function randNumMat(m,n) Result(num)

! Generates a uniformly distributed random number
! between [a,b]

    Implicit NONE

    Integer, Intent(IN) :: m, n 
    Real(DP)            :: num(m,n)
   
    Call RANDOM_NUMBER(num)

  End Function randNumMat

  Function randNumN() Result(num)

! Generates gaussian normal distribution of random numbers
! Adapted from Algorithm 721 ACM

    Implicit NONE

    Real(DP) :: s, t, a, b, r1, r2, u, v, x, y, q
    Real(DP) :: num

    s  =  0.449871_DP
    t  = -0.386595_DP
    a  =  0.19600_DP
    b  =  0.25472_DP
    r1 =  0.27597_DP
    r2 =  0.27846_DP

!  Generate P = (u,v) uniform in rectangle enclosing acceptance region

    Loop1: Do
      Call RANDOM_NUMBER(u)
      Call RANDOM_NUMBER(v)
      v = 1.7156_DP * (v - 0.5_DP)
      x = u - s
      y = ABS(v) - t
      q = x**2 + y*(a*y - b*x)

      If (q < r1) EXIT Loop1
      If (q > r2) CYCLE Loop1
      If (v**2 < -4.0_DP*LOG(u)*u**2) EXIT Loop1

    End Do Loop1

    num = v/u

  End Function randNumN

  Function randNumNVec(n) Result(num)

    Implicit NONE

    Integer, Intent(IN) :: n
    Real(DP)            :: num(n)

    Integer :: i

    Do i= 1,n
      num(i) = randNumN()
    EndDo

  End Function randNumNVec  

  Function randNumNMat(m,n) Result(num)

    Implicit NONE

    Integer, Intent(IN) :: m,n
    Real(DP)            :: num(m,n)

    Integer :: j

    Do j= 1,n
      num(:,j) = randNumNVec(m)
    EndDo

  End Function randNumNMat  

  Function rand_binstring(length) Result(binstring)

! Generate a string of random 0s and 1s

    Implicit NONE

    Integer, Intent(IN)   :: length
    Character(LEN=length) :: binString

    Integer  :: i
    Real(DP) :: rnum(length)
    
    binstring = REPEAT(" ",length) 
    rnum = randNumBndsVec(length)
    Do i=1,length
      If (rnum(i) < 0.5_DP) Then
        binstring(i:i) = "0"
      Else
        binstring(i:i) = "1"
      EndIf
    EndDo

  End Function rand_binstring
      
  Function randperm(n) Result(rnum)

! Generate random permutation of integers from 1 to n
! Mimics MATLAB ranperm(n)

    Implicit NONE

    Integer, Intent(IN) :: n
    Integer             :: rnum(n)

    Integer  :: i, j, k
    Integer  :: temp
    Real(DP) :: u


    Do i=1,n
      rnum(i) = i
    EndDo

    Do j=n,2,-1

      Call RANDOM_NUMBER(u)
      k = FLOOR(REAL(j,DP)*u) + 1
      temp = rnum(k)
      rnum(k) = rnum(j)
      rnum(j) = temp
    EndDo

  End Function randperm      

  Subroutine permuteX(k,X)

! Greens algorithm to randomly permute k of the 
! objects in X 

    Implicit NONE

    Integer,  Intent(IN)    :: k
    Real(DP), Intent(INOUT) :: X(:)

    Integer  :: i, j, n
    Real(DP) :: t
 
    n = SIZE(X,1)

    Do i=1,k
      j = INT(REAL(n-i+1,DP)*randnum()) + i
      t = x(i)
      x(i) = x(j)
      x(j) = t
    EndDo 

  End Subroutine permuteX

  Subroutine randsample(k,X,A)

! Greens algorithm to randomly sample the contents of X 
! and return k objects from X 

    Implicit NONE

    Integer,  Intent(IN)    :: k
    Real(DP), Intent(IN)    :: X(:)
    Real(DP), Intent(INOUT) :: a(k)

    Integer :: i, j, n, m
 
    n = SIZE(X,1)
    m = 0
    Do i=1,n
      j = INT(REAL(n-i+1,DP)*randnum()) + 1
      If (j <= (k-m)) Then
        m    = m+1
        a(m) = x(i)
        If (m >= k) EXIT
      EndIf 
    EndDo
 
  End Subroutine randsample 

End Module random_number_utils 
