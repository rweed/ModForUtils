!  
!  Copyright (C) 2021, Mississippi State University.
!  All rights reserved.
!  
!  Redistribution and use in source and binary forms, with or without 
!  modification, are permitted provided that the following conditions are met:
!  
!  1. Redistributions of source code must retain the above copyright notice, 
!  this list of conditions and the following disclaimer.
!  
!  2. Redistributions in binary form must reproduce the above copyright notice, 
!  this list of conditions and the following disclaimer in the documentation 
!  and/or other materials provided with the distribution.
!  
!  3. Neither the name of the Mississippi State University nor the names of 
!  its contributors may be used to endorse or promote products derived from 
!  this software without specific prior written permission.
!  
!  4. Redistributions of this software, in whole or in part, in any form, 
!  must be freely available and licensed under this original License. The 
!  Government may add additional restrictions to their modified and 
!  redistributed software as required by Law. However, these restrictions 
!  do not apply to the original software distribution.
!  
!  5. Redistributions of this software, in whole or in part, must be freely 
!  available along with the corresponding source code.
!  
!  6. Redistribution of this source code, including any modifications, may 
!  not be intentionally obfuscated.
!  
!  7. Other code may make use of this software, in whole or in part, without 
!  restriction, provided that it does not apply any restriction to this 
!  software other than outlined above.
!  
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
!  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
!  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MISSISSIPPI STATE UNIVERSITY BE
!  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!  POSSIBILITY OF SUCH DAMAGE.
!  
Module quickSort

! Implements quicksort routines for real, integer, and character arrays
! REAL32, REAL64, INT32, INT64, and Character(LEN=1) arrays are
! supported. The code is taken from the quicksort implementation given
! in Hanson and Hopkins, "Numerical Computation with Modern Fortran",
! SIAM, 2013.

! Arrays default to sorting into ascending order. This can be
! changed by setting the optional argument order to order=DECENDING
! for any of the overloaded quicksort interfaces. Two forms of the
! quicksort routines are provided, one sorts the arrays in place
! and changes the values in the array, the second doesn't change
! the values but returns a permutation (index) array that can
! be used to reorder the input array outside of qsort

! Note access to the different versions are through the overloaded
! qsort interface

! Examples:

!  Real(REAL32) :: a(50)
!  Integer      :: index(50)

!  Call qsort(a, 1, 50)        ! sorts in place in ascending order 
!  Call qsort(a, 1, 50, index) ! returns permutation index, a is not changed
!  Call qsort(a, 1, 50, order=DECENDING) ! sorts in decending order

  USE ISO_FORTRAN_ENV, ONLY: INT8, INT16, INT32, INT64, REAL32, REAL64 

  Implicit NONE

  PRIVATE

  Integer, PARAMETER :: SWITCHSORTS = 10
  
  Real(REAL32),     SAVE  :: savedR32   = 0.0_REAL32
  Real(REAL64),     SAVE  :: savedR64   = 0.0_REAL64
  Integer(INT32),   SAVE  :: savedI32   = 0_INT32
  Integer(INT64),   SAVE  :: savedI64   = 0_INT64
  Character(LEN=1), SAVE  :: savedChar  = ' ' 
  Integer,          SAVE  :: savedIndex = 0

  Logical, Parameter  :: ASCENDING = .TRUE. 
  Logical, Parameter  :: DESCENDING = .FALSE. 

  Interface qsort
    Module Procedure qsortInPlaceR32
    Module Procedure qsortIndexR32
    Module Procedure qsortInPlaceR64
    Module Procedure qsortIndexR64
    Module Procedure qsortInPlaceI32
    Module Procedure qsortIndexI32
    Module Procedure qsortInPlaceI64
    Module Procedure qsortIndexI64
    Module Procedure qsortInPlaceChar
    Module Procedure qsortIndexChar
  End Interface

  Public :: qsort, ASCENDING, DESCENDING

Contains

  Subroutine qsortInPlaceR32(a, left, right, order) 

    Implicit NONE

    Integer,      Intent(IN)             :: left, right
    Real(REAL32), Intent(INOUT)          :: a(left:right)
    Logical,      Intent(IN),   OPTIONAL :: order

    Logical :: sortorder
    
    sortorder = ASCENDING
    If (PRESENT(order)) sortorder=order
    
    Call quicksortIP(left, right)

    Call insertionSort

  Contains
      
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Real(REAL32)   :: tr32

      tr32 = a(i)
      a(i) = a(j)
      a(j) = tr32

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (sortorder .EQV. ASCENDING) Then
        compare = a(i) < a(j)
      Else
        compare = a(i) > a(j)
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      a(i) = a(j)

    End Subroutine moveValue
    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      If (sortorder .EQV. ASCENDING) Then
        compareValue = savedR32 < a(j)
      Else
        compareValue = savedR32 > a(j)
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      savedR32 = a(i)

    End Subroutine saveValue

    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      a(i) = savedR32

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIP(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIP(left, i-1)
        Call quicksortIP(i+1, right)
      EndIf

    End Subroutine quicksortIP

  End Subroutine qsortInPlaceR32

  Subroutine qsortIndexR32(a, left, right, permute, order)

    Implicit NONE

    Integer,      Intent(IN)           :: left, right
    Real(REAL32), Intent(INOUT)        :: a(left:right)
    Integer,      Intent(INOUT)        :: permute(left:right)
    Logical,      Intent(IN), OPTIONAL :: order

    Integer :: j
    Logical :: sortorder

    sortorder = ASCENDING
    If (PRESENT(order)) sortorder = order
  
    Do j=left,right

      permute(j) = j

    EndDo 

    Call quicksortIndex(left, right)

    Call insertionSort

  Contains
 
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: tndx 
      
      tndx = permute(i)
      permute(i) = permute(j)
      permute(j) = tndx

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: ii, ij

      ii = permute(i)
      ij = permute(j)

      If (sortorder .EQV. ASCENDING) Then
        compare = a(ii) < a(ij)
      Else
        compare = a(ii) > a(ij)
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      permute(i) = permute(j)

    End Subroutine moveValue

    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      Integer :: ij

      ij = permute(j)
      If (sortorder .EQV. ASCENDING) Then
        compareValue = savedR32 < a(ij)
      Else
        compareValue = savedR32 > a(ij)
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i
      
      savedIndex = permute(i)
      savedR32   = a(savedIndex)
      
    End Subroutine saveValue
    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      permute(i) = savedIndex

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIndex(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIndex(left, i-1)
        Call quicksortIndex(i+1, right)
   
      EndIf

    End Subroutine quickSortIndex

  End Subroutine qsortIndexR32

  Subroutine qsortInPlaceR64(a, left, right, order)

    Implicit NONE

    Integer,      Intent(IN)             :: left, right
    Real(REAL64), Intent(INOUT)          :: a(left:right)
    Logical,      Intent(IN),   OPTIONAL :: order

    Logical :: sortorder

    sortorder = ASCENDING
    If (PRESENT(order)) sortorder = order

    Call quicksortIP(left, right)

    Call insertionSort

  Contains
      
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Real(REAL64)   :: tr64

      tr64 = a(i)
      a(i) = a(j)
      a(j) = tr64

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (sortorder .EQV. ASCENDING) Then
        compare = a(i) < a(j)
      Else
        compare = a(i) > a(j)
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      a(i) = a(j)

    End Subroutine moveValue
    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      If (sortorder .EQV. ASCENDING) Then
        compareValue = savedR64 < a(j)
      Else
        compareValue = savedR64 > a(j)
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      savedR64 = a(i)

    End Subroutine saveValue

    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      a(i) = savedR64

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIP(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIP(left, i-1)
        Call quicksortIP(i+1, right)
      EndIf

    End Subroutine quicksortIP

  End Subroutine qsortInPlaceR64

  Subroutine qsortIndexR64(a, left, right, permute, order)

    Implicit NONE

    Integer,      Intent(IN)            :: left, right
    Real(REAL64), Intent(INOUT)         :: a(left:right)
    Integer,      Intent(INOUT)         :: permute(left:right)
    Logical,      Intent(IN),  OPTIONAL :: order

    Logical :: sortorder
    Integer :: j

    sortorder = ASCENDING
    If(PRESENT(order)) sortorder = order

    Do j=left,right

      permute(j) = j

    EndDo 

    Call quicksortIndex(left, right)

    Call insertionSort

  Contains
 
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: tndx 
      
      tndx       = permute(i)
      permute(i) = permute(j)
      permute(j) = tndx

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: ii, ij

      ii = permute(i)
      ij = permute(j)

      If (sortorder .EQV. ASCENDING) Then
        compare = a(ii) < a(ij)
      Else
        compare = a(ii) > a(ij)
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      permute(i) = permute(j)

    End Subroutine moveValue

    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      Integer :: ij

      ij           = permute(j)
      If (sortorder .EQV. ASCENDING) Then
        compareValue = savedR64 < a(ij)
      Else
        compareValue = savedR64 > a(ij)
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i
      
      savedIndex = permute(i)
      savedR64 = a(savedIndex)
      
    End Subroutine saveValue
    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      permute(i) = savedIndex

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIndex(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIndex(left, i-1)
        Call quicksortIndex(i+1, right)
   
      EndIf

    End Subroutine quickSortIndex

  End Subroutine qsortIndexR64

  Subroutine qsortInPlaceI32(a, left, right, order)

    Implicit NONE

    Integer,        Intent(IN)             :: left, right
    Integer(INT32), Intent(INOUT)          :: a(left:right)
    Logical,        Intent(IN),   OPTIONAL :: order

    Logical :: sortorder

    sortorder = ASCENDING
    If (PRESENT(order)) sortorder = order

    Call quicksortIP(left, right)

    Call insertionSort

  Contains
      
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer(INT32) :: ti32

      ti32 = a(i)
      a(i) = a(j)
      a(j) = ti32

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (sortorder .EQV. ASCENDING) Then
        compare = a(i) < a(j)
      Else
        compare = a(i) > a(j)
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      a(i) = a(j)

    End Subroutine moveValue
    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      If (sortorder .EQV. ASCENDING) Then
        compareValue = savedI32 < a(j)
      Else
        compareValue = savedI32 > a(j)
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      savedI32 = a(i)

    End Subroutine saveValue

    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      a(i) = savedI32

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIP(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIP(left, i-1)
        Call quicksortIP(i+1, right)
      EndIf

    End Subroutine quicksortIP

  End Subroutine qsortInPlaceI32

  Subroutine qsortIndexI32(a, left, right, permute, order)

    Implicit NONE

    Integer,        Intent(IN)             :: left, right
    Integer(INT32), Intent(INOUT)          :: a(left:right)
    Integer,        Intent(INOUT)          :: permute(left:right)
    Logical,        Intent(IN),   OPTIONAL :: order

    Integer :: j
    Logical :: sortorder

    sortorder = ASCENDING
    If (PRESENT(order)) sortorder = order

    Do j=left,right

      permute(j) = j

    EndDo 

    Call quicksortIndex(left, right)

    Call insertionSort

  Contains
 
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: tndx 
      
      tndx = permute(i)
      permute(i) = permute(j)
      permute(j) = tndx

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: ii, ij

      ii = permute(i)
      ij = permute(j)

      If (sortorder .EQV. ASCENDING) Then
        compare = a(ii) < a(ij)
      Else
        compare = a(ii) > a(ij)
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      permute(i) = permute(j)

    End Subroutine moveValue

    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      Integer :: ij

      ij = permute(j)
      If (sortorder .EQV. ASCENDING) Then
        compareValue = savedI32 < a(ij)
      Else
        compareValue = savedI32 > a(ij)
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i
      
      savedIndex = permute(i)
      savedI32 = a(savedIndex)
      
    End Subroutine saveValue
    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      permute(i) = savedIndex

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIndex(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIndex(left, i-1)
        Call quicksortIndex(i+1, right)
   
      EndIf

    End Subroutine quickSortIndex

  End Subroutine qsortIndexI32

  Subroutine qsortInPlaceI64(a, left, right, order)

    Implicit NONE

    Integer,        Intent(IN)             :: left, right
    Integer(INT64), Intent(INOUT)          :: a(left:right)
    Logical,        Intent(IN),   OPTIONAL :: order

    Logical :: sortorder

    sortorder = ASCENDING
    If (PRESENT(order)) sortorder = order

    Call quicksortIP(left, right)

    Call insertionSort

  Contains
      
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer(INT64) :: ti64

      ti64 = a(i)
      a(i) = a(j)
      a(j) = ti64

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (sortorder .EQV. ASCENDING) Then
        compare = a(i) < a(j)
      Else
        compare = a(i) > a(j)
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      a(i) = a(j)

    End Subroutine moveValue
    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      If (sortorder .EQV. ASCENDING) Then
        compareValue = savedI64 < a(j)
      Else
        compareValue = savedI64 > a(j)
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      savedI64 = a(i)

    End Subroutine saveValue

    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      a(i) = savedI64

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIP(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIP(left, i-1)
        Call quicksortIP(i+1, right)
      EndIf

    End Subroutine quicksortIP

  End Subroutine qsortInPlaceI64

  Subroutine qsortIndexI64(a, left, right, permute, order)

    Implicit NONE

    Integer,        Intent(IN)             :: left, right
    Integer(INT64), Intent(INOUT)          :: a(left:right)
    Integer,        Intent(INOUT)          :: permute(left:right)
    Logical,        Intent(IN),   OPTIONAL :: order

    Integer :: j
    Logical :: sortorder

    sortorder = ASCENDING
    If (PRESENT(order)) sortorder = order

    Do j=left,right

      permute(j) = j

    EndDo 

    Call quicksortIndex(left, right)

    Call insertionSort

  Contains
 
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: tndx 
      
      tndx = permute(i)
      permute(i) = permute(j)
      permute(j) = tndx

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: ii, ij

      ii = permute(i)
      ij = permute(j)

      If (sortorder .EQV. ASCENDING) Then
        compare = a(ii) < a(ij)
      Else
        compare = a(ii) > a(ij)
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      permute(i) = permute(j)

    End Subroutine moveValue

    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      Integer :: ij

      ij = permute(j)
      If (sortorder .EQV. ASCENDING) Then
        compareValue = savedI64 < a(ij)
      Else
        compareValue = savedI64 > a(ij)
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i
      
      savedIndex = permute(i)
      savedI64 = a(savedIndex)
      
    End Subroutine saveValue
    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      permute(i) = savedIndex

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIndex(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIndex(left, i-1)
        Call quicksortIndex(i+1, right)
   
      EndIf

    End Subroutine quickSortIndex

  End Subroutine qsortIndexI64

  Subroutine qsortInPlaceChar(a, left, right, order)

    Implicit NONE

    Integer,          Intent(IN)             :: left, right
    Character(LEN=1), Intent(INOUT)          :: a(left:right)
    Logical,          Intent(IN),   OPTIONAL :: order

    Logical :: sortorder

    sortorder = ASCENDING
    If (PRESENT(order)) sortorder = order

    Call quicksortIP(left, right)

    Call insertionSort

  Contains
      
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Character(LEN=1) :: tchar

      tchar = a(i)
      a(i)  = a(j)
      a(j)  = tchar 

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (sortorder .EQV. ASCENDING) Then
        compare = LLT(a(i), a(j))
      Else
        compare = LGT(a(i),a(j))
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      a(i) = a(j)

    End Subroutine moveValue
    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      If (sortorder .EQV. ASCENDING) Then
        compareValue = LLT(savedChar, a(j))
      Else
        compareValue = LGT(savedChar, a(j))
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      savedChar = a(i)

    End Subroutine saveValue

    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      a(i) = savedChar

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIP(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIP(left, i-1)
        Call quicksortIP(i+1, right)
      EndIf

    End Subroutine quicksortIP

  End Subroutine qsortInPlaceChar

  Subroutine qsortIndexChar(a, left, right, permute, order)

    Implicit NONE

    Integer,          Intent(IN)             :: left, right
    Character(LEN=1), Intent(INOUT)          :: a(left:right)
    Integer,          Intent(INOUT)          :: permute(left:right)
    Logical,          Intent(IN),   OPTIONAL :: order

    Integer :: j
    Logical :: sortorder

    sortorder = ASCENDING
    If (PRESENT(order)) sortorder = order

    Do j=left,right

      permute(j) = j

    EndDo 

    Call quicksortIndex(left, right)

    Call insertionSort

  Contains
 
    Subroutine exchange(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: tndx
      
      tndx = permute(i)
      permute(i) = permute(j)
      permute(j) = tndx

    End Subroutine exchange

    Logical Function compare(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      Integer :: ii, ij

      ii = permute(i)
      ij = permute(j)

      If (sortorder .EQV. ASCENDING) Then
        compare = LLT(a(ii), a(ij))
      Else
        compare = LGT(a(ii), a(ij))
      EndIf

    End Function compare

    Subroutine compex(i,j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      If (compare(j,i)) Call exchange(i,j)

    End Subroutine compex

    Subroutine moveValue(i, j)

      Implicit NONE

      Integer, Intent(IN) :: i, j

      permute(i) = permute(j)

    End Subroutine moveValue

    Logical Function compareValue(j)

      Implicit NONE

      Integer, Intent(IN) :: j

      Integer :: ij

      ij = permute(j)
      If (sortorder .EQV. ASCENDING) Then
        compareValue = LLT(savedChar, a(ij))
      Else
        compareValue = LGT(savedChar, a(ij))
      EndIf

    End Function compareValue

    Subroutine saveValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i
      
      savedIndex = permute(i)
      savedChar = a(savedIndex)
      
    End Subroutine saveValue
    Subroutine restoreValue(i)

      Implicit NONE

      Integer, Intent(IN) :: i

      permute(i) = savedIndex

    End Subroutine restoreValue

    Subroutine insertionSort()

      Implicit NONE

      Integer :: i, j

      Do i=left+1, right
        Call compex(left,i)
      EndDo

      Do i=left+2, right

        j=i
        Call saveValue(i)

        Do While(compareValue(j-1))
          Call moveValue(j,j-1)
          j=j-1
        EndDo
     
        Call restoreValue(j)

      EndDo

    End Subroutine insertionSort

    Function partition(left, right) RESULT(i)

      Implicit NONE

      Integer, Intent(IN) :: left, right
      Integer :: i, j

      i = left - 1
      j = right

      Call saveValue(right)
   
      Loop1 : Do

        Loop2 : Do

          i = i+1
      
          If (i>right) EXIT Loop2
          If (compareValue(i)) EXIT Loop2
        
        EndDo Loop2

        Loop3: Do

          j = j-1
          If (.NOT. compareValue(j) .OR. j==left) EXIT LOOP3

        EndDo Loop3

        If (i>=j) EXIT Loop1
        Call exchange(i, j)

      EndDo Loop1

      Call exchange(i, right)

    End Function partition

    Recursive Subroutine quickSortIndex(left, right)

      Implicit NONE

      Integer, Intent(IN) :: left, right

      Integer :: i

      If ((right-left) > SWITCHSORTS) Then

        Call exchange((right+left)/2, (right-1))
        Call compex(left, right-1) 
        Call compex(right, left) 
        Call compex(right-1, right)
        i    = partition(left+1, right-1)
        Call quicksortIndex(left, i-1)
        Call quicksortIndex(i+1, right)
   
      EndIf

    End Subroutine quickSortIndex

  End Subroutine qsortIndexChar

End Module quicksort 
