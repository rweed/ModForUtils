
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

 
Module sortUtils

! Sorting utils. Insertion short for short lists plus mergeSort and heapsorrt
! for longer arrays. quickSort is also brought in from quickSort module to give 
! one set of interfaces for all sorters. Wrapper routines are provided
! for the actual sorting routines to allow sorting arrays in place (
! ie original array is overwritten by sorted array) or return the
! sorted values in a separate array 

!  Insertion sort and heap sort routines are based on example code from 
!  roseettcode.org

!  merge sort routines are based on example code found at
!    http://www-h.eng.cam.ac.uk/help/languages/fortran/f90/examples.html


  USE ISO_FORTRAN_ENV, ONLY: INT8, INT16, INT32, INT64, REAL32, REAL64
  USE quickSort, ONLY: qsort, ASCENDING, DESCENDING

  PRIVATE

  Interface isort
    Module Procedure insertionSortI8 
    Module Procedure insertionSortI16 
    Module Procedure insertionSortI32 
    Module Procedure insertionSortI64
    Module Procedure insertionSortR32 
    Module Procedure insertionSortR64
  End Interface

  Interface msort
    Module Procedure mergeSortI8 
    Module Procedure mergeSortI16 
    Module Procedure mergeSortI32 
    Module Procedure mergeSortI64
    Module Procedure mergeSortR32 
    Module Procedure mergeSortR64
  End Interface

  Interface hsort
    Module Procedure heapSortI8 
    Module Procedure heapSortI16 
    Module Procedure heapSortI32 
    Module Procedure heapSortI64
    Module Procedure heapSortR32 
    Module Procedure heapSortR64
  End Interface

  Interface csort
    Module Procedure countingSortI8 
    Module Procedure countingSortI16 
    Module Procedure countingSortI32 
    Module Procedure countingSortI64
  End Interface 

  PUBLIC :: isort, msort, hsort, csort, qsort, ASCENDING, DESCENDING

Contains

  Subroutine insertionSortI8(a,b)

    Implicit NONE

    Integer(INT8),           Intent(INOUT) :: a(:)
    Integer(INT8), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      Call isort8(b)
    Else
      Call isort8(a)
    EndIf

  Contains

    Subroutine isort8(a)

      Implicit NONE

      Integer(INT8),  Intent(INOUT) :: a(:)

      Integer :: n, i, j
      Integer(INT8) :: temp

      n = SIZE(a, DIM=1)
      If (n == 2) Then
        temp = a(2)
        If (temp < a(1)) Then
          a(2) = a(1)
          a(1) = temp
        End If
      Else  
        Do i=2,n
          temp = a(i)
          j  = i-1
          Do
            If (j<1) EXIT
            If (a(j) <= temp) EXIT
            a(j+1) = a(j)
            j = j-1
          EndDo
          a(j+1) = temp 
        EndDo
      EndIf

    End Subroutine isort8

  End Subroutine insertionSortI8

  Subroutine insertionSortI16(a,b)

    Implicit NONE

    Integer(INT16),           Intent(INOUT) :: a(:)
    Integer(INT16), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      Call isort16(b)
    Else
      Call isort16(a)
    EndIf

  Contains

    Subroutine isort16(a)

      Implicit NONE

      Integer(INT16),  Intent(INOUT) :: a(:)

      Integer :: n, i, j
      Integer(INT16) :: temp

      n = SIZE(a,DIM=1)
      If (n == 2) Then
        temp = a(2)
        If (temp < a(1)) Then
          a(2) = a(1)
          a(1) = temp
        End If
      Else  
        Do i=2,n
          temp = a(i)
          j  = i-1
          Do
            If (j<1) EXIT
            If (a(j) <= temp) EXIT
            a(j+1) = a(j)
            j = j-1
          EndDo
          a(j+1) = temp 
        EndDo
      End If
    End Subroutine isort16

  End Subroutine insertionSortI16

  Subroutine insertionSortI32(a,b)

    Implicit NONE

    Integer(INT32),           Intent(INOUT) :: a(:)
    Integer(INT32), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      Call isort32(b)
    Else
      Call isort32(a)
    EndIf

  Contains

    Subroutine isort32(a)

      Implicit NONE

      Integer(INT32),  Intent(INOUT) :: a(:)

      Integer :: n, i, j
      Integer(INT32) :: temp

      n = SIZE(a,DIM=1)
      If (n == 2) Then
        temp = a(2)
        If (temp < a(1)) Then
          a(2) = a(1)
          a(1) = temp
        End If
      Else  
        Do i=2,n
          temp = a(i)
          j  = i-1
          Do
            If (j<1) EXIT
            If (a(j) <= temp) EXIT
            a(j+1) = a(j)
            j = j-1
          EndDo
          a(j+1) = temp 
        EndDo
      End If
    End Subroutine isort32

  End Subroutine insertionSortI32

  Subroutine insertionSortI64(a,b)

    Implicit NONE

    Integer(INT64),           Intent(INOUT) :: a(:)
    Integer(INT64), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      Call isort64(b)
    Else
      Call isort64(a)
    EndIf

  Contains

    Subroutine isort64(a)

      Implicit NONE

      Integer(INT64), Intent(INOUT) :: a(:)
    
      Integer        :: n, i, j
      Integer(INT64) ::  temp

      n = SIZE(a)
      If (n == 2) Then
        temp = a(2)
        If (temp < a(1)) Then
          a(2) = a(1)
          a(1) = temp
        End If
      Else  
        Do i=2,n
          temp = a(i)
          j  = i-1
          Do
            If (j<1) EXIT
            If (a(j) <= temp) EXIT
            a(j+1) = a(j)
            j = j-1
          EndDo
          a(j+1) = temp 
        EndDo
      End If
    End Subroutine isort64

  End Subroutine insertionSortI64

  Subroutine insertionSortR32(a,b)

    Implicit NONE

    Real(REAL32),           Intent(INOUT) :: a(:)
    Real(REAL32), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      Call rsort32(b)
    Else
      Call rsort32(a)
    EndIf

  Contains

    Subroutine rsort32(a)

      Implicit NONE

      Real(REAL32),  Intent(INOUT) :: a(:)

      Integer      :: n, i, j
      Real(REAL32) :: temp

      n = SIZE(a,DIM=1)
      If (n == 2) Then
        temp = a(2)
        If (temp < a(1)) Then
          a(2) = a(1)
          a(1) = temp
        End If
      Else  
        Do i=2,n
          temp = a(i)
          j  = i-1
          Do
            If (j<1) EXIT
            If (a(j) <= temp) EXIT
            a(j+1) = a(j)
            j = j-1
          EndDo
          a(j+1) = temp 
        EndDo
      End If
    End Subroutine rsort32

  End Subroutine insertionSortR32

  Subroutine insertionSortR64(a,b)

    Implicit NONE

    Real(REAL64),           Intent(INOUT) :: a(:)
    Real(REAL64), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      Call rsort64(b)
    Else
      Call rsort64(a)
    EndIf

  Contains

    Subroutine rsort64(a)

      Implicit NONE

      Real(REAL64),  Intent(INOUT) :: a(:)

      Integer      :: n, i, j
      Real(REAL64) :: temp

      n = SIZE(a,DIM=1)
      If (n == 2) Then
        temp = a(2)
        If (temp < a(1)) Then
          a(2) = a(1)
          a(1) = temp
        End If
      Else  
        Do i=2,n
          temp = a(i)
          j  = i-1
          Do
            If (j<1) EXIT
            If (a(j) <= temp) EXIT
            a(j+1) = a(j)
            j = j-1
          EndDo
          a(j+1) = temp 
        EndDo
      End If
    End Subroutine rsort64

  End Subroutine insertionSortR64
 
  Subroutine mergeSortI8(a, ascending, b)

    Implicit NONE

    Integer(INT8),           Intent(INOUT) :: a(:)
    Logical,        OPTIONAL, Intent(IN)    :: ascending 
    Integer(INT8), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      If (PRESENT(ascending)) Then
        Call msortI8(b, ascending)
      Else
        Call msortI8(b)
      EndIf
    Else
      If (PRESENT(ascending)) Then
        Call msortI8(a, ascending)
      Else
        Call msortI8(a)
      EndIf
    EndIf

  Contains

    Recursive Subroutine msortI8 (a, ascending)

      Implicit NONE

      Integer(INT8), Intent(INOUT) :: a(:)
      Logical, Intent(IN), OPTIONAL :: ascending
      Logical :: up

      Integer :: low, high, mid

!   Set sorting order. Default is ascending
   
      up = .TRUE.
      If (PRESENT(ascending)) up = ascending 

      low  = LBOUND(a,1)
      high = UBOUND(a,1)

      If ( low < high) Then
        mid = (low+high)/2
        Call msortI8(a(low:mid), up)
        Call msortI8(a(mid+1:high), up)
        a(low:high) = mergeI8(a(low:mid), a(mid+1:high), up)
      EndIf

    End Subroutine msortI8

    Function mergeI8 (a, b, up) Result(merge)

      Integer(INT8),  Intent(INOUT) :: a(:), b(:)
      Logical, Intent(IN) :: up

      Integer(INT8)  :: merge(SIZE(a)+SIZE(b))

      Integer :: ia, ha, la
      Integer :: ib, hb, lb
      Integer :: ic

      Logical :: condition

      la = LBOUND(a,1)
      ha = UBOUND(a,1)
      lb = LBOUND(b,1)
      hb = UBOUND(b,1)

      ia = la
      ib = lb
      ic = 1

      Do While (ia<=ha .AND. ib<=hb)

        If (up) Then
          condition= (a(ia) <= b(ib))
        Else
          condition= (a(ia) >= b(ib))
        EndIf

        If (condition) Then
          merge(ic)=a(ia)
          ia=ia+1
        Else
          merge(ic)=b(ib)
          ib=ib+1
        EndIf

        ic = ic + 1

      EndDo

      If (ia>ha) Then
        merge(ic:) = b(ib:hb)
      Else
        merge(ic:) = a(ia:ha)
      EndIf

    End Function mergeI8

  End Subroutine mergeSortI8

  Subroutine mergeSortI16(a, ascending, b)

    Implicit NONE

    Integer(INT16),           Intent(INOUT) :: a(:)
    Logical,        OPTIONAL, Intent(IN)    :: ascending 
    Integer(INT16), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      If (PRESENT(ascending)) Then
        Call msortI16(b, ascending)
      Else
        Call msortI16(b)
      EndIf
    Else
      If (PRESENT(ascending)) Then
        Call msortI16(a, ascending)
      Else
        Call msortI16(a)
      EndIf
    EndIf

  Contains

    Recursive Subroutine msortI16 (a, ascending)

      Implicit NONE

      Integer(INT16), Intent(INOUT) :: a(:)
      Logical, Intent(IN), OPTIONAL :: ascending
      Logical :: up

      Integer :: low, high, mid

!   Set sorting order. Default is ascending
   
      up = .TRUE.
      If (PRESENT(ascending)) up = ascending 

      low  = LBOUND(a,1)
      high = UBOUND(a,1)

      If ( low < high) Then
        mid = (low+high)/2
        Call msortI16(a(low:mid), up)
        Call msortI16(a(mid+1:high), up)
        a(low:high) = mergeI16(a(low:mid), a(mid+1:high), up)
      EndIf

    End Subroutine msortI16

    Function mergeI16 (a, b, up) Result(merge)

      Integer(INT16),  Intent(INOUT) :: a(:), b(:)
      Logical, Intent(IN) :: up

      Integer(INT16)  :: merge(SIZE(a)+SIZE(b))

      Integer :: ia, ha, la
      Integer :: ib, hb, lb
      Integer :: ic

      Logical :: condition

      la = LBOUND(a,1)
      ha = UBOUND(a,1)
      lb = LBOUND(b,1)
      hb = UBOUND(b,1)

      ia = la
      ib = lb
      ic = 1

      Do While (ia<=ha .AND. ib<=hb)

        If (up) Then
          condition= (a(ia) <= b(ib))
        Else
          condition= (a(ia) >= b(ib))
        EndIf

        If (condition) Then
          merge(ic)=a(ia)
          ia=ia+1
        Else
          merge(ic)=b(ib)
          ib=ib+1
        EndIf

        ic = ic + 1

      EndDo

      If (ia>ha) Then
        merge(ic:) = b(ib:hb)
      Else
        merge(ic:) = a(ia:ha)
      EndIf

    End Function mergeI16

  End Subroutine mergeSortI16

  Subroutine mergeSortI32(a, ascending, b)

    Implicit NONE

    Integer(INT32),           Intent(INOUT) :: a(:)
    Logical,        OPTIONAL, Intent(IN)    :: ascending 
    Integer(INT32), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      If (PRESENT(ascending)) Then
        Call msortI32(b, ascending)
      Else
        Call msortI32(b)
      EndIf
    Else
      If (PRESENT(ascending)) Then
        Call msortI32(a, ascending)
      Else
        Call msortI32(a)
      EndIf
    EndIf

  Contains

    Recursive Subroutine msortI32 (a, ascending)

      Implicit NONE

      Integer(INT32), Intent(INOUT) :: a(:)
      Logical, Intent(IN), OPTIONAL :: ascending
      Logical :: up

      Integer :: low, high, mid

!   Set sorting order. Default is ascending
   
      up = .TRUE.
      If (PRESENT(ascending)) up = ascending 

      low  = LBOUND(a,1)
      high = UBOUND(a,1)

      If ( low < high) Then
        mid = (low+high)/2
        Call msortI32(a(low:mid), up)
        Call msortI32(a(mid+1:high), up)
        a(low:high) = mergeI32(a(low:mid), a(mid+1:high), up)
      EndIf

    End Subroutine msortI32

    Function mergeI32 (a, b, up) Result(merge)

      Integer(INT32),  Intent(INOUT) :: a(:), b(:)
      Logical, Intent(IN) :: up

      Integer(INT32)  :: merge(SIZE(a)+SIZE(b))

      Integer :: ia, ha, la
      Integer :: ib, hb, lb
      Integer :: ic

      Logical :: condition

      la = LBOUND(a,1)
      ha = UBOUND(a,1)
      lb = LBOUND(b,1)
      hb = UBOUND(b,1)

      ia = la
      ib = lb
      ic = 1

      Do While (ia<=ha .AND. ib<=hb)

        If (up) Then
          condition= (a(ia) <= b(ib))
        Else
          condition= (a(ia) >= b(ib))
        EndIf

        If (condition) Then
          merge(ic)=a(ia)
          ia=ia+1
        Else
          merge(ic)=b(ib)
          ib=ib+1
        EndIf

        ic = ic + 1

      EndDo

      If (ia>ha) Then
        merge(ic:) = b(ib:hb)
      Else
        merge(ic:) = a(ia:ha)
      EndIf

    End Function mergeI32

  End Subroutine mergeSortI32

  Subroutine mergeSortI64(a, ascending, b)

    Implicit NONE

    Integer(INT64),           Intent(INOUT) :: a(:)
    Logical,        OPTIONAL, Intent(IN)    :: ascending 
    Integer(INT64), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      If (PRESENT(ascending)) Then
        Call msortI64(b, ascending)
      Else
        Call msortI64(b)
      EndIf
    Else
      If (PRESENT(ascending)) Then
        Call msortI64(a, ascending)
      Else
        Call msortI64(a)
      EndIf
    EndIf

  Contains

    Recursive Subroutine msortI64 (a, ascending)

      Implicit NONE

      Integer(INT64),           Intent(INOUT) :: a(:)
      Logical,        OPTIONAL, Intent(IN)    :: ascending

      Logical :: up

      Integer :: low, high, mid

!   Set sorting order. Default is ascending
   
      up = .TRUE.
      If (PRESENT(ascending)) up = ascending 

      low  = LBOUND(a,1)
      high = UBOUND(a,1)

      If ( low < high) Then
        mid = (low+high)/2
        Call msortI64(a(low:mid), up)
        Call msortI64(a(mid+1:high), up)
        a(low:high) = mergeI64(a(low:mid), a(mid+1:high), up)
      EndIf

    End Subroutine msortI64

    Function mergeI64 (a, b, up) Result(merge)

      Integer(INT64), Intent(INOUT) :: a(:), b(:)
      Logical,        Intent(IN) :: up

      Integer(INT64)  :: merge(SIZE(a)+SIZE(b))

      Integer :: ia, ha, la
      Integer :: ib, hb, lb
      Integer :: ic

      Logical :: condition

      la = LBOUND(a,1)
      ha = UBOUND(a,1)
      lb = LBOUND(b,1)
      hb = UBOUND(b,1)

      ia = la
      ib = lb
      ic = 1

      Do While (ia<=ha .AND. ib<=hb)

        If (up) Then
          condition= (a(ia) <= b(ib))
        Else
          condition= (a(ia) >= b(ib))
        EndIf

        If (condition) Then
          merge(ic)=a(ia)
          ia=ia+1
        Else
          merge(ic)=b(ib)
          ib=ib+1
        EndIf

        ic = ic + 1

      EndDo

      If (ia>ha) Then
        merge(ic:) = b(ib:hb)
      Else
        merge(ic:) = a(ia:ha)
      EndIf

    End Function mergeI64

  End Subroutine mergeSortI64

  Subroutine mergeSortR32(a, ascending, b)

    Implicit NONE

    Real(REAL32),           Intent(INOUT) :: a(:)
    Logical,      OPTIONAL, Intent(IN)    :: ascending 
    Real(REAL32), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      If (PRESENT(ascending)) Then
        Call msortR32(b, ascending)
      Else
        Call msortR32(b)
      EndIf
    Else
      If (PRESENT(ascending)) Then
        Call msortR32(a, ascending)
      Else
        Call msortR32(a)
      EndIf
    EndIf

  Contains

    Recursive Subroutine msortR32 (a, ascending)

      Implicit NONE

      Real(REAL32),           Intent(INOUT) :: a(:)
      Logical,      OPTIONAL, Intent(IN)    :: ascending
      Logical :: up

      Integer :: low, high, mid

!   Set sorting order. Default is ascending
   
      up = .TRUE.
      If (PRESENT(ascending)) up = ascending 

      low  = LBOUND(a,1)
      high = UBOUND(a,1)

      If ( low < high) Then
        mid = (low+high)/2
        Call msortR32(a(low:mid), up)
        Call msortR32(a(mid+1:high), up)
        a(low:high) = mergeR32(a(low:mid), a(mid+1:high), up)
      EndIf

    End Subroutine msortR32
    Function mergeR32 (a, b, up) Result(merge)

      Real(REAL32), Intent(INOUT) :: a(:), b(:)
      Logical,      Intent(IN) :: up

      Real(REAL32)  :: merge(SIZE(a)+SIZE(b))

      Integer :: ia, ha, la
      Integer :: ib, hb, lb
      Integer :: ic

      Logical :: condition

      la = LBOUND(a,1)
      ha = UBOUND(a,1)
      lb = LBOUND(b,1)
      hb = UBOUND(b,1)

      ia = la
      ib = lb
      ic = 1

      Do While (ia<=ha .AND. ib<=hb)

        If (up) Then
          condition= (a(ia) <= b(ib))
        Else
          condition= (a(ia) >= b(ib))
        EndIf

        If (condition) Then
          merge(ic)=a(ia)
          ia=ia+1
        Else
          merge(ic)=b(ib)
          ib=ib+1
        EndIf

        ic = ic + 1

      EndDo

      If (ia>ha) Then
        merge(ic:) = b(ib:hb)
      Else
        merge(ic:) = a(ia:ha)
      EndIf

    End Function mergeR32

  End Subroutine mergeSortR32

  Subroutine mergeSortR64(a, ascending, b)

    Implicit NONE

    Real(REAL64),           Intent(INOUT) :: a(:)
    Logical,      OPTIONAL, Intent(IN)    :: ascending 
    Real(REAL64), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      b = a
      If (PRESENT(ascending)) Then
        Call msortR64(b, ascending)
      Else
        Call msortR64(b)
      EndIf
    Else
      If (PRESENT(ascending)) Then
        Call msortR64(a, ascending)
      Else
        Call msortR64(a)
      EndIf
    EndIf

  Contains

    Recursive Subroutine msortR64 (a, ascending)

      Implicit NONE

      Real(REAL64),           Intent(INOUT) :: a(:)
      Logical,      OPTIONAL, Intent(IN)    :: ascending
      Logical :: up

      Integer :: low, high, mid

!   Set sorting order. Default is ascending
   
      up = .TRUE.
      If (PRESENT(ascending)) up = ascending 

      low  = LBOUND(a,1)
      high = UBOUND(a,1)

      If ( low < high) Then
        mid = (low+high)/2
        Call msortR64(a(low:mid), up)
        Call msortR64(a(mid+1:high), up)
        a(low:high) = mergeR64(a(low:mid), a(mid+1:high), up)
      EndIf

    End Subroutine msortR64

    Function mergeR64 (a, b, up) Result(merge)

      Real(REAL64), Intent(INOUT) :: a(:), b(:)
      Logical,      Intent(IN) :: up

      Real(REAL64)  :: merge(SIZE(a)+SIZE(b))

      Integer :: ia, ha, la
      Integer :: ib, hb, lb
      Integer :: ic

      Logical :: condition

      la = LBOUND(a,1)
      ha = UBOUND(a,1)
      lb = LBOUND(b,1)
      hb = UBOUND(b,1)

      ia = la
      ib = lb
      ic = 1

      Do While (ia<=ha .AND. ib<=hb)

        If (up) Then
          condition= (a(ia) <= b(ib))
        Else
          condition= (a(ia) >= b(ib))
        EndIf

        If (condition) Then
          merge(ic)=a(ia)
          ia=ia+1
        Else
          merge(ic)=b(ib)
          ib=ib+1
        EndIf

        ic = ic + 1

      EndDo

      If (ia>ha) Then
        merge(ic:) = b(ib:hb)
      Else
        merge(ic:) = a(ia:ha)
      EndIf

    End Function mergeR64

  End Subroutine mergeSortR64

  Subroutine heapsortI8(a,b)

    Implicit NONE

    Integer(INT8),           Intent(INOUT) :: a(:)
    Integer(INT8), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      a = b
      Call heapI8(b)
    Else 
      Call heapI8(a)
    End If

  Contains

    Subroutine heapI8(a)

      Integer(INT8), Intent(INOUT) :: a(0:)

      Integer       :: start, n, bottom
      Integer(INT8) :: temp

      n = SIZE(a)
      Do start = (n - 2) / 2, 0, -1
        Call siftdownI8(a, start, n);
      End Do
   
      Do bottom = n - 1, 1, -1
        temp = a(0)
        a(0) = a(bottom)
        a(bottom) = temp;
        Call siftdownI8(a, 0, bottom)
      End Do

    End Subroutine heapI8
  
    Subroutine siftdownI8(a, start, bottom)
  
      Integer(INT8), Intent(INOUT) :: a(0:)

      Integer, Intent(IN) :: start, bottom

      Integer       :: child, root
      Integer(INT8) :: temp

      root = start
      Do
        If (root*2 + 1 >= bottom) EXIT
        child = root * 2 + 1
   
        If (child + 1 < bottom) Then
          If (a(child) < a(child+1)) child = child + 1
        End if
    
        If (a(root) < a(child)) Then
          temp = a(child)
          a(child) = a (root)
          a(root) = temp
          root = child
        Else
          RETURN
        End If  
      End Do      
   
    End Subroutine siftdownI8

  End Subroutine heapsortI8

  Subroutine heapsortI16(a,b)

    Implicit NONE

    Integer(INT16),           Intent(INOUT) :: a(:)
    Integer(INT16), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      a = b
      Call heapI16(b)
    Else 
      Call heapI16(a)
    End If

  Contains

    Subroutine heapI16(a)

      Integer(INT16), Intent(INOUT) :: a(0:)

      Integer        :: start, n, bottom
      Integer(INT16) :: temp

      n = SIZE(a)
      Do start = (n - 2) / 2, 0, -1
        Call siftdownI16(a, start, n);
      End Do
     
      Do bottom = n - 1, 1, -1
        temp = a(0)
        a(0) = a(bottom)
        a(bottom) = temp;
        Call siftdownI16(a, 0, bottom)
      End Do

    End Subroutine heapI16

    Subroutine siftdownI16(a, start, bottom)
  
      Integer(INT16), Intent(INOUT) :: a(0:)

      Integer, Intent(IN) :: start, bottom

      Integer        :: child, root
      Integer(INT16) :: temp

      root = start
      Do
        If (root*2 + 1 >= bottom) EXIT
        child = root * 2 + 1
    
        If (child + 1 < bottom) Then
          If (a(child) < a(child+1)) child = child + 1
        End if
    
        If (a(root) < a(child)) Then
          temp = a(child)
          a(child) = a (root)
          a(root) = temp
          root = child
        Else
          RETURN
        End If  
      End Do      
    
    End Subroutine siftdownI16

  End Subroutine heapsortI16

  Subroutine heapsortI32(a,b)

    Implicit NONE

    Integer(INT32),           Intent(INOUT) :: a(:)
    Integer(INT32), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      a = b
      Call heapI32(b)
    Else 
      Call heapI32(a)
    End If

  Contains

    Subroutine heapI32(a)

      Integer(INT32), Intent(INOUT) :: a(0:)

      Integer        :: start, n, bottom
      Integer(INT32) :: temp

      n = SIZE(a)
      Do start = (n - 2) / 2, 0, -1
        Call siftdownI32(a, start, n);
      End Do
     
      Do bottom = n - 1, 1, -1
        temp = a(0)
        a(0) = a(bottom)
        a(bottom) = temp;
        Call siftdownI32(a, 0, bottom)
      End Do

    End Subroutine heapI32

    Subroutine siftdownI32(a, start, bottom)
  
      Integer(INT32), Intent(INOUT) :: a(0:)

      Integer, Intent(IN) :: start, bottom

      Integer        :: child, root
      Integer(INT32) :: temp

      root = start
      Do
        If (root*2 + 1 >= bottom) EXIT
        child = root * 2 + 1
    
        If (child + 1 < bottom) Then
          If (a(child) < a(child+1)) child = child + 1
        End if
    
        If (a(root) < a(child)) Then
          temp = a(child)
          a(child) = a (root)
          a(root) = temp
          root = child
        Else
          RETURN
        End If  
      End Do      
    
    End Subroutine siftdownI32

  End Subroutine heapsortI32

  Subroutine heapsortI64(a,b)

    Implicit NONE

    Integer(INT64),           Intent(INOUT) :: a(:)
    Integer(INT64), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      a = b
      Call heapI64(b)
    Else 
      Call heapI64(a)
    End If

  Contains

    Subroutine heapI64(a)

      Integer(INT64), Intent(INOUT) :: a(0:)

      Integer        :: start, n, bottom
      Integer(INT64) :: temp

      n = SIZE(a)
      Do start = (n - 2) / 2, 0, -1
        Call siftdownI64(a, start, n);
      End Do
     
      Do bottom = n - 1, 1, -1
        temp = a(0)
        a(0) = a(bottom)
        a(bottom) = temp;
        Call siftdownI64(a, 0, bottom)
      End Do
    End Subroutine heapI64

    Subroutine siftdownI64(a, start, bottom)
  
      Integer(INT64), Intent(INOUT) :: a(0:)

      Integer, Intent(IN) :: start, bottom

      Integer        :: child, root
      Integer(INT64) :: temp

      root = start
      Do
        If (root*2 + 1 >= bottom) EXIT
        child = root * 2 + 1
    
        If (child + 1 < bottom) Then
          If (a(child) < a(child+1)) child = child + 1
        End if
    
        If (a(root) < a(child)) Then
          temp = a(child)
          a(child) = a (root)
          a(root) = temp
          root = child
        Else
          RETURN
        End If  
      End Do      
    
    End Subroutine siftdownI64

  End Subroutine heapsortI64

  Subroutine heapsortR32(a,b)

    Implicit NONE

    Real(REAL32),           Intent(INOUT) :: a(:)
    Real(REAL32), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      a = b
      Call heapR32(b)
    Else 
      Call heapR32(a)
    End If

  Contains

    Subroutine heapR32(a)

      Real(REAL32), Intent(INOUT) :: a(0:)

      Integer      :: start, n, bottom
      Real(REAL32) :: temp

      n = SIZE(a)
      Do start = (n - 2) / 2, 0, -1
        Call siftdownR32(a, start, n);
      End Do
     
      Do bottom = n - 1, 1, -1
        temp = a(0)
        a(0) = a(bottom)
        a(bottom) = temp;
        Call siftdownR32(a, 0, bottom)
      End Do
    End Subroutine heapR32

    Subroutine siftdownR32(a, start, bottom)
  
      Real(REAL32), Intent(INOUT) :: a(0:)

      Integer, Intent(IN) :: start, bottom

      Integer      :: child, root
      Real(REAL32) :: temp

      root = start
      Do
        If (root*2 + 1 >= bottom) EXIT
        child = root * 2 + 1
    
        If (child + 1 < bottom) Then
          If (a(child) < a(child+1)) child = child + 1
        End if
    
        If (a(root) < a(child)) Then
          temp = a(child)
          a(child) = a (root)
          a(root) = temp
          root = child
        Else
          RETURN
        End If  
      End Do      
    
    End Subroutine siftdownR32

  End Subroutine heapsortR32

  Subroutine heapsortR64(a,b)

    Implicit NONE

    Real(REAL64),           Intent(INOUT) :: a(:)
    Real(REAL64), OPTIONAL, Intent(INOUT) :: b(:)

    If (PRESENT(b)) Then
      a = b
      Call heapR64(b)
    Else 
      Call heapR64(a)
    End If

  Contains

    Subroutine heapR64(a)

      Real(REAL64), Intent(INOUT) :: a(0:)

      Integer      :: start, n, bottom
      Real(REAL64) :: temp

      n = SIZE(a)
      Do start = (n - 2) / 2, 0, -1
        Call siftdownR64(a, start, n);
      End Do
     
      Do bottom = n - 1, 1, -1
        temp = a(0)
        a(0) = a(bottom)
        a(bottom) = temp;
        Call siftdownR64(a, 0, bottom)
      End Do

    End Subroutine heapR64

    Subroutine siftdownR64(a, start, bottom)
  
    Real(REAL64), Intent(INOUT) :: a(0:)

      Integer, Intent(IN) :: start, bottom

      Integer      :: child, root
      Real(REAL64) :: temp

      root = start
      Do
        If (root*2 + 1 >= bottom) EXIT
        child = root * 2 + 1
    
        If (child + 1 < bottom) Then
          If (a(child) < a(child+1)) child = child + 1
        End if
    
        If (a(root) < a(child)) Then
          temp = a(child)
          a(child) = a (root)
          a(root) = temp
          root = child
        Else
          RETURN
        End If  
      End Do      
    
    End Subroutine siftdownR64

  End Subroutine heapsortR64

  Subroutine countingSortI8(a, amin, amax)

! Counting sort for integer arrays. Code taken from rosettacode.org

    IMPLICIT NONE

    Integer(INT8),           Intent(INOUT) :: a(:)
    Integer(INT8), OPTIONAL, Intent(IN)    :: amin
    Integer(INT8), OPTIONAL, Intent(IN)    :: amax

    Integer(INT8) :: tmin, tmax

    If (PRESENT(amin) .AND. PRESENT(amax)) Then
      tmin = amin
      tmax = amax
    Else
      tmin = MINVAL(a)
      tmax = MAXVAL(a)
    End If
 
    Call countingSort_mmI8(a, tmin, tmax) 

  Contains 

    Subroutine countingSort_mmI8(a, tmin, tmax)

      IMPLICIT NONE

      Integer(INT8), Intent(INOUT) :: a(:)
      integer(INT8), Intent(IN)    :: tmin, tmax
   
      Integer(INT8) :: cnt(tmin:tmax)
      Integer :: i, j
   
      cnt = 0 

      Do i=1, SIZE(a)
        cnt(a(i)) = cnt(a(i))+1
      End Do 

      j = 1
      Do i = tmin, tmax
        Do 
          If ( cnt(i) <= 0 ) EXIT
            a(j)   = i
            j      = j + 1
            cnt(i) = cnt(i) - 1
        End Do
      End Do
 
    End subroutine countingSort_mmI8

  End subroutine countingSortI8
 
  Subroutine countingSortI16(a, amin, amax)

! Counting sort for integer arrays. Code taken from rosettacode.org

    IMPLICIT NONE

    Integer(INT16),           Intent(INOUT) :: a(:)
    Integer(INT16), OPTIONAL, Intent(IN)    :: amin
    Integer(INT16), OPTIONAL, Intent(IN)    :: amax

    Integer(INT16) :: tmin, tmax

    If (PRESENT(amin) .AND. PRESENT(amax)) Then
      tmin = amin
      tmax = amax
    Else
      tmin = MINVAL(a)
      tmax = MAXVAL(a)
    End If
 
    Call countingSort_mmI16(a, tmin, tmax) 

  Contains 

    Subroutine countingSort_mmI16(a, tmin, tmax)

      IMPLICIT NONE

      Integer(INT16), Intent(INOUT) :: a(:)
      integer(INT16), Intent(IN)    :: tmin, tmax
   
      Integer(INT16) :: cnt(tmin:tmax)
      Integer :: i, j
   
      cnt = 0 

      Do i=1, SIZE(a)
        cnt(a(i)) = cnt(a(i))+1
      End Do 

      j = 1
      Do i = tmin, tmax
        Do 
          If ( cnt(i) <= 0 ) EXIT
            a(j)   = i
            j      = j + 1
            cnt(i) = cnt(i) - 1
        End Do
      End Do
 
    End subroutine countingSort_mmI16

  End subroutine countingSortI16

  Subroutine countingSortI32(a, amin, amax)

! Counting sort for integer arrays. Code taken from rosettacode.org

    IMPLICIT NONE

    Integer(INT32),           Intent(INOUT) :: a(:)
    Integer(INT32), OPTIONAL, Intent(IN)    :: amin
    Integer(INT32), OPTIONAL, Intent(IN)    :: amax

    Integer(INT32) :: tmin, tmax

    If (PRESENT(amin) .AND. PRESENT(amax)) Then
      tmin = amin
      tmax = amax
    Else
      tmin = MINVAL(a)
      tmax = MAXVAL(a)
    End If
 
    Call countingSort_mmI32(a, tmin, tmax) 

  Contains 

    Subroutine countingSort_mmI32(a, tmin, tmax)

      IMPLICIT NONE

      Integer(INT32), Intent(INOUT) :: a(:)
      integer(INT32), Intent(IN)    :: tmin, tmax
   
      Integer(INT32) :: cnt(tmin:tmax)
      Integer :: i, j
   
      cnt = 0 

      Do i=1, SIZE(a)
        cnt(a(i)) = cnt(a(i))+1
      End Do 

      j = 1
      Do i = tmin, tmax
        Do 
          If ( cnt(i) <= 0 ) EXIT
            a(j)   = i
            j      = j + 1
            cnt(i) = cnt(i) - 1
        End Do
      End Do
 
    End subroutine countingSort_mmI32

  End subroutine countingSortI32

  Subroutine countingSortI64(a, amin, amax)

! Counting sort for integer arrays. Code taken from rosettacode.org

    IMPLICIT NONE

    Integer(INT64),           Intent(INOUT) :: a(:)
    Integer(INT64), OPTIONAL, Intent(IN)    :: amin
    Integer(INT64), OPTIONAL, Intent(IN)    :: amax

    Integer(INT64) :: tmin, tmax

    If (PRESENT(amin) .AND. PRESENT(amax)) Then
      tmin = amin
      tmax = amax
    Else
      tmin = MINVAL(a)
      tmax = MAXVAL(a)
    End If
 
    Call countingSort_mmI64(a, tmin, tmax) 

  Contains 

    Subroutine countingSort_mmI64(a, tmin, tmax)

      IMPLICIT NONE

      Integer(INT64), Intent(INOUT) :: a(:)
      integer(INT64), Intent(IN)    :: tmin, tmax
   
      Integer(INT64) :: cnt(tmin:tmax)
      Integer :: i, j
   
      cnt = 0 

      Do i=1, SIZE(a)
        cnt(a(i)) = cnt(a(i))+1
      End Do 

      j = 1
      Do i = tmin, tmax
        Do 
          If ( cnt(i) <= 0 ) EXIT
            a(j)   = i
            j      = j + 1
            cnt(i) = cnt(i) - 1
        End Do
      End Do
 
    End subroutine countingSort_mmI64

  End subroutine countingSortI64

End Module sortUtils 
