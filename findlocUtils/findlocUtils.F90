   
!  Copyright (C) 2021, Richard Weed.
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
!  3. Neither the name of the copyright holder nor the names of 
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
!  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE
!  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!  POSSIBILITY OF SUCH DAMAGE.
   
Module findlocUtils

  USE ISO_FORTRAN_ENV, ONLY: INT32, INT64, REAL32, REAL64

  Implicit NONE

  PRIVATE
  SAVE

  Interface findloc_no08
    Module Procedure findloc1D_I32
    Module Procedure findloc1D_I64
    Module Procedure findloc1D_R32
    Module Procedure findloc1D_R64
    Module Procedure findloc1D_Logical
    Module Procedure findloc1DDIM1_I32
    Module Procedure findloc1DDIM1_I64
    Module Procedure findloc1DDIM1_R32
    Module Procedure findloc1DDIM1_R64
    Module Procedure findloc1DDIM1_Logical
    Module Procedure findloc2D_I32
    Module Procedure findloc2D_I64
    Module Procedure findloc2D_R32
    Module Procedure findloc2D_R64
    Module Procedure findloc2D_Logical
    Module Procedure findloc2DDIM_I32
    Module Procedure findloc2DDIM_I64
    Module Procedure findloc2DDIM_R32
    Module Procedure findloc2DDIM_R64
    Module Procedure findloc2DDIM_Logical
    Module Procedure findloc3D_I32
    Module Procedure findloc3D_I64
    Module Procedure findloc3D_R32
    Module Procedure findloc3D_R64
    Module Procedure findloc3D_Logical
    Module Procedure findloc3DDIM_I32
    Module Procedure findloc3DDIM_I64
    Module Procedure findloc3DDIM_R32
    Module Procedure findloc3DDIM_R64
    Module Procedure findloc3DDIM_Logical

  End Interface

  PUBLIC :: findloc_no08

Contains

! Provide replacements for F2008 findloc function if not suppored
! by compiler. Basic logic is stolen from NASA Glenn FR code and modified
! to make mask an optional argument and provide support for 1D, 2D and
! 3D arrays. INT32, INT64, REAL32, REAL64, and Logical arrays only
! are supported. Also, the DIM argument form is supported but
! the result for all arrays > rank 1 must be allocatable arrays

  Pure Function findloc1D_I32(a, value, mask, back) Result(index)

    Implicit NONE

    Integer(INT32), Intent(IN)           :: a(:)
    Integer(INT32), Intent(IN)           :: value
    Logical,        Intent(IN), OPTIONAL :: mask(:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer                              :: index(1)

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0
    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index(1) = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1) == value)      &
                .AND.       m(SIZE(a):1:-1)),DIM=1)
      If (index(1) > 0) index(1) = UBOUND(a,DIM=1)-index(1) + 1
    Else

      index(1) = MAXLOC(MERGE(1,0, a == value .AND. m), dim=1)

    EndIf

  End Function findloc1D_I32

  Pure Function findloc1D_I64(a, value, mask, back) Result(index)

    Implicit NONE

    Integer(INT64), Intent(IN)           :: a(:)
    Integer(INT64), Intent(IN)           :: value
    Logical,        Intent(IN), OPTIONAL :: mask(:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer                              :: index(1)

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index(1) = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1) == value)                &
                 .AND.       m(size(a):1:-1)),DIM=1)
      If (index(1) > 0) index(1) = UBOUND(a,DIM=1)-index(1) + 1
    Else

      index(1) = MAXLOC(MERGE(1,0, (a == value) .AND. m), dim=1)

    EndIf

  End Function findloc1D_I64

  Pure Function findloc1D_R32(a, value, mask, back) Result(index)

    Implicit NONE

    Real(REAL32), Intent(IN)           :: a(:)
    Real(REAL32), Intent(IN)           :: value
    Logical,      Intent(IN), OPTIONAL :: mask(:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer                            :: index(1)

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT(a==value .AND. m) == 0) RETURN
   
    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index(1) = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1) == value)      &
                 .AND.          m(SIZE(a):1:-1)),DIM=1)
      If (index(1) > 0) index(1) = UBOUND(a,DIM=1)-index(1) + 1
    Else

      index(1) = MAXLOC(MERGE(1,0, (a == value) .AND. m), dim=1)

    EndIf

  End Function findloc1D_R32

  Pure Function findloc1D_R64(a, value, mask, back) Result(index)

    Implicit NONE

    Real(REAL64), Intent(IN)           :: a(:)
    Real(REAL64), Intent(IN)           :: value
    Logical,      Intent(IN), OPTIONAL :: mask(:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer                            :: index(1)

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT((a==value) .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index(1) = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1) == value) .AND.     &
                     m(SIZE(a):1:-1)),DIM=1)
      If (index(1) > 0) index(1) = UBOUND(a,DIM=1)-index(1) + 1
    Else

      index(1) = MAXLOC(MERGE(1,0, (a == value) .AND. m), dim=1)

    EndIf

  End Function findloc1D_R64

  Pure Function findloc1D_Logical(a, value, mask, back) Result(index)

    Implicit NONE

    Logical, Intent(IN)           :: a(:)
    Logical, Intent(IN)           :: value
    Logical, Intent(IN), OPTIONAL :: mask(:)
    Logical, Intent(IN), OPTIONAL :: back

    Integer                       :: index(1)

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT((a.EQV.value) .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1).EQV.value)             &
              .AND.       m(SIZE(a):1:-1)),DIM=1)
      If (index(1) > 0) index(1) = UBOUND(a,DIM=1)-index(1) + 1
    Else

      index = MAXLOC(MERGE(1,0, (a.EQV.value) .AND. m), dim=1)

    EndIf

  End Function findloc1D_Logical

  Pure Function findloc1DDIM1_I32(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Integer(INT32), Intent(IN)           :: a(:)
    Integer(INT32), Intent(IN)           :: value
    Integer,        Intent(IN)           :: dim 
    Logical,        Intent(IN), OPTIONAL :: mask(:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer                              :: index

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1) == value) .AND.          &
                     m(size(a):1:-1)),DIM=dim)
      If (index > 0) index = UBOUND(a,DIM=1)-index + 1
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m), dim=dim)

    EndIf

  End Function findloc1DDIM1_I32

  Pure Function findloc1DDIM1_I64(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Integer(INT64), Intent(IN)           :: a(:)
    Integer(INT64), Intent(IN)           :: value
    Integer,        Intent(IN)           :: dim 
    Logical,        Intent(IN), OPTIONAL :: mask(:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer                              :: index

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1) == value) .AND.          &
                     m(size(a):1:-1)),DIM=dim)
      If (index > 0) index = UBOUND(a,DIM=1)-index + 1
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m), DIM=dim)

    EndIf

  End Function findloc1DDIM1_I64

  Pure Function findloc1DDIM1_R32(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Real(REAL32), Intent(IN)           :: a(:)
    Real(REAL32), Intent(IN)           :: value
    Integer,      Intent(IN)           :: dim
    Logical,      Intent(IN), OPTIONAL :: mask(:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer                              :: index

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT(a==value .AND. m) == 0) RETURN
   
    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1) == value) .AND.          &
                     m(size(a):1:-1)),DIM=dim)
      If (index > 0) index = UBOUND(a,DIM=1)-index + 1
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m), dim=dim)

    EndIf

  End Function findloc1DDIM1_R32

  Pure Function findloc1DDIM1_R64(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Real(REAL64), Intent(IN)           :: a(:)
    Real(REAL64), Intent(IN)           :: value
    Integer,      Intent(IN)           :: dim
    Logical,      Intent(IN), OPTIONAL :: mask(:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer                            :: index

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1) == value) .AND.          &
                     m(size(a):1:-1)),DIM=dim)
      If (index > 0) index = UBOUND(a,DIM=1)-index + 1
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m), dim=dim)

    EndIf

  End Function findloc1DDIM1_R64

  Pure Function findloc1DDIM1_Logical(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Logical, Intent(IN)           :: a(:)
    Logical, Intent(IN)           :: value
    Integer, Intent(IN)           :: dim
    Logical, Intent(IN), OPTIONAL :: mask(:)
    Logical, Intent(IN), OPTIONAL :: back

    Integer                       :: index

    Logical :: reverse
    Logical, ALLOCATABLE :: m(:)

    index = 0

    ALLOCATE(m(SIZE(a,DIM=1)))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT((a.EQV.value) .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(SIZE(a):1:-1).EQV.value)   &
              .AND.         m(SIZE(a):1:-1)),DIM=dim)
      If (index > 0) index = UBOUND(a,DIM=1)-index + 1
    Else

      index = MAXLOC(MERGE(1,0, (a.EQV.value) .AND. m), DIM=dim)

    EndIf

  End Function findloc1DDIM1_Logical

  Pure Function findloc2D_I32(a, value, mask, back) Result(index)

    Implicit NONE

    Integer(INT32), Intent(IN)           :: a(:,:)
    Integer(INT32), Intent(IN)           :: value
    Logical,        Intent(IN), OPTIONAL :: mask(:,:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer                            :: index(2)

    Integer :: im, jm
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im,jm))
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1) == value) .AND.      &
                     m(im:1:-1, jm:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
      EndIf 
   Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m))

    EndIf

  End Function findloc2D_I32

  Pure Function findloc2D_I64(a, value, mask, back) Result(index)

    Implicit NONE

    Integer(INT64), Intent(IN)           :: a(:,:)
    Integer(INT64), Intent(IN)           :: value
    Logical,        Intent(IN), OPTIONAL :: mask(:,:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer                            :: index(2)

    Integer :: im, jm
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im,jm))
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1) == value) .AND.      &
                     m(im:1:-1, jm:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
      EndIf 
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m))

    EndIf

  End Function findloc2d_I64

  Pure Function findloc2D_R32(a, value, mask, back) Result(index)

    Implicit NONE

    Real(REAL32), Intent(IN)           :: a(:,:)
    Real(REAL32), Intent(IN)           :: value
    Logical,      Intent(IN), OPTIONAL :: mask(:,:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer                            :: index(2)

    Integer :: im, jm
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im,jm))
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1) == value) .AND.      &
                     m(im:1:-1, jm:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
      EndIf 
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m))

    EndIf

  End Function findloc2d_R32

  Pure Function findloc2D_R64(a, value, mask, back) Result(index)

    Implicit NONE

    Real(REAL64), Intent(IN)           :: a(:,:)
    Real(REAL64), Intent(IN)           :: value
    Logical,      Intent(IN), OPTIONAL :: mask(:,:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer                            :: index(2)

    Integer :: im, jm
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im,jm))
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1) == value) .AND.      &
                     m(im:1:-1, jm:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
      EndIf 
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m))

    EndIf

  End Function findloc2d_R64

  Pure Function findloc2d_Logical(a, value, mask, back) Result(index)

    Implicit NONE

    Logical, Intent(IN)           :: a(:,:)
    Logical, Intent(IN)           :: value
    Logical, Intent(IN), OPTIONAL :: mask(:,:)
    Logical, Intent(IN), OPTIONAL :: back

    Integer                       :: index(2)

    Integer :: im, jm
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
   
    ALLOCATE(m(im,jm))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If(COUNT((a.EQV.value) .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1).EQV.value) .AND.     &
                     m(im:1:-1, jm:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
      EndIf 
    Else

      index = MAXLOC(MERGE(1,0, (a.EQV.value) .AND. m))

    EndIf

  End Function findloc2D_Logical

  Pure Function findloc2DDIM_I32(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Integer(INT32), Intent(IN)           :: a(:,:)
    Integer(INT32), Intent(IN)           :: value
    Integer,        Intent(IN)           :: dim
    Logical,        Intent(IN), OPTIONAL :: mask(:,:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer,        ALLOCATABLE          :: index(:)

    Integer :: im, jm, i
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)
    Logical, ALLOCATABLE :: m2(:,:)

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im, jm))
    ALLOCATE(m2(im, jm))

    Select Case (dim)

      CASE(1)
        ALLOCATE(index(jm))
      CASE(2)
        ALLOCATE(index(im))
    End Select
    index = 0
 
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1, jm:1:-1)==value) .AND.                 &
            m(im:1:-1,jm:1:-1))) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1) == value) .AND.      &
                     m(im:1:-1, jm:1:-1)),DIM=dim,MASK=m2)
      index(:) = index(SIZE(index):1:-1)

      Do i=1,SIZE(index)
        If (index(i) > 0) Then
          index(i) = UBOUND(a,DIM=dim) - index(i) + 1
        EndIf
      EndDo
   Else

      m2 = (MERGE(1,0, (a==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a==value) .AND. m),DIM=dim, MASK=m2)

    EndIf

  End Function findloc2DDIM_I32

  Pure Function findloc2DDIM_I64(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Integer(INT64), Intent(IN)           :: a(:,:)
    Integer(INT64), Intent(IN)           :: value
    Integer,        Intent(IN)           :: dim
    Logical,        Intent(IN), OPTIONAL :: mask(:,:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer,        ALLOCATABLE          :: index(:)

    Integer :: im, jm, i
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)
    Logical, ALLOCATABLE :: m2(:,:)


    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im, jm))
    ALLOCATE(m2(im, jm))

    Select Case (dim)

      CASE(1)
        ALLOCATE(index(jm))
      CASE(2)
        ALLOCATE(index(im))
    End Select
    index = 0

    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1, jm:1:-1)==value) .AND.                 &
            m(im:1:-1,jm:1:-1))) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1) == value) .AND.      &
                     m(im:1:-1, jm:1:-1)),DIM=dim, MASK=m2)

      index(:) = index(SIZE(index):1:-1)
      Do i=1,SIZE(index)
        If (index(i) > 0) Then
          index(i) = UBOUND(a,DIM=dim) - index(i) + 1
        EndIf
      EndDo

   Else

      m2 = (MERGE(1,0, (a==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a==value) .AND. m),DIM=dim, MASK=m2)

    EndIf

  End Function findloc2DDIM_I64

  Pure Function findloc2DDIM_R32(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Real(REAL32), Intent(IN)           :: a(:,:)
    Real(REAL32), Intent(IN)           :: value
    Integer,      Intent(IN)           :: dim
    Logical,      Intent(IN), OPTIONAL :: mask(:,:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer,      ALLOCATABLE          :: index(:)

    Integer :: im, jm, i
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)
    Logical, ALLOCATABLE :: m2(:,:)


    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im, jm))
    ALLOCATE(m2(im, jm))

    Select Case (dim)

      CASE(1)
        ALLOCATE(index(jm))
      CASE(2)
        ALLOCATE(index(im))
    End Select
    index = 0
 
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1, jm:1:-1)==value) .AND.                 &
            m(im:1:-1,jm:1:-1))) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1) == value) .AND.      &
                     m(im:1:-1, jm:1:-1)),DIM=dim, MASK=m2)

      index(:) = index(SIZE(index):1:-1)
      Do i=1,SIZE(index)
        If (index(i) > 0) Then
          index(i) = UBOUND(A,DIM=dim) - index(i) + 1
        EndIf
      EndDo

   Else

      m2 = (MERGE(1,0, (a==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a==value) .AND. m),DIM=dim, MASK=m2)

    EndIf

  End Function findloc2DDIM_R32

  Pure Function findloc2DDIM_R64(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Real(REAL64), Intent(IN)           :: a(:,:)
    Real(REAL64), Intent(IN)           :: value
    Integer,      Intent(IN)           :: dim
    Logical,      Intent(IN), OPTIONAL :: mask(:,:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer,      ALLOCATABLE          :: index(:)

    Integer :: im, jm, i
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)
    Logical, ALLOCATABLE :: m2(:,:)


    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im, jm))
    ALLOCATE(m2(im, jm))

    Select Case (dim)

      CASE(1)
        ALLOCATE(index(jm))
      CASE(2)
        ALLOCATE(index(im))
    End Select
    index = 0
 
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1, jm:1:-1)==value) .AND.                 &
            m(im:1:-1,jm:1:-1))) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1) == value) .AND.      &
                     m(im:1:-1, jm:1:-1)),DIM=dim, MASK=m2)

      index(:) = index(SIZE(index):1:-1)
      Do i=1,SIZE(index)
        If (index(i) > 0) Then
          index(i) = UBOUND(a,DIM=dim) - index(i) + 1
        EndIf
      EndDo

   Else

      m2 = (MERGE(1,0, (a==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a==value) .AND. m),DIM=dim, MASK=m2)

    EndIf

  End Function findloc2DDIM_R64

  Pure Function findloc2DDIM_Logical(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Logical, Intent(IN)           :: a(:,:)
    Logical, Intent(IN)           :: value
    Integer, Intent(IN)           :: dim
    Logical, Intent(IN), OPTIONAL :: mask(:,:)
    Logical, Intent(IN), OPTIONAL :: back

    Integer, ALLOCATABLE          :: index(:)

    Integer :: im, jm, i
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:)
    Logical, ALLOCATABLE :: m2(:,:)


    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)

    ALLOCATE(m(im, jm))
    ALLOCATE(m2(im, jm))

    Select Case (dim)

      CASE(1)
        ALLOCATE(index(jm))
      CASE(2)
        ALLOCATE(index(im))
    End Select
    index = 0
 
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT((a.EQV.value) .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1, jm:1:-1).EQV.value) .AND.                 &
            m(im:1:-1,jm:1:-1))) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1).EQV.value) .AND.      &
                     m(im:1:-1, jm:1:-1)),DIM=dim, MASK=m2)

      index(:) = index(SIZE(index):1:-1)
      Do i=1,SIZE(index)
        If (index(i) > 0) Then
          index(i) = UBOUND(a,DIM=dim) - index(i) + 1
        EndIf
      EndDo

   Else

      m2 = (MERGE(1,0, (a.EQV.value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a.EQV.value) .AND. m),DIM=dim, MASK=m2)

    EndIf

  End Function findloc2DDIM_Logical

  Pure Function findloc3D_I32(a, value, mask, back) Result(index)

    Implicit NONE

    Integer(INT32), Intent(IN)           :: a(:,:,:)
    Integer(INT32), Intent(IN)           :: value
    Logical,        Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer                            :: index(3)

    Integer :: im, jm, km
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1)==value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
        index(3) = UBOUND(a,DIM=3)- index(3) + 1
      EndIf 
    Else

      index = MAXLOC(MERGE(1,0, (a == value) .AND. m))

    EndIf

  End Function findloc3D_I32

  Pure Function findloc3D_I64(a, value, mask, back) Result(index)

    Implicit NONE

    Integer(INT64), Intent(IN)           :: a(:,:,:)
    Integer(INT64), Intent(IN)           :: value
    Logical,        Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer                              :: index(3)

    Integer :: im, jm, km
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1) == value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
        index(3) = UBOUND(a,DIM=3)- index(3) + 1
      EndIf 
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m))

    EndIf

  End Function findloc3d_I64

  Pure Function findloc3D_R32(a, value, mask, back) Result(index)

    Implicit NONE

    Real(REAL32), Intent(IN)           :: a(:,:,:)
    Real(REAL32), Intent(IN)           :: value
    Logical,      Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer                            :: index(3)

    Integer :: im, jm, km
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)


    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1)==value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)))

      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
        index(3) = UBOUND(a,DIM=3)- index(3) + 1
      EndIf 

    Else

      index = MAXLOC(MERGE(1,0, (a == value) .AND. m))

    EndIf

  End Function findloc3d_R32

  Pure Function findloc3D_R64(a, value, mask, back) Result(index)

    Implicit NONE

    Real(REAL64), Intent(IN)           :: a(:,:,:)
    Real(REAL64), Intent(IN)           :: value
    Logical,      Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer                            :: index(3)

    Integer :: im, jm, km
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1)==value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1)- index(1) + 1
        index(2) = UBOUND(a,DIM=2)- index(2) + 1
        index(3) = UBOUND(a,DIM=3)- index(3) + 1
      EndIf 
    Else

      index = MAXLOC(MERGE(1,0, (a==value) .AND. m))

    EndIf

  End Function findloc3d_R64
  
  Pure Function findloc3d_Logical(a, value, mask, back) Result(index)

    Implicit NONE

    Logical, Intent(IN)           :: a(:,:,:)
    Logical, Intent(IN)           :: value
    Logical, Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical, Intent(IN), OPTIONAL :: back

    Integer                       :: index(3)

    Integer :: im, jm, km
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)

    index = 0

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    m = .TRUE.
    If (PRESENT(mask)) m = mask

    If (COUNT(a.EQV.value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      index = MAXLOC(MERGE(1, 0, (a(im:1:-1, jm:1:-1, km:1:-1).EQV.value)  &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)))
      If (ALL(index > 0)) Then
        index(1) = UBOUND(a,DIM=1) - index(1) + 1
        index(2) = UBOUND(a,DIM=2) - index(2) + 1
        index(3) = UBOUND(a,DIM=3) - index(3) + 1
      EndIf 

    Else

      index = MAXLOC(MERGE(1,0, (a.EQV.value) .AND. m))

    EndIf

  End Function findloc3D_Logical

  Pure Function findloc3DDIM_I32(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Integer(INT32), Intent(IN)           :: a(:,:,:)
    Integer(INT32), Intent(IN)           :: value
    Integer,        Intent(IN)           :: dim
    Logical,        Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer,        ALLOCATABLE          :: index(:,:)

    Integer :: im, jm, km, i, j
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)
    Logical, ALLOCATABLE :: m2(:,:,:)

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    ALLOCATE(m2(im, jm, km))

    Select Case(dim)

      Case (1)
        ALLOCATE(index(jm,km))
      Case(2)
        ALLOCATE(index(im,km))
      Case(3)
        ALLOCATE(index(im,jm))

    End Select

    index = 0
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1,jm:1:-1, km:1:-1)==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1)==value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)),DIM=dim,       &
                     MASK=m2)

      im = SIZE(index,DIM=1)
      jm = SIZE(index,DIM=2)
      index(:,:) = index(im:1:-1, jm:1:-1)
      Do j=1,jm
        Do i=1,im
          If (index(i,j) > 0) Then
            index(i,j) = UBOUND(a,DIM=dim) - index(i,j) + 1
          EndIf
        EndDo
      EndDo

    Else

      m2 = (MERGE(1,0, (a==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, ((a==value) .AND. m)),DIM=dim, MASK=m2)

    EndIf

  End Function findloc3DDIM_I32

  Pure Function findloc3DDIM_I64(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Integer(INT64), Intent(IN)           :: a(:,:,:)
    Integer(INT64), Intent(IN)           :: value
    Integer,        Intent(IN)           :: dim
    Logical,        Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical,        Intent(IN), OPTIONAL :: back

    Integer,        ALLOCATABLE          :: index(:,:)

    Integer :: im, jm, km, i, j
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)
    Logical, ALLOCATABLE :: m2(:,:,:)


    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    ALLOCATE(m2(im, jm, km))

    Select Case(dim)

      Case (1)
        ALLOCATE(index(jm,km))
      Case(2)
        ALLOCATE(index(im,km))
      Case(3)
        ALLOCATE(index(im,jm))

    End Select

    index = 0
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1,jm:1:-1, km:1:-1)==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1)==value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)),DIM=dim,       &
                     MASK=m2)

      im = SIZE(index,DIM=1)
      jm = SIZE(index,DIM=2)
      index(:,:) = index(im:1:-1, jm:1:-1)
      Do j=1,jm
        Do i=1,im
          If (index(i,j) > 0) Then
            index(i,j) = UBOUND(a,DIM=dim) - index(i,j) + 1
          EndIf
        EndDo
      EndDo

    Else

      m2 = (MERGE(1,0, (a==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a==value) .AND. m),DIM=dim)

    EndIf

  End Function findloc3DDIM_I64

  Pure Function findloc3DDIM_R32(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Real(REAL32), Intent(IN)           :: a(:,:,:)
    Real(REAL32), Intent(IN)           :: value
    Integer,      Intent(IN)           :: dim
    Logical,      Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer,      ALLOCATABLE          :: index(:,:)

    Integer :: im, jm, km, i, j
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)
    Logical, ALLOCATABLE :: m2(:,:,:)


    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    ALLOCATE(m2(im, jm, km))

    Select Case(dim)

      Case (1)
        ALLOCATE(index(jm,km))
      Case(2)
        ALLOCATE(index(im,km))
      Case(3)
        ALLOCATE(index(im,jm))

    End Select

    index = 0
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1,jm:1:-1, km:1:-1)==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1)==value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)),DIM=dim,       &
                     MASK=m2)

      im = SIZE(index,DIM=1)
      jm = SIZE(index,DIM=2)
      index(:,:) = index(im:1:-1, jm:1:-1)
      Do j=1,jm
        Do i=1,im
          If (index(i,j) > 0) Then
            index(i,j) = UBOUND(a,DIM=dim) - index(i,j) + 1
          EndIf
        EndDo
      EndDo

    Else

      m2 = (MERGE(1,0, (a==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a==value) .AND. m),DIM=dim)

    EndIf

  End Function findloc3DDIM_R32

  Pure Function findloc3DDIM_R64(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Real(REAL64), Intent(IN)           :: a(:,:,:)
    Real(REAL64), Intent(IN)           :: value
    Integer,      Intent(IN)           :: dim
    Logical,      Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical,      Intent(IN), OPTIONAL :: back

    Integer,      ALLOCATABLE          :: index(:,:)

    Integer :: im, jm, km, i, j
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)
    Logical, ALLOCATABLE :: m2(:,:,:)


    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    ALLOCATE(m2(im, jm, km))
    Select Case(dim)

      Case (1)
        ALLOCATE(index(jm,km))
      Case(2)
        ALLOCATE(index(im,km))
      Case(3)
        ALLOCATE(index(im,jm))

    End Select

    index = 0
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT(a==value .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1,jm:1:-1, km:1:-1)==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1)==value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)),DIM=dim,       &
                     MASK=m2)

      im = SIZE(index,DIM=1)
      jm = SIZE(index,DIM=2)
      index(:,:) = index(im:1:-1, jm:1:-1)
      Do j=1,jm
        Do i=1,im
          If (index(i,j) > 0) Then
            index(i,j) = UBOUND(a,DIM=dim) - index(i,j) + 1
          EndIf
        EndDo
      EndDo

    Else

      m2 = (MERGE(1,0, (a==value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a==value) .AND. m),DIM=dim)

    EndIf

  End Function findloc3DDIM_R64

  Pure Function findloc3DDIM_Logical(a, value, dim, mask, back) Result(index)

    Implicit NONE

    Logical, Intent(IN)           :: a(:,:,:)
    Logical, Intent(IN)           :: value
    Integer, Intent(IN)           :: dim
    Logical, Intent(IN), OPTIONAL :: mask(:,:,:)
    Logical, Intent(IN), OPTIONAL :: back

    Integer,      ALLOCATABLE          :: index(:,:)

    Integer :: im, jm, km, i , j
    Logical :: reverse
    Logical, ALLOCATABLE :: m(:,:,:)
    Logical, ALLOCATABLE :: m2(:,:,:)

    im = SIZE(a,DIM=1)
    jm = SIZE(a,DIM=2)
    km = SIZE(a,DIM=3)

    ALLOCATE(m(im, jm, km))
    ALLOCATE(m2(im, jm, km))

    Select Case(dim)

      Case (1)
        ALLOCATE(index(jm,km))
      Case(2)
        ALLOCATE(index(im,km))
      Case(3)
        ALLOCATE(index(im,jm))

    End Select

    index = 0
    m = .TRUE.
    If (PRESENT(mask)) m=mask

    If(COUNT((a.EQV.value) .AND. m) == 0) RETURN

    reverse = .FALSE.

    If (PRESENT(back)) reverse = back

    If (reverse) Then

      m2 = (MERGE(1,0, (a(im:1:-1,jm:1:-1, km:1:-1).EQV.value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a(im:1:-1, jm:1:-1, km:1:-1).EQV.value)   &
                     .AND. m(im:1:-1, jm:1:-1, km:1:-1)),DIM=dim,          &
                      MASK=m2)

      im = SIZE(index,DIM=1)
      jm = SIZE(index,DIM=2)
      index(:,:) = index(im:1:-1, jm:1:-1)
      Do j=1,jm
        Do i=1,im
          If (index(i,j) > 0) Then
            index(i,j) = UBOUND(a,DIM=dim) - index(i,j) + 1
          EndIf
        EndDo
      EndDo

    Else

      m2 = (MERGE(1,0, (a.EQV.value) .AND. m)) > 0
      index = MAXLOC(MERGE(1,0, (a.EQV.value) .AND. m),DIM=dim)

    EndIf

  End Function findloc3DDIM_Logical

End Module findlocUtils
