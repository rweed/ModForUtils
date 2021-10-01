   
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
   
Program testfindloc

  USE ISO_FORTRAN_ENV, ONLY: INT32, INT64, REAL32, REAL64

#ifdef NO_F2008_FINDLOC
  USE findlocUtils, ONLY: FINDLOC=>findloc_no08
#endif

  Implicit NONE

  Integer :: i, il
  Integer :: ila(1)
  Integer(INT32) :: ival4
  Integer(INT64) :: ival8
  Real(REAL64)   :: rval
  Integer :: ila2d(2)
  Integer :: il3(3)
  Integer, ALLOCATABLE :: ial3(:)

  Integer(INT32) :: ia1d(5)
  Integer(INT32) :: ia2d2(2,3)
  Real(REAL64)   :: ra1d(5)
  Logical        :: mask1d(5)
  Logical        :: mask2d(3,3)
  Real(REAL64)   :: ra2d(3,3)
  Integer(INT64) :: ia2d(3,3)
  Integer(INT32), ALLOCATABLE :: b(:,:,:)
  Integer,        ALLOCATABLE :: index(:,:)

  ia1d = [10, 1, -20, 21, 3]
  ra1d = [3., 20., 21.2, -3., 3.]
  ia2d2 = RESHAPE([1, 2, 2, 3,4,5],[2,3])
 
  ra2d = RESHAPE([1.1,3.,5.2,7., 9., 11., -13.6, 100.4, 12.],[3,3])
  ia2d = RESHAPE([3, -100, 20, 21, 31, 500, 1000, 1, 6],[3,3])


  mask1d = .TRUE.
  mask2d = .TRUE.
 
  Print *,''
  Print *,''
#ifdef NO_F2008_FINDLOC
  Print *,'      ***** TEST OF FINDLOC REPlACEMENT FUNCTIONS *****'
#else 
  Print *,'      ***** TEST OF INTRINSIC FINDLOC *****'
#endif

  Print *,' 1D arrays'
  Print *,' '
  Write(*,'(" Integer32 1D = ", 5I4)') ia1d(:)  
  Write(*,'(" Real64 1D    = ", 5F10.1)') ra1d(:)
  Print *,' '
  Print *,' 2d Integer64 array'
  Print *,' '
  Do i=1,3
    Write(*,'(3I5)') ia2d(i,:)
  EndDo
  Print *,' '
  Print *,' 2d Real64 array'
  Print *,' '
  Do i=1,3
    Write(*,'(3F10.1)') ra2d(i,:)
  EndDo

  Print *,''
  Print *,' 1D array tests'
  Print *,''

  ival4 = 21
  il = FINDLOC(ia1d, ival4, DIM=1)
  ival4 = 3
  ila = FINDLOC(ia1d, ival4)

  Print *,' il for ival = 21, DIM=1 : ', il
  Print *,' ila for ival = 3 : ', ila(1)
  rval = 21.2

  il = FINDLOC(ra1d, rval, DIM=1)

  Print *,' il for rval = 21.2, DIM=1 : ', il
  rval = -3.0
!  il = FINDLOC(ra1d,rval, DIM=1, BACK=.TRUE.)
!  Print *,' il for rval = -3.0, DIM=1, BACK=.true. : ', il
  ila = FINDLOC(ra1d,rval, BACK=.TRUE.)
  Print *,' ila for rval = -3.0, BACK=.true. : ', ila(1)

  ila = FINDLOC((ra1d<0.0_REAL64),.TRUE.)  
  Print *,' ila for rval < 0.0 : ', ila(1)

  Print *,''
  Print *,' 2D array tests'
  Print *,''

  ival8 = 1000_INT64
  ila2d = FINDLOC(ia2d,ival8)
  Write(*,'(" ila2d for ival8 = 1000 : ", 2I5)') ila2d(:)
  Print *,''

  rval = -13.6   
  ila2d = FINDLOC(ra2d, rval) 
  Write(*,' (" ila2d for rval = -13.6 : ", 2I5)') ila2d(:)
  Print *,''
 
  ra2d = -ra2d
  ila2d = FINDLOC((ra2d>0.0_REAL64), .TRUE.)
  Write(*,' (" ila2d for rval8 >0  : ", 2I5)') ila2d(:)
  Print *,''
 
  ra2d = -ra2d
  rval=11.0
  ila2d = FINDLOC(ra2d, rval, BACK=.TRUE.)
  Write(*,' (" ila2d for rval8 rval=11 BACK=.TRUE  : ", 2I5)') ila2d(:)
  Print *,''

  ival4 = 2
  ial3 = FINDLOC(ia2d2, ival4, DIM=1)
  Write(*,' (" ial3 for ia2d2 ival4=2, DIM=1 : ", 3I5)') ial3(:)
  Print *,''
  ial3 = FINDLOC(ia2d2, ival4, DIM=1, BACK=.TRUE.)
  Write(*,' (" ial3 for ia2d2 ival4=2, DIM=1, BACK=.TRUE. : ", 3I5)') ial3(:)
  Print *,''
  ial3 = FINDLOC(ia2d2, ival4, DIM=2)
  Write(*,' (" ial3 for ia2d2 ival4=2, DIM=2 : ", 2I5)') ial3(:)
  Print *,''
  ial3 = FINDLOC(ia2d2, ival4, DIM=2, BACK=.TRUE.)
  Write(*,' (" ial3 for ia2d2 ival4=2, DIM=1, BACK=.TRUE. : ", 2I5)') ial3(:)
   
  
  Print *,''
  Print *,' 3D array tests'

  ival4 = 27

  ALLOCATE(b(4,2,3))

  b = RESHAPE([1, 2, 3, 4, 5, 27, 6, 7, 8, 9, 10, 27, 11, 27, 12, 13, 14, &
              18, 27, 20, 15, 16, 17, 27],[4,2,3])

  il3 = FINDLOC(b, ival4)
  Print *,''
  Print *,' FINDLOC(b,val) test'
  Print *,''
  Write(*,'(" index 3d for ival= 27 : ",3I5)') il3(:)
  il3 = FINDLOC(b, ival4, BACK=.TRUE.)
  Print *,''
  Write(*,'(" index 3d for ival= 27, BACK=.TRUE. : ",3I5)') il3(:)
  
  Print *,''
  Print *,' FINDLOC(b,val, DIM) tests'
  Print *,''


  ALLOCATE(index(2,3))
  Print *,''
  Print *,' i planes'
  Print *,''
  Print *,' b i=1'
  Print *,''
  Do i=1,2
    Write(*,'(3I5)') b(1,i,:)
  EndDo
  Print *,''
  Print *,' b i=2'
  Print *,''
  Do i=1,2
    Write(*,'(3I5)') b(2,i,:)
  EndDo
  Print *,''
  Print *,' b i=3'
  Print *,''
  Do i=1,2
    Write(*,'(3I5)') b(3,i,:)
  EndDo

  Print *,''
  Print *,' b i=4'
  Print *,''
  Do i=1,2
    Write(*,'(3I5)') b(4,i,:)
  EndDo
  Print *,''
  index = FINDLOC(b, ival4, DIM=1)
  Write(*,'(" b3d, val=27, DIM=1 : ")')
  Do i=1,2
    Write(*,'(3I5)') index(i,:)
  EndDo
  Print *,''
  index = FINDLOC(b, ival4, DIM=1, BACK=.TRUE.)
  Write(*,'(" b3d, val=27, DIM=1, BACK=.TRUE. : ")')
  Do i=1,2
    Write(*,'(3I5)') index(i,:)
  EndDo


  DEALLOCATE(index)
  ALLOCATE(index(4,3))
  Print *,''
  Print *,' j planes'
  Print *,''
  Print *,' b j=1'
  Print *,''
  Do i=1,4
    Write(*,'(3I5)') b(i,1,:)
  EndDo
  Print *,''
  Print *,' b j=2'
  Print *,''
  Do i=1,4
    Write(*,'(3I5)') b(i,2,:)
  EndDo

  Print *,''
  index = FINDLOC(b, ival4, DIM=2)
  Write(*,'(" b3d, val=27, DIM=2 : ")')
  Print *,''
  Do i=1,4
    Write(*,'(3I5)') index(i,:)
  EndDo

  Print *,''
  index = FINDLOC(b, ival4, DIM=2, BACK=.TRUE.)
  Write(*,'(" b3d, val=27, DIM=2, BACK=.TRUE. : ")')
  Print *,''
  Do i=1,4
    Write(*,'(3I5)') index(i,:)
  EndDo

  Print *,''
  Print *,' k planes'
  Print *,''
  Print *,' b k=1'
  Print *,''
  Do i=1,4
    Write(*,'(2I5)') b(i,:,1)
  EndDo
  Print *,''
  Print *,' b k=2'
  Print *,''
  Do i=1,4
    Write(*,'(2I5)') b(i,:,2)
  EndDo
  Print *,''
  Print *,' b k=3'
  Print *,''
  Do i=1,4
    Write(*,'(2I5)') b(i,:,3)
  EndDo
 
  Print *,''
  DEALLOCATE(index)
  index = FINDLOC(b, ival4, DIM=3)
  Write(*,'(" b3d, val=27, DIM=3 : ")')
  Print *,''
  Do i=1,4
    Write(*,'(2I5)') index(i,:)
  EndDo
  DEALLOCATE(index)
  Print *,''
  index = FINDLOC(b, ival4, DIM=3, BACK=.TRUE.)
  Write(*,'(" b3d, val=27, DIM=3, BACK=.TRUE. : ")')
  Print *,''
  Print *,' shape index = ', shape(index)
  Do i=1,4
    Write(*,'(2I5)') index(i,:)
  EndDo

End Program testfindloc
