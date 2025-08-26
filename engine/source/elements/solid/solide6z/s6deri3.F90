!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    s6cderi3      ../engine/source/elements/thickshell/solide6c/s6cderi3.F
      !||--- called by ------------------------------------------------------
      !||    s6cforc3      ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s6cke3        ../engine/source/elements/thickshell/solide6c/s6cke3.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../engine/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../engine/share/message_module/message_mod.F
      !||====================================================================
      module s6deri3_2_mod
      contains

      !! \brief Compute derivatives and Jacobian matrix for 6-node solid element
      !! \details This routine calculates the Jacobian matrix, its inverse, and related
      !!          shape function derivatives for 6-node solid elements. It also handles
      !!          negative volume detection and correction.
      subroutine s6deri3_2(                   &
        off,     det,     ngl,     x1,      &
        x2,      x3,      x4,      x5,      &
        x6,      y1,      y2,      y3,      &
        y4,      y5,      y6,      z1,      &
        z2,      z3,      z4,      z5,      &
        z6,      px1,     px2,     px3,     &
        px4,     py1,     py2,     py3,     &
        py4,     pz1,     pz2,     pz3,     &
        pz4,     px1h,    px2h,    px3h,    &
        py1h,    py2h,    py3h,    pz1h,    &
        pz2h,    pz3h,    jacob5,  jacob6,  &
        jacob4,  jacob8,  jacob9,  jacob7,  &
        jaci33,  b1x,     b1y,     b2y,     &
        b2x,     b1122,   b1221,   b2212,   &
        b1121,   b1xh,    b1yh,    b2xh,    &
        b2yh,    b1122h,  b1221h,  b2212h,  &
        b1121h,  vzl,     volg,    sav,     &
        offg,    nel,     ismstr,            &
        px5,     py5,     pz5,              &
        px6,     py6,     pz6,              &
        idel7nok, ineg_v,  mstop,volmin,    &
        idtmin  )    

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   MODULES
      ! ----------------------------------------------------------------------------------------------------------------------
        use MESSAGE_MOD
        use MVSIZ_MOD, only : mvsiz
        use PRECISION_MOD, only : wp
        use CONSTANT_MOD, only : zero, one, two, third, fourth, one_over_8, one_over_12

            ! ----------------------------------------------------------------------------------------------------------------------
            !                                                   IMPLICIT NONE
            ! ----------------------------------------------------------------------------------------------------------------------
              implicit none
#include      "units_c.inc"
      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   INCLUDED FILES
      ! ----------------------------------------------------------------------------------------------------------------------
!      #include "implicit_f.inc"
!      #include "comlock.inc"
!      #include "mvsiz_p.inc"
!      #include "com06_c.inc"
!      #include "units_c.inc"
!      #include "scr07_c.inc"
!      #include "scr17_c.inc"
!      #include "scr18_c.inc"


      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   ARGUMENTS
      ! ----------------------------------------------------------------------------------------------------------------------
      integer, intent(in)    :: ismstr                                 !< Material strain option flag
      integer, intent(in)    :: nel                                    !< Number of elements
      integer, intent(in)    :: ngl(nel)                              !< Global element numbers
      
      real(kind=wp), intent(inout) :: off(nel)                        !< Element deactivation flag
      real(kind=wp), intent(out)   :: det(nel)                        !< Jacobian determinant
      
      real(kind=wp), intent(inout)    :: x1(nel)                         !< X coordinate of node 1
      real(kind=wp), intent(inout)    :: x2(nel)                         !< X coordinate of node 2
      real(kind=wp), intent(inout)    :: x3(nel)                         !< X coordinate of node 3
      real(kind=wp), intent(inout)    :: x4(nel)                         !< X coordinate of node 4
      real(kind=wp), intent(inout)    :: x5(nel)                         !< X coordinate of node 5
      real(kind=wp), intent(inout)    :: x6(nel)                         !< X coordinate of node 6
      
      real(kind=wp), intent(inout)    :: y1(nel)                         !< Y coordinate of node 1
      real(kind=wp), intent(inout)    :: y2(nel)                         !< Y coordinate of node 2
      real(kind=wp), intent(inout)    :: y3(nel)                         !< Y coordinate of node 3
      real(kind=wp), intent(inout)    :: y4(nel)                         !< Y coordinate of node 4
      real(kind=wp), intent(inout)    :: y5(nel)                         !< Y coordinate of node 5
      real(kind=wp), intent(inout)    :: y6(nel)                         !< Y coordinate of node 6
      
      real(kind=wp), intent(inout)    :: z1(nel)                         !< Z coordinate of node 1
      real(kind=wp), intent(inout)    :: z2(nel)                         !< Z coordinate of node 2
      real(kind=wp), intent(inout)    :: z3(nel)                         !< Z coordinate of node 3
      real(kind=wp), intent(inout)    :: z4(nel)                         !< Z coordinate of node 4
      real(kind=wp), intent(inout)    :: z5(nel)                         !< Z coordinate of node 5
      real(kind=wp), intent(inout)    :: z6(nel)                         !< Z coordinate of node 6
      
      real(kind=wp), intent(out)   :: px1(nel)                        !< Shape function derivative dN1/dxi
      real(kind=wp), intent(out)   :: px2(nel)                        !< Shape function derivative dN2/dxi
      real(kind=wp), intent(out)   :: px3(nel)                        !< Shape function derivative dN3/dxi
      real(kind=wp), intent(out)   :: px4(nel)                        !< Shape function derivative dN4/dxi
      real(kind=wp), intent(out)   :: px5(nel)                        !< Shape function derivative dN5/dxi
      real(kind=wp), intent(out)   :: px6(nel)                        !< Shape function derivative dN6/dxi
      
      real(kind=wp), intent(out)   :: py1(nel)                        !< Shape function derivative dN1/deta
      real(kind=wp), intent(out)   :: py2(nel)                        !< Shape function derivative dN2/deta
      real(kind=wp), intent(out)   :: py3(nel)                        !< Shape function derivative dN3/deta
      real(kind=wp), intent(out)   :: py4(nel)                        !< Shape function derivative dN4/deta
      real(kind=wp), intent(out)   :: py5(nel)                        !< Shape function derivative dN5/deta
      real(kind=wp), intent(out)   :: py6(nel)                        !< Shape function derivative dN6/deta
      
      real(kind=wp), intent(out)   :: pz1(nel)                        !< Shape function derivative dN1/dzeta
      real(kind=wp), intent(out)   :: pz2(nel)                        !< Shape function derivative dN2/dzeta
      real(kind=wp), intent(out)   :: pz3(nel)                        !< Shape function derivative dN3/dzeta
      real(kind=wp), intent(out)   :: pz4(nel)                        !< Shape function derivative dN4/dzeta
      real(kind=wp), intent(out)   :: pz5(nel)                        !< Shape function derivative dN5/dzeta
      real(kind=wp), intent(out)   :: pz6(nel)                        !< Shape function derivative dN6/dzeta
      
      real(kind=wp), intent(out)   :: px1h(nel)                       !< Hourglass shape function derivative dN1h/dxi
      real(kind=wp), intent(out)   :: px2h(nel)                       !< Hourglass shape function derivative dN2h/dxi
      real(kind=wp), intent(out)   :: px3h(nel)                       !< Hourglass shape function derivative dN3h/dxi
      
      real(kind=wp), intent(out)   :: py1h(nel)                       !< Hourglass shape function derivative dN1h/deta
      real(kind=wp), intent(out)   :: py2h(nel)                       !< Hourglass shape function derivative dN2h/deta
      real(kind=wp), intent(out)   :: py3h(nel)                       !< Hourglass shape function derivative dN3h/deta
      
      real(kind=wp), intent(out)   :: pz1h(nel)                       !< Hourglass shape function derivative dN1h/dzeta
      real(kind=wp), intent(out)   :: pz2h(nel)                       !< Hourglass shape function derivative dN2h/dzeta
      real(kind=wp), intent(out)   :: pz3h(nel)                       !< Hourglass shape function derivative dN3h/dzeta
      
      real(kind=wp), intent(out)   :: jacob4(nel)                     !< Jacobian matrix component J12
      real(kind=wp), intent(out)   :: jacob5(nel)                     !< Jacobian matrix component J22
      real(kind=wp), intent(out)   :: jacob6(nel)                     !< Jacobian matrix component J32
      real(kind=wp), intent(out)   :: jacob7(nel)                     !< Jacobian matrix component J13
      real(kind=wp), intent(out)   :: jacob8(nel)                     !< Jacobian matrix component J23
      real(kind=wp), intent(out)   :: jacob9(nel)                     !< Jacobian matrix component J33
      
      real(kind=wp), intent(out)   :: jaci33(nel)                     !< Inverse Jacobian component (3,3)
      
      real(kind=wp), intent(out)   :: b1x(mvsiz,2)                    !< B-matrix component for strain calculation
      real(kind=wp), intent(out)   :: b1y(mvsiz,2)                    !< B-matrix component for strain calculation
      real(kind=wp), intent(out)   :: b2x(mvsiz,2)                    !< B-matrix component for strain calculation
      real(kind=wp), intent(out)   :: b2y(mvsiz,2)                    !< B-matrix component for strain calculation
      
      real(kind=wp), intent(out)   :: b1xh(mvsiz,2)                   !< Hourglass B-matrix component
      real(kind=wp), intent(out)   :: b1yh(mvsiz,2)                   !< Hourglass B-matrix component
      real(kind=wp), intent(out)   :: b2xh(mvsiz,2)                   !< Hourglass B-matrix component
      real(kind=wp), intent(out)   :: b2yh(mvsiz,2)                   !< Hourglass B-matrix component
      
      real(kind=wp), intent(out)   :: b1122(nel)                      !< Mixed B-matrix component
      real(kind=wp), intent(out)   :: b1221(nel)                      !< Mixed B-matrix component
      real(kind=wp), intent(out)   :: b2212(nel)                      !< Mixed B-matrix component
      real(kind=wp), intent(out)   :: b1121(nel)                      !< Mixed B-matrix component
      
      real(kind=wp), intent(out)   :: b1122h(nel)                     !< Hourglass mixed B-matrix component
      real(kind=wp), intent(out)   :: b1221h(nel)                     !< Hourglass mixed B-matrix component
      real(kind=wp), intent(out)   :: b2212h(nel)                     !< Hourglass mixed B-matrix component
      real(kind=wp), intent(out)   :: b1121h(nel)                     !< Hourglass mixed B-matrix component
      
      real(kind=wp), intent(out)   :: vzl(nel)                        !< Local volume change rate
      real(kind=wp), intent(out)   :: volg(nel)                       !< Global element volume
      real(kind=wp), intent(inout) :: offg(nel)                       !< Global element deactivation flag
      
      real(kind=wp), intent(in)    :: sav(nel,15)                     !< Saved nodal coordinates for negative volume recovery


      integer, intent(inout) :: idel7nok  
      integer, intent(inout) :: ineg_v
      integer, intent(inout)             :: mstop
      real(kind=wp), intent(in) :: volmin
      integer,dimension(102) :: idtmin
      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   LOCAL VARIABLES
      ! ----------------------------------------------------------------------------------------------------------------------
        integer :: i, j, icor, nnega                         !< Loop counters and flags
        integer :: index(mvsiz)                              !< Index array for negative volume elements
        
        real(kind=wp) :: dett(mvsiz)                         !< Inverse determinant
        real(kind=wp) :: jac1(mvsiz), jac2(mvsiz), jac3(mvsiz) !< Jacobian matrix components
        real(kind=wp) :: jac4(mvsiz), jac5(mvsiz), jac6(mvsiz) !< Jacobian matrix components
        real(kind=wp) :: jac7(mvsiz), jac8(mvsiz), jac9(mvsiz) !< Jacobian matrix components
        
        real(kind=wp) :: jaci1, jaci2, jaci3                 !< Jacobian inverse components
        real(kind=wp) :: jaci4, jaci5, jaci6                 !< Jacobian inverse components
        real(kind=wp) :: jaci7, jaci8, jaci9                 !< Jacobian inverse components
        real(kind=wp) :: jaci12, jaci45, jaci78               !< Combined inverse components
        
        real(kind=wp) :: x21(mvsiz), x31(mvsiz), x54(mvsiz), x64(mvsiz) !< Coordinate differences
        real(kind=wp) :: y21(mvsiz), y31(mvsiz), y54(mvsiz), y64(mvsiz) !< Coordinate differences
        real(kind=wp) :: z21(mvsiz), z31(mvsiz), z54(mvsiz), z64(mvsiz) !< Coordinate differences
        real(kind=wp) :: x41(mvsiz), y41(mvsiz), z41(mvsiz)             !< Coordinate differences
        
        real(kind=wp) :: jac_59_68(mvsiz), jac_67_49(mvsiz), jac_48_57(mvsiz) !< Cross products
        real(kind=wp) :: fac                                                   !< Scaling factor

      ! ----------------------------------------------------------------------------------------------------------------------
 
! -----------------------------------------------
  nnega = 0
!
  do i = 1, nel

  !      write(*,*) 'In S6DERI3  I = ', I
  !      write(*,*) 'X1(I) = ', X1(I)
  !      write(*,*) 'X2(I) = ', X2(I)
  !      write(*,*) 'X3(I) = ', X3(I)
  !      write(*,*) 'X4(I) = ', X4(I)
  !      write(*,*) 'X5(I) = ', X5(I)
  !      write(*,*) 'X6(I) = ', X6(I)

  !      write(*,*) 'Y1(I) = ', Y1(I)
  !      write(*,*) 'Y2(I) = ', Y2(I)
  !      write(*,*) 'Y3(I) = ', Y3(I)
  !      write(*,*) 'Y4(I) = ', Y4(I)
  !      write(*,*) 'Y5(I) = ', Y5(I)
  !      write(*,*) 'Y6(I) = ', Y6(I)

  !      write(*,*) 'Z1(I) = ', Z1(I)
  !      write(*,*) 'Z2(I) = ', Z2(I)
  !      write(*,*) 'Z3(I) = ', Z3(I)
  !      write(*,*) 'Z4(I) = ', Z4(I)
  !      write(*,*) 'Z5(I) = ', Z5(I)
  !      write(*,*) 'Z6(I) = ', Z6(I)
   
  x21(i) = x2(i) - x1(i)
  x31(i) = x3(i) - x1(i)
  x41(i) = x4(i) - x1(i)
  x54(i) = x5(i) - x4(i)
  x64(i) = x6(i) - x4(i)

  y21(i) = y2(i) - y1(i)
  y31(i) = y3(i) - y1(i)
  y41(i) = y4(i) - y1(i)
  y54(i) = y5(i) - y4(i)
  y64(i) = y6(i) - y4(i)
  
  z21(i) = z2(i) - z1(i)
  z31(i) = z3(i) - z1(i)
  z41(i) = z4(i) - z1(i)
  z54(i) = z5(i) - z4(i)
  z64(i) = z6(i) - z4(i)
  end do
!c
!c jacobian matrix
  do i=1,nel
!c  -------ri.xi---->ksi--------
   jac1(i)=x21(i)+x54(i)
   jac2(i)=y21(i)+y54(i)
   jac3(i)=z21(i)+z54(i)
  end do
  do i=1,nel
!c  -------si.xi--->eta--------
   jac4(i)=x31(i)+x64(i)
   jac5(i)=y31(i)+y64(i)
   jac6(i)=z31(i)+z64(i)
!c  -------ti.xi----zeta-------
  ! //todo:a verify with qiang ok pas de souci
   jac7(i)=third*(x41(i)+x5(i)-x2(i)+x6(i)-x3(i))
   jac8(i)=third*(y41(i)+y5(i)-y2(i)+y6(i)-y3(i))
   jac9(i)=third*(z41(i)+z5(i)-z2(i)+z6(i)-z3(i))
   !jac7(i)=third*(x5(i)-x2(i)-x6(i)+x3(i)) + x41(i)
   !jac8(i)=third*(y5(i)-y2(i)-y6(i)+y3(i)) + y41(i)
   !jac9(i)=third*(z5(i)-z2(i)-z6(i)+z3(i)) + z41(i)

   !jac7(i)=x41(i)
   !jac8(i)=y41(i)
   !jac9(i)=z41(i) 
   !write(*,*) 'new'
  end do
!c
  do i=1,nel
   jacob4(i)=jac4(i)
   jacob5(i)=jac5(i)
   jacob6(i)=jac6(i)
   jacob7(i)=jac7(i)
   jacob8(i)=jac8(i)
   jacob9(i)=jac9(i)
  enddo 
!c
  do i=1,nel
  jac_59_68(i)=jac5(i)*jac9(i)-jac6(i)*jac8(i)
  jac_67_49(i)=jac6(i)*jac7(i)-jac4(i)*jac9(i)
  jac_48_57(i)=jac4(i)*jac8(i)-jac5(i)*jac7(i)
  end do
!C
!      DO I=1,NEL
!           write(*,*) 'JAC1 = ', JAC1(I)
!           write(*,*) 'JAC2 = ', JAC2(I)
!           write(*,*) 'JAC3 = ', JAC3(I)
!           write(*,*) 'JAC4 = ', JAC4(I)
!           write(*,*) 'JAC5 = ', JAC5(I)
!           write(*,*) 'JAC6 = ', JAC6(I)
!           write(*,*) 'JAC7 = ', JAC7(I)
!           write(*,*) 'JAC8 = ', JAC8(I)
!           write(*,*) 'JAC9 = ', JAC9(I)
!      END DO

      do i = 1, nel
        det(i) = one_over_8 * (jac1(i) * jac_59_68(i) + jac2(i) * jac_67_49(i) + jac3(i) * jac_48_57(i))
      end do

      ! Check for minimum volume conditions and handle negative volumes
      if (idtmin(1) == 1) then
        icor = 0
        do i = 1, nel
          if (off(i) == zero) then
            det(i) = one
          elseif ((det(i) <= volmin) .or. (det(i) <= zero)) then
            icor = 1
          endif
        enddo
        
        if (icor > 0) then
          do i = 1, nel
            if (off(i) /= zero) then
              if (det(i) <= volmin) then
                det(i) = one
                off(i) = zero
!    #include "lockon.inc"
                write(istdo, 2000) ngl(i)
                write(iout, 2000) ngl(i)
!    #include "lockoff.inc"
              elseif (det(i) <= zero) then
                call ancmsg(msgid=166, anmode=aninfo, i1=ngl(i))
                mstop = 1
              endif
            endif
          enddo
        endif
        
      elseif (idtmin(1) == 2) then
        icor = 0
        do i = 1, nel
          if (off(i) == zero) then
            det(i) = one
          elseif ((det(i) <= volmin) .or. (det(i) <= zero)) then
            icor = 1
          endif
        enddo
        
        if (icor > 0) then
          do i = 1, nel
            if ((off(i) /= zero) .and. (det(i) <= volmin .or. det(i) <= zero)) then
              det(i) = one
              off(i) = zero
!    #include "lockon.inc"
              write(istdo, 2000) ngl(i)
              write(iout, 2000) ngl(i)
!    #include "lockoff.inc"
              idel7nok = 1
            endif
          enddo
        endif
        
      elseif (ismstr /= 4) then
        icor = 0
        do i = 1, nel
          if (off(i) == zero) then
            det(i) = one
          elseif ((det(i) <= volmin) .or. (det(i) <= zero)) then
            icor = 1
          endif
        enddo
        
        if (icor > 0) then
          do i = 1, nel
            if (off(i) == zero) then
              det(i) = one
            elseif (offg(i) > one) then
              ! Element already flagged - skip processing
            elseif ((det(i) <= volmin) .or. (det(i) <= zero)) then
              nnega = nnega + 1
              index(nnega) = i
!    #include "lockon.inc"
              write(istdo, 3000) ngl(i)
              write(iout, 3000) ngl(i)
!    #include "lockoff.inc"
            endif
          enddo
          
          
          if (ineg_v == 0) then
            call ancmsg(msgid=280, anmode=aninfo)
            mstop = 1
          endif
        end if ! (icor > 0)
        
      else
        ! Handle case where ismstr == 4
        icor = 0
        do i = 1, nel
          if (off(i) == zero) then
            det(i) = one
          elseif (det(i) <= zero) then
            icor = 1
          endif
        enddo
        
        if (icor > 0) then
          do i = 1, nel
            if (off(i) /= zero) then
              if (det(i) <= zero) then
                call ancmsg(msgid=166, anmode=aninfo, i1=ngl(i))
                mstop = 1
              endif
            endif
          enddo
        endif
      endif

      ! Process elements with negative volumes for coordinate recovery
      if (nnega > 0) then
!    #include "vectorize.inc"
        do j = 1, nnega
!          write(*,*) 'In the loop of NNEGA'
            i = index(j)
            x1(i) = sav(i,1)
            y1(i) = sav(i,2)
            z1(i) = sav(i,3)
            x2(i) = sav(i,4)
            y2(i) = sav(i,5)
            z2(i) = sav(i,6)
            x3(i) = sav(i,7)
            y3(i) = sav(i,8)
            z3(i) = sav(i,9)
            x4(i) = sav(i,10)
            y4(i) = sav(i,11)
            z4(i) = sav(i,12)
            x5(i) = sav(i,13)
            y5(i) = sav(i,14)
            z5(i) = sav(i,15)
            x6(i) = zero
            y6(i) = zero
            z6(i) = zero
      !
             x21(i) = x2(i) - x1(i)
             x31(i) = x3(i) - x1(i)
             x41(i) = x4(i) - x1(i)
             x54(i) = x5(i) - x4(i)
             x64(i) = x6(i) - x4(i)
             y21(i) = y2(i) - y1(i)
             y31(i) = y3(i) - y1(i)
             y41(i) = y4(i) - y1(i)
             y54(i) = y5(i) - y4(i)
             y64(i) = y6(i) - y4(i)
             z21(i) = z2(i) - z1(i)
             z31(i) = z3(i) - z1(i)
             z41(i) = z4(i) - z1(i)
             z54(i) = z5(i) - z4(i)
             z64(i) = z6(i) - z4(i)
      !
             jac1(i) = x21(i) + x54(i)
             jac2(i) = y21(i) + y54(i)
             jac3(i) = z21(i) + z54(i)
      !----
             jac4(i) = x31(i) + x64(i)
             jac5(i) = y31(i) + y64(i)
             jac6(i) = z31(i) + z64(i)
             jac7(i) = third * (x41(i) + x5(i) - x2(i) + x6(i) - x3(i))
             jac8(i) = third * (y41(i) + y5(i) - y2(i) + y6(i) - y3(i))
             jac9(i) = third * (z41(i) + z5(i) - z2(i) + z6(i) - z3(i))
      !
             jacob4(i) = jac4(i)
             jacob5(i) = jac5(i)
             jacob6(i) = jac6(i)
             jacob7(i) = jac7(i)
             jacob8(i) = jac8(i)
             jacob9(i) = jac9(i)
      !
             jac_59_68(i) = jac5(i) * jac9(i) - jac6(i) * jac8(i)
             jac_67_49(i) = jac6(i) * jac7(i) - jac4(i) * jac9(i)
             jac_48_57(i) = jac4(i) * jac8(i) - jac5(i) * jac7(i)
      !
             det(i) = one_over_8 * (jac1(i) * jac_59_68(i) + jac2(i) * jac_67_49(i) + jac3(i) * jac_48_57(i))
             offg(i) = two
           end do
          end if
      !
      ! Jacobian matrix inverse
          do i = 1, nel
          dett(i) = one_over_8 / det(i)
      !
          jaci1 = dett(i) * jac_59_68(i)
          jaci4 = dett(i) * jac_67_49(i)
          jaci7 = dett(i) * jac_48_57(i)
          jaci2 = dett(i) * (-jac2(i) * jac9(i) + jac3(i) * jac8(i))
          jaci5 = dett(i) * ( jac1(i) * jac9(i) - jac3(i) * jac7(i))
          jaci8 = dett(i) * (-jac1(i) * jac8(i) + jac2(i) * jac7(i))
          jaci3 = dett(i) * ( jac2(i) * jac6(i) - jac3(i) * jac5(i))
          jaci6 = dett(i) * (-jac1(i) * jac6(i) + jac3(i) * jac4(i))
          jaci9 = dett(i) * ( jac1(i) * jac5(i) - jac2(i) * jac4(i))


          
          
      !           write(*,*) 'jaci1 = ', jaci1
      !           write(*,*) 'jaci2 = ', jaci2
      !           write(*,*) 'jaci3 = ', jaci3
      !           write(*,*) 'jaci4 = ', jaci4
      !           write(*,*) 'jaci5 = ', jaci5
      !           write(*,*) 'jaci6 = ', jaci6
      !           write(*,*) 'jaci7 = ', jaci7
      !           write(*,*) 'jaci8 = ', jaci8
      !           write(*,*) 'jaci9 = ', jaci9
          

      ! Here is OK //!TO VERIFY
           jaci12 = jaci1 + jaci2
           jaci45 = jaci4 + jaci5
           jaci78 = jaci7 + jaci8

          !jaci12 = jaci1 + jaci2 + jaci3
          !jaci45 = jaci4 + jaci5 + jaci6 
          !jaci78 = jaci7 + jaci8 + jaci9
      !
      ! Symmetry (a b c a b c)->P1-P3, anti-symmetry(-1 -1 -1 1 1 1)->P4
          px1(i) = -jaci12 - third * jaci3
          py1(i) = -jaci45 - third * jaci6
          pz1(i) = -jaci78 - third * jaci9

          px4(i) = -jaci12 + third * jaci3
          py4(i) = -jaci45 + third * jaci6
          pz4(i) = -jaci78 + third * jaci9

          px2(i) = jaci1 - third * jaci3
          py2(i) = jaci4 - third * jaci6
          pz2(i) = jaci7 - third * jaci9

          px5(i) = jaci1 + third * jaci3
          py5(i) = jaci4 + third * jaci6
          pz5(i) = jaci7 + third * jaci9


          px3(i) = jaci2 - third * jaci3
          py3(i) = jaci5 - third * jaci6
          pz3(i) = jaci8 - third * jaci9

          px6(i) = jaci2 + third * jaci3
          py6(i) = jaci5 + third * jaci6
          pz6(i) = jaci8 + third * jaci9        
      !        //!TO VERIFY
           ! px4(i) = third * jaci3
           ! py4(i) = third * jaci6
           ! pz4(i) = third * jaci9
           !px4(i) =  jaci3 - jaci2 - jaci1
           !py4(i) =  jaci6 - jaci4 - jaci5
           !pz4(i) =  jaci9 - jaci8 - jaci7
        
          !px4(i) = -jaci12 + third * jaci3
          !py4(i) = -jaci45 + third * jaci6
          !pz4(i) = -jaci78 + third * jaci9

      !       write(*,*) 'px1', px1(i)
      !       write(*,*) 'py1', py1(i)
      !       write(*,*) 'pz1', pz1(i)

      !        write(*,*) 'px2', px2(i)
      !        write(*,*) 'py2', py2(i)
      !        write(*,*) 'pz2', pz2(i)

      !        write(*,*) 'px3', px3(i)
      !        write(*,*) 'py3', py3(i)
      !        write(*,*) 'pz3', pz3(i)

      !        write(*,*) 'px4', px4(i)
      !        write(*,*) 'py4', py4(i)
      !        write(*,*) 'pz4', pz4(i)

      !        write(*,*) 'px5', px5(i)
      !        write(*,*) 'py5', py5(i)
      !        write(*,*) 'pz5', pz5(i)

      !        write(*,*) 'px6', px6(i)
      !        write(*,*) 'py6', py6(i)
      !        write(*,*) 'pz6', pz6(i)

          jaci33(i) = jaci9 * one_over_12
          end do
      !

          do i = 1, nel
           fac = dett(i) * one_over_12
           b1x(i,1) = 1.0_wp
           b1x(i,2) = 1.0_wp
           b1y(i,1) = 1.0_wp
           b1y(i,2) = 1.0_wp
           b2x(i,1) = 1.0_wp
           b2x(i,2) = 1.0_wp
           b2y(i,1) = 1.0_wp
           b2y(i,2) = 1.0_wp
           fac = fac * 2.0_wp
           b1122(i) = 1.0_wp
           b1221(i) = 1.0_wp
           b2212(i) = 1.0_wp
           b1121(i) = 1.0_wp
      !
           b1xh(i,1) = 1.0_wp
           b1xh(i,2) = 1.0_wp
           b1yh(i,1) = 1.0_wp
           b1yh(i,2) = 1.0_wp
           b2xh(i,1) = 1.0_wp
           b2xh(i,2) = 1.0_wp
           b2yh(i,1) = 1.0_wp
           b2yh(i,2) = 1.0_wp
           fac = fac * two
           b1122h(i) = 1.0_wp
           b1221h(i) = 1.0_wp
           b2212h(i) = 1.0_wp
           b1121h(i) = 1.0_wp

           px1h(i) = 1.0_wp
           py1h(i) = 1.0_wp
           pz1h(i) = 1.0_wp
           px2h(i) = 1.0_wp
           py2h(i) = 1.0_wp
           pz2h(i) = 1.0_wp
           px3h(i) = 1.0_wp
           py3h(i) = 1.0_wp
           pz3h(i) = 1.0_wp
          end do
      ! For shear traitement----------
        !    do i = 1, nel
        !     fac = dett(i) * one_over_12
        !     b1x(i,1) = -fac * jac1(i) * jac2(i)
        !     b1x(i,2) = -fac * jac4(i) * jac5(i)
        !     b1y(i,1) = -fac * jac2(i) * jac2(i)
        !     b1y(i,2) = -fac * jac5(i) * jac5(i)
        !     b2x(i,1) = -fac * jac1(i) * jac1(i)
        !     b2x(i,2) = -fac * jac4(i) * jac4(i)
        !     b2y(i,1) = b1x(i,1)
        !     b2y(i,2) = b1x(i,2)
        !     fac = fac * 2.0_wp
        !     b1122(i) = fac * jac1(i) * jac5(i)
        !     b1221(i) = fac * jac2(i) * jac4(i)
        !     b2212(i) = fac * jac5(i) * jac2(i)
        !     b1121(i) = b2212(i)
      !
        !     b1xh(i,1) = -fac * (x54(i) * y54(i) - x21(i) * y21(i))
        !     b1xh(i,2) = -fac * (x64(i) * y64(i) - x31(i) * y31(i))
        !     b1yh(i,1) = -fac * (y54(i) * y54(i) - y21(i) * y21(i))
        !     b1yh(i,2) = -fac * (y64(i) * y64(i) - y31(i) * y31(i))
        !     b2xh(i,1) = -fac * (x54(i) * x54(i) - x21(i) * x21(i))
        !     b2xh(i,2) = -fac * (x64(i) * x64(i) - x31(i) * x31(i))
        !     b2yh(i,1) = b1xh(i,1)
        !     b2yh(i,2) = b1xh(i,2)
        !     fac = fac * two
        !     b1122h(i) = fac * (x54(i) * y64(i) - x21(i) * y31(i))
        !     b1221h(i) = fac * (x64(i) * y54(i) - x31(i) * y21(i))
        !     b2212h(i) = fac * (y54(i) * y64(i) - y21(i) * y31(i))
        !     b1121h(i) = b2212h(i)
        !    end do
      ! Non constant part
        !    do i = 1, nel
        !     jac1(i) = -x21(i) + x54(i)
        !     jac2(i) = -y21(i) + y54(i)
        !     jac3(i) = -z21(i) + z54(i)
        !     jac4(i) = -x31(i) + x64(i)
        !     jac5(i) = -y31(i) + y64(i)
        !     jac6(i) = -z31(i) + z64(i)
        !    end do
      !
        !    do i = 1, nel
        !     jac_59_68(i) = jac5(i) * jac9(i) - jac6(i) * jac8(i)
        !     jac_67_49(i) = jac6(i) * jac7(i) - jac4(i) * jac9(i)
        !     jac_48_57(i) = jac4(i) * jac8(i) - jac5(i) * jac7(i)
        !    end do
      !
        !    do i = 1, nel
        !     jaci1 = dett(i) * jac_59_68(i)
        !     jaci4 = dett(i) * jac_67_49(i)
        !     jaci7 = dett(i) * jac_48_57(i)
        !     jaci2 = dett(i) * (-jac2(i) * jac9(i) + jac3(i) * jac8(i))
        !     jaci5 = dett(i) * ( jac1(i) * jac9(i) - jac3(i) * jac7(i))
        !     jaci8 = dett(i) * (-jac1(i) * jac8(i) + jac2(i) * jac7(i))
      !
        !     jaci12 = jaci1 + jaci2
        !     jaci45 = jaci4 + jaci5
        !     jaci78 = jaci7 + jaci8
      !
      ! Symmetry(a b c a b c)->P1-P3
        !     px1h(i) = -jaci12
        !     py1h(i) = -jaci45
        !     pz1h(i) = -jaci78
        !     px2h(i) = jaci1
        !     py2h(i) = jaci4
        !     pz2h(i) = jaci7
        !     px3h(i) = jaci2
        !     py3h(i) = jaci5
        !     pz3h(i) = jaci8
        !    end do
      !                                                                     12
           do i = 1, nel
          vzl(i) = fourth * (jacob9(i) * ( &
               x54(i) * y64(i) - x21(i) * y31(i) - x64(i) * y54(i) + x31(i) * y21(i)) &
                  - jacob8(i) * ( &
               x54(i) * z64(i) + x31(i) * z21(i) - x21(i) * z31(i) - x64(i) * z54(i)) &
                  + jacob7(i) * ( &
               y54(i) * z64(i) + y31(i) * z21(i) - y21(i) * z31(i) - y64(i) * z54(i)) &
                )
          !vzl(i) = 0. 
          volg(i) = det(i)
           end do
          return
      !
       1000 format(/' zero or negative volume : 3d-element nb',i10/)
       2000 format(/' zero or negative volume : delete 3d-element nb',i10/)
      3000 format(/' zero or negative volume : 3d-element nb:', i10, /, &
            'solid-shell element is switched to small strain option'/)
    end subroutine s6deri3_2
  end module s6deri3_2_mod