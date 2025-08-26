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
      !||    s6cdefc3   ../engine/source/elements/thickshell/solide6c/s6cdefo3.F
      !||--- called by ------------------------------------------------------
      !||    s6cforc3   ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||====================================================================

      module s6cdefc3_2_mod
      contains

      ! ======================================================================================================================
      !                                                   PROCEDURES
      ! ======================================================================================================================

      !! \brief Compute strain rates and hourglass control for 6-node solid elements
      !! \details Calculates strain rate tensor components (dxx, dyy, dzz, dxy, dxz, dyz)
      !!          and hourglass control strain rates for 6-node thick shell/solid elements.
      !!          Includes volumetric strain rate and spin rate calculations.
      subroutine s6cdefc3_2( &
         px1, px2, px3, px4, &
         py1, py2, py3, py4, &
         pz1, pz2, pz3, pz4, &
         vx1, vx2, vx3, vx4, &
         vx5, vx6, vy1, vy2, &
         vy3, vy4, vy5, vy6, &
         vz1, vz2, vz3, vz4, &
         vz5, vz6, dxx, dxy, &
         dxz, dyx, dyy, dyz, &
         dzx, dzy, dzz, &
         wxx, wyy, &
         wzz, dhxx, dhxy, dhxz, &
         dhyx, dhyy, dhyz, dhzx, &
         dhzy, dhzz, &
         px1h, px2h, px3h, &
         py1h, py2h, py3h, pz1h, &
         pz2h, pz3h, ji33, b1x, &
         b1y, b2y, b2x, b1122, &
         b1221, b2212, b1121, b1xh, &
         b1yh, b2xh, b2yh, b1122h, &
         b1221h, b2212h, b1121h, ddhv, &
         nu, nel, &
         px5, py5, pz5, &
         px6, py6, pz6)

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   MODULES
      ! ----------------------------------------------------------------------------------------------------------------------
         use PRECISION_MOD, only : WP
     
      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   IMPLICIT NONE
      ! ----------------------------------------------------------------------------------------------------------------------
         implicit none

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   ARGUMENTS
      ! ----------------------------------------------------------------------------------------------------------------------
         integer, intent(in) :: nel                                          !< Number of elements

         real(kind=WP), intent(in) :: vx1(nel)                              !< X-velocity at node 1
         real(kind=WP), intent(in) :: vx2(nel)                              !< X-velocity at node 2
         real(kind=WP), intent(in) :: vx3(nel)                              !< X-velocity at node 3
         real(kind=WP), intent(in) :: vx4(nel)                              !< X-velocity at node 4
         real(kind=WP), intent(in) :: vx5(nel)                              !< X-velocity at node 5
         real(kind=WP), intent(in) :: vx6(nel)                              !< X-velocity at node 6
         real(kind=WP), intent(in) :: vy1(nel)                              !< Y-velocity at node 1
         real(kind=WP), intent(in) :: vy2(nel)                              !< Y-velocity at node 2
         real(kind=WP), intent(in) :: vy3(nel)                              !< Y-velocity at node 3
         real(kind=WP), intent(in) :: vy4(nel)                              !< Y-velocity at node 4
         real(kind=WP), intent(in) :: vy5(nel)                              !< Y-velocity at node 5
         real(kind=WP), intent(in) :: vy6(nel)                              !< Y-velocity at node 6
         real(kind=WP), intent(in) :: vz1(nel)                              !< Z-velocity at node 1
         real(kind=WP), intent(in) :: vz2(nel)                              !< Z-velocity at node 2
         real(kind=WP), intent(in) :: vz3(nel)                              !< Z-velocity at node 3
         real(kind=WP), intent(in) :: vz4(nel)                              !< Z-velocity at node 4
         real(kind=WP), intent(in) :: vz5(nel)                              !< Z-velocity at node 5
         real(kind=WP), intent(in) :: vz6(nel)                              !< Z-velocity at node 6

         real(kind=WP), intent(in) :: px1(nel)                              !< Shape function derivative X1
         real(kind=WP), intent(in) :: px2(nel)                              !< Shape function derivative X2
         real(kind=WP), intent(in) :: px3(nel)                              !< Shape function derivative X3
         real(kind=WP), intent(in) :: px4(nel)                              !< Shape function derivative X4
         real(kind=WP), intent(in) :: px5(nel)                              !< Shape function derivative X5
         real(kind=WP), intent(in) :: px6(nel)                              !< Shape function derivative X6
         real(kind=WP), intent(in) :: py1(nel)                              !< Shape function derivative Y1
         real(kind=WP), intent(in) :: py2(nel)                              !< Shape function derivative Y2
         real(kind=WP), intent(in) :: py3(nel)                              !< Shape function derivative Y3
         real(kind=WP), intent(in) :: py4(nel)                              !< Shape function derivative Y4
         real(kind=WP), intent(in) :: py5(nel)                              !< Shape function derivative Y5
         real(kind=WP), intent(in) :: py6(nel)                              !< Shape function derivative Y6
         real(kind=WP), intent(in) :: pz1(nel)                              !< Shape function derivative Z1
         real(kind=WP), intent(in) :: pz2(nel)                              !< Shape function derivative Z2
         real(kind=WP), intent(in) :: pz3(nel)                              !< Shape function derivative Z3
         real(kind=WP), intent(in) :: pz4(nel)                              !< Shape function derivative Z4
         real(kind=WP), intent(in) :: pz5(nel)                              !< Shape function derivative Z5
         real(kind=WP), intent(in) :: pz6(nel)                              !< Shape function derivative Z6

         real(kind=WP), intent(in) :: px1h(nel)                             !< Hourglass shape function derivative X1
         real(kind=WP), intent(in) :: px2h(nel)                             !< Hourglass shape function derivative X2
         real(kind=WP), intent(in) :: px3h(nel)                             !< Hourglass shape function derivative X3
         real(kind=WP), intent(in) :: py1h(nel)                             !< Hourglass shape function derivative Y1
         real(kind=WP), intent(in) :: py2h(nel)                             !< Hourglass shape function derivative Y2
         real(kind=WP), intent(in) :: py3h(nel)                             !< Hourglass shape function derivative Y3
         real(kind=WP), intent(in) :: pz1h(nel)                             !< Hourglass shape function derivative Z1
         real(kind=WP), intent(in) :: pz2h(nel)                             !< Hourglass shape function derivative Z2
         real(kind=WP), intent(in) :: pz3h(nel)                             !< Hourglass shape function derivative Z3

         real(kind=WP), intent(in) :: ji33(nel)                             !< Jacobian inverse component 33
         real(kind=WP), intent(in) :: b1x(nel,2)                            !< B matrix X component 1
         real(kind=WP), intent(in) :: b1y(nel,2)                            !< B matrix Y component 1
         real(kind=WP), intent(in) :: b2x(nel,2)                            !< B matrix X component 2
         real(kind=WP), intent(in) :: b2y(nel,2)                            !< B matrix Y component 2
         real(kind=WP), intent(in) :: b1xh(nel,2)                           !< Hourglass B matrix X component 1
         real(kind=WP), intent(in) :: b1yh(nel,2)                           !< Hourglass B matrix Y component 1
         real(kind=WP), intent(in) :: b2xh(nel,2)                           !< Hourglass B matrix X component 2
         real(kind=WP), intent(in) :: b2yh(nel,2)                           !< Hourglass B matrix Y component 2

         real(kind=WP), intent(in) :: b1122(nel)                            !< B matrix component 1122
         real(kind=WP), intent(in) :: b1221(nel)                            !< B matrix component 1221
         real(kind=WP), intent(in) :: b2212(nel)                            !< B matrix component 2212
         real(kind=WP), intent(in) :: b1121(nel)                            !< B matrix component 1121
         real(kind=WP), intent(in) :: b1122h(nel)                           !< Hourglass B matrix component 1122
         real(kind=WP), intent(in) :: b1221h(nel)                           !< Hourglass B matrix component 1221
         real(kind=WP), intent(in) :: b2212h(nel)                           !< Hourglass B matrix component 2212
         real(kind=WP), intent(in) :: b1121h(nel)                           !< Hourglass B matrix component 1121

         real(kind=WP), intent(in) :: nu(nel)                               !< Poisson's ratio

         real(kind=WP), intent(out) :: dxx(nel)                             !< Strain rate XX component
         real(kind=WP), intent(out) :: dxy(nel)                             !< Strain rate XY component
         real(kind=WP), intent(out) :: dxz(nel)                             !< Strain rate XZ component
         real(kind=WP), intent(out) :: dyx(nel)                             !< Strain rate YX component
         real(kind=WP), intent(out) :: dyy(nel)                             !< Strain rate YY component
         real(kind=WP), intent(out) :: dyz(nel)                             !< Strain rate YZ component
         real(kind=WP), intent(out) :: dzx(nel)                             !< Strain rate ZX component
         real(kind=WP), intent(out) :: dzy(nel)                             !< Strain rate ZY component
         real(kind=WP), intent(out) :: dzz(nel)                             !< Strain rate ZZ component

         real(kind=WP), intent(out) :: wxx(nel)                             !< Spin rate XX component
         real(kind=WP), intent(out) :: wyy(nel)                             !< Spin rate YY component
         real(kind=WP), intent(out) :: wzz(nel)                             !< Spin rate ZZ component

         real(kind=WP), intent(out) :: dhxx(nel)                            !< Hourglass strain rate XX component
         real(kind=WP), intent(out) :: dhxy(nel)                            !< Hourglass strain rate XY component
         real(kind=WP), intent(out) :: dhxz(nel)                            !< Hourglass strain rate XZ component
         real(kind=WP), intent(out) :: dhyx(nel)                            !< Hourglass strain rate YX component
         real(kind=WP), intent(out) :: dhyy(nel)                            !< Hourglass strain rate YY component
         real(kind=WP), intent(out) :: dhyz(nel)                            !< Hourglass strain rate YZ component
         real(kind=WP), intent(out) :: dhzx(nel)                            !< Hourglass strain rate ZX component
         real(kind=WP), intent(out) :: dhzy(nel)                            !< Hourglass strain rate ZY component
         real(kind=WP), intent(out) :: dhzz(nel)                            !< Hourglass strain rate ZZ component

         real(kind=WP), intent(out) :: ddhv(nel)                            !< Volumetric strain rate

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   LOCAL VARIABLES
      ! ----------------------------------------------------------------------------------------------------------------------
         integer :: i                                                        ! Loop counter
         real(kind=WP) :: vx14(nel), vy14(nel), vz14(nel)
         real(kind=WP) :: vx25(nel), vy25(nel), vz25(nel)
         real(kind=WP) :: vx36(nel), vy36(nel), vz36(nel)
         real(kind=WP) :: vx14n(nel), vy14n(nel), vz14n(nel)
         real(kind=WP) :: vx25n(nel), vy25n(nel), vz25n(nel)
         real(kind=WP) :: vx36n(nel), vy36n(nel), vz36n(nel)
         real(kind=WP) :: vx3614n(nel), vy3614n(nel), vx2514n(nel), vy2514n(nel)
         real(kind=WP) :: vxhi(nel), vyhi(nel), vzhi(nel)
         real(kind=WP) :: nu1, pxsvx, pysvy, pzsvz, pxavx, pyavy, pzavz, termad

      ! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------

   ! Compute velocity combinations for finite element calculation
   do i = 1, nel
      vx14(i) = vx1(i) + vx4(i)
      vx25(i) = vx2(i) + vx5(i)
      vx36(i) = vx3(i) + vx6(i)
      vxhi(i) = vx4(i) + vx5(i) + vx6(i) - vx1(i) - vx2(i) - vx3(i)
      vy14(i) = vy1(i) + vy4(i)
      vy25(i) = vy2(i) + vy5(i)
      vy36(i) = vy3(i) + vy6(i)
      vyhi(i) = vy4(i) + vy5(i) + vy6(i) - vy1(i) - vy2(i) - vy3(i)
      vz14(i) = vz1(i) + vz4(i)
      vz25(i) = vz2(i) + vz5(i)
      vz36(i) = vz3(i) + vz6(i)
      vzhi(i) = vz4(i) + vz5(i) + vz6(i) - vz1(i) - vz2(i) - vz3(i)
   end do
   
   do i = 1, nel
      vx14n(i) = -vx1(i) + vx4(i)
      vx25n(i) = -vx2(i) + vx5(i)
      vx36n(i) = -vx3(i) + vx6(i)
      vy14n(i) = -vy1(i) + vy4(i)
      vy25n(i) = -vy2(i) + vy5(i)
      vy36n(i) = -vy3(i) + vy6(i)
      vz14n(i) = -vz1(i) + vz4(i)
      vz25n(i) = -vz2(i) + vz5(i)
      vz36n(i) = -vz3(i) + vz6(i)

      vx3614n(i) = vx36n(i) - vx14n(i)
      vy3614n(i) = vy36n(i) - vy14n(i)
      vx2514n(i) = vx25n(i) - vx14n(i)
      vy2514n(i) = vy25n(i) - vy14n(i)
   end do

   ! Constant part - strain rate calculation
   do i = 1, nel
      dxx(i) = px1(i)*vx14(i) + px2(i)*vx25(i) + &
                   px3(i)*vx36(i) + px4(i)*vxhi(i)
      dyy(i) = py1(i)*vy14(i) + py2(i)*vy25(i) + &
                   py3(i)*vy36(i) + py4(i)*vyhi(i)
      dzz(i) = pz1(i)*vz14(i) + pz2(i)*vz25(i) + &
                   pz3(i)*vz36(i) + pz4(i)*vzhi(i)
      dxy(i) = py1(i)*vx14(i) + py2(i)*vx25(i) + &
                   py3(i)*vx36(i) + py4(i)*vxhi(i)
      dyx(i) = px1(i)*vy14(i) + px2(i)*vy25(i) + &
                   px3(i)*vy36(i) + px4(i)*vyhi(i)
      
      ! Shear treatment
      dxz(i) = ji33(i)*2.0_WP*vxhi(i) &
                   - b1122(i)*vx36n(i) + b1221(i)*vx25n(i) &
                   + b2212(i)*(vy25n(i) - vy36n(i))
      dxz(i) = dxz(i) + ji33(i)*(vxhi(i) - vx14n(i)) &
                   - b1x(i,1)*vx3614n(i) + b1x(i,2)*vx2514n(i) &
                   - b1y(i,1)*vy3614n(i) + b1y(i,2)*vy2514n(i)
      dyz(i) = ji33(i)*2.0_WP*vyhi(i) &
                   - b1122(i)*vy25n(i) + b1221(i)*vy36n(i) &
                   + b1121(i)*(vx36n(i) - vx25n(i))
      dyz(i) = dyz(i) + ji33(i)*(vyhi(i) - vy14n(i)) &
                   + b2x(i,1)*vx3614n(i) - b2x(i,2)*vx2514n(i) &
                   + b2y(i,1)*vy3614n(i) - b2y(i,2)*vy2514n(i)

      dzx(i) = 0.5_WP*(px1(i)*vz14(i) + px2(i)*vz25(i) + &
                   px3(i)*vz36(i))
      dzy(i) = 0.5_WP*(py1(i)*vz14(i) + py2(i)*vz25(i) + &
                   py3(i)*vz36(i))
   end do

   ! Initialize spin rates
   do i = 1, nel
      wxx(i) = 0.0_WP
      wyy(i) = 0.0_WP
      wzz(i) = 0.0_WP
   end do

   ! Non-constant part - hourglass control
   do i = 1, nel
      nu1 = nu(i) / (1.0_WP - nu(i))
      pxavx = px1(i)*vx14n(i) + px2(i)*vx25n(i) + px3(i)*vx36n(i)
      pyavy = py1(i)*vy14n(i) + py2(i)*vy25n(i) + py3(i)*vy36n(i)
      pzavz = pz1(i)*vz14n(i) + pz2(i)*vz25n(i) + pz3(i)*vz36n(i)
      pxsvx = px1h(i)*vx14(i) + px2h(i)*vx25(i) + px3h(i)*vx36(i)
      pysvy = py1h(i)*vy14(i) + py2h(i)*vy25(i) + py3h(i)*vz36(i)
      pzsvz = pz1h(i)*vz14(i) + pz2h(i)*vz25(i) + pz3h(i)*vz36(i)
      termad = -nu(i)*pyavy - nu1*pzavz
      ddhv(i) = termad
      dhxx(i) = pxsvx + pxavx + termad
      termad = -nu(i)*pxsvx - nu1*pzsvz
      ddhv(i) = ddhv(i) + termad
      dhyy(i) = pysvy + pyavy + termad
      termad = -nu(i)*(pxsvx + pyavy) - nu1*(pxavx + pysvy)
      ddhv(i) = ddhv(i) + termad
      dhzz(i) = pzsvz + pzavz + termad
      dhxy(i) = py1(i)*vx14n(i) + py2(i)*vx25n(i) + &
                     py3(i)*vx36n(i) + &
                     py1h(i)*vx14(i) + py2h(i)*vx25(i) + &
                     py3h(i)*vx36(i)
      dhyx(i) = px1(i)*vy14n(i) + px2(i)*vy25n(i) + &
                     px3(i)*vy36n(i) + &
                     px1h(i)*vy14(i) + px2h(i)*vy25(i) + &
                     px3h(i)*vy36(i)
      
      ! Shear treatment for hourglass
      dhxz(i) = -b1122h(i)*vx36n(i) + b1221h(i)*vx25n(i) &
                     + b2212h(i)*(vy25n(i) - vy36n(i))
      dhxz(i) = dhxz(i) &
                     - b1xh(i,1)*vx3614n(i) + b1xh(i,2)*vx2514n(i) &
                     - b1yh(i,1)*vy3614n(i) + b1yh(i,2)*vy2514n(i)
      dhyz(i) = -b1122h(i)*vy25n(i) + b1221h(i)*vy36n(i) &
                     + b1121h(i)*(vx36n(i) - vx25n(i))
      dhyz(i) = dhyz(i) &
                     + b2xh(i,1)*vx3614n(i) - b2xh(i,2)*vx2514n(i) &
                     + b2yh(i,1)*vy3614n(i) - b2yh(i,2)*vy2514n(i)
      dhzx(i) = 0.5_WP*(px1h(i)*vz14(i) + px2h(i)*vz25(i) + &
                     px3h(i)*vz36(i))
      dhzy(i) = 0.5_WP*(py1h(i)*vz14(i) + py2h(i)*vz25(i) + &
                     py3h(i)*vz36(i))
   end do

   ! Alternative strain rate calculation (direct node approach)
   do i = 1, nel
      dxx(i) = px1(i)*vx1(i) + px2(i)*vx2(i) + px5(i)*vx5(i) + &
                   px3(i)*vx3(i) + px6(i)*vx6(i) + px4(i)*vx4(i)

      dyy(i) = py1(i)*vy1(i) + py2(i)*vy2(i) + py5(i)*vy5(i) + &
                   py3(i)*vy3(i) + py6(i)*vy6(i) + py4(i)*vy4(i)

      dzz(i) = pz1(i)*vz1(i) + pz2(i)*vz2(i) + pz5(i)*vz5(i) + &
                   pz3(i)*vz3(i) + pz6(i)*vz6(i) + pz4(i)*vz4(i)

      dxy(i) = py1(i)*vx1(i) + py2(i)*vx2(i) + py5(i)*vx5(i) + &
                   py3(i)*vx3(i) + py6(i)*vx6(i) + py4(i)*vx4(i)

      dyx(i) = px1(i)*vy1(i) + px2(i)*vy2(i) + px5(i)*vy5(i) + &
                   px3(i)*vy3(i) + px6(i)*vy6(i) + px4(i)*vy4(i)

      dxz(i) = pz1(i)*vx1(i) + pz2(i)*vx2(i) + pz5(i)*vx5(i) + &
                   pz3(i)*vx3(i) + pz6(i)*vx6(i) + pz4(i)*vx4(i)

      dzx(i) = px1(i)*vz1(i) + px2(i)*vz2(i) + px5(i)*vz5(i) + &
                   px3(i)*vz3(i) + px6(i)*vz6(i) + px4(i)*vz4(i)

      dzy(i) = py1(i)*vz1(i) + py2(i)*vz2(i) + py5(i)*vz5(i) + &
                   py3(i)*vz3(i) + py6(i)*vz6(i) + py4(i)*vz4(i)

      dyz(i) = pz1(i)*vy1(i) + pz2(i)*vy2(i) + pz5(i)*vy5(i) + &
                   pz3(i)*vy3(i) + pz6(i)*vy6(i) + pz4(i)*vy4(i)
   end do
!C
      RETURN

      end subroutine s6cdefc3_2
      end module s6cdefc3_2_mod