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
      !||    s6chour3   ../engine/source/elements/thickshell/solide6c/s6chourg3.F
      !||--- called by ------------------------------------------------------
      !||    s6cforc3   ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||====================================================================
!CALL S6CHOUR3(GBUF%RHO,VOLG,CXX,
!.     X1, X2, X3, X3, X4, X5, X6, X6,
!.     Y1, Y2, Y3, Y3, Y4, Y5, Y6, Y6,
!.     Z1, Z2, Z3, Z3, Z4, Z5, Z6, Z6,
!.     VZ1, VZ2, VZ3, VZ3, VZ4, VZ5, VZ6, VZ6,
!.     F31,F32,F33,F34,F35,F36,
!.     NU,GBUF%HOURG,OFF,GBUF%VOL,GBUF%EINT,NEL)

!          CALL S6CHOUR3_1(
!     .     PM, NPROPM, GBUF%RHO,VOLG,CXX,
!     .     X1, X2, X3, X1, X4, X5, X6, X4,
!     .     Y1, Y2, Y3, Y1, Y4, Y5, Y6, Y4,
!     .     Z1, Z2, Z3, Z1, Z4, Z5, Z6, Z4,
!     .     VX1, VX2, VX3, VX1, VX4, VX5, VX6, VX4,
!     .     VY1, VY2, VY3, VY1, VY4, VY5, VY6, VY4,
!     .     VZ1, VZ2, VZ3, VZ1, VZ4, VZ5, VZ6, VZ4,
!     .     F11,F12,F13,F14,F15,F16,
!     .     F21,F22,F23,F24,F25,F26,
!     .     F31,F32,F33,F34,F35,F36,     
!     .     NU,GBUF%HOURG,OFF,GBUF%VOL,GBUF%EINT,NEL,MAT)
      module s6hour3_2_mod
      contains

      !! \brief Compute hourglass forces for 6-node solid elements
      !! \details This subroutine calculates hourglass control forces for 6-node solid elements
      !!          to prevent spurious zero-energy modes. It computes the hourglass forces and
      !!          updates the nodal forces and internal energy.
      subroutine s6hour3_2(                &
            pm, npropm, rho, vol, ssp,      &
            x1, x2, x3, x4, x5, x6, x7, x8, &
            y1, y2, y3, y4, y5, y6, y7, y8, &
            z1, z2, z3, z4, z5, z6, z7, z8, &
            vx1, vx2, vx3, vx4, vx5, vx6, vx7, vx8, &
            vy1, vy2, vy3, vy4, vy5, vy6, vy7, vy8, &
            vz1, vz2, vz3, vz4, vz5, vz6, vz7, vz8, &
            f11, f12, f13, f15, f16, f17, &
            f21, f22, f23, f25, f26, f27, &
            f31, f32, f33, f35, f36, f37, &
            nu, fhour, off, vol0, eint, nel, &
            mat, npropg, geo, pid,          &
            dt1)


      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   MODULES
      ! ----------------------------------------------------------------------------------------------------------------------
!         use DEBUG_MOD
         use PRECISION_MOD, only : WP
         use CONSTANT_MOD, only : ZERO, ONE, TWO, THIRD, FOURTH, HALF, ONE_OVER_8, &
                                              ONE_OVER_64, TWO_THIRD, EM20, ZEP00666666667

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   IMPLICIT NONE
      ! ----------------------------------------------------------------------------------------------------------------------
         implicit none

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   INCLUDED FILES
      ! ----------------------------------------------------------------------------------------------------------------------
!      #include "com08_c.inc"
!      #include "mvsiz_p.inc"

!A VERIFIER CHAQUE PARAMETER
!MAT PM, NPROPM,is verified
!C-----------------------------------------------
!C   M O D U L E S
!C-----------------------------------------------
!C-----------------------------------------------
!C   I M P L I C I T   T Y P E S
!C-----------------------------------------------
!       USE DEBUG_MOD
!#include      "implicit_f.inc"
!C-----------------------------------------------
!C   G L O B A L   P A R A M E T E R S
!C-----------------------------------------------
!#include      "mvsiz_p.inc"
!C-----------------------------------------------
!C   C O M M O N   B L O C K S
!C-----------------------------------------------
!#include      "com08_c.inc"
!C-----------------------------------------------
!C   D U M M Y   A R G U M E N T S
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
         real(kind=WP),              intent(in) :: dt1 !< time step
   integer                                   , intent(in)    :: nel         !< Number of elements
   integer                                   , intent(in)    :: npropm      !< Number of material property columns
   integer                                   , intent(in)    :: npropg      !< Number of property group columns
   integer, dimension(nel)                   , intent(in)    :: mat         !< Material identifiers
   integer, dimension(nel)                   , intent(in)    :: pid         !< Property identifiers
   real(kind=WP), dimension(npropm,*)        , intent(in)    :: pm          !< Material properties
   real(kind=WP), dimension(npropg,*)        , intent(in)    :: geo         !< Geometry properties
   real(kind=WP), dimension(nel)             , intent(in)    :: rho         !< Density
   real(kind=WP), dimension(nel)             , intent(in)    :: vol         !< Current volume
   real(kind=WP), dimension(nel)             , intent(in)    :: ssp         !< Sound speed
   real(kind=WP), dimension(nel)             , intent(in)    :: x1          !< X-coordinate of node 1
   real(kind=WP), dimension(nel)             , intent(in)    :: x2          !< X-coordinate of node 2
   real(kind=WP), dimension(nel)             , intent(in)    :: x3          !< X-coordinate of node 3
   real(kind=WP), dimension(nel)             , intent(in)    :: x4          !< X-coordinate of node 4
   real(kind=WP), dimension(nel)             , intent(in)    :: x5          !< X-coordinate of node 5
   real(kind=WP), dimension(nel)             , intent(in)    :: x6          !< X-coordinate of node 6
   real(kind=WP), dimension(nel)             , intent(in)    :: x7          !< X-coordinate of node 7
   real(kind=WP), dimension(nel)             , intent(in)    :: x8          !< X-coordinate of node 8
   real(kind=WP), dimension(nel)             , intent(in)    :: y1          !< Y-coordinate of node 1
   real(kind=WP), dimension(nel)             , intent(in)    :: y2          !< Y-coordinate of node 2
   real(kind=WP), dimension(nel)             , intent(in)    :: y3          !< Y-coordinate of node 3
   real(kind=WP), dimension(nel)             , intent(in)    :: y4          !< Y-coordinate of node 4
   real(kind=WP), dimension(nel)             , intent(in)    :: y5          !< Y-coordinate of node 5
   real(kind=WP), dimension(nel)             , intent(in)    :: y6          !< Y-coordinate of node 6
   real(kind=WP), dimension(nel)             , intent(in)    :: y7          !< Y-coordinate of node 7
   real(kind=WP), dimension(nel)             , intent(in)    :: y8          !< Y-coordinate of node 8
   real(kind=WP), dimension(nel)             , intent(in)    :: z1          !< Z-coordinate of node 1
   real(kind=WP), dimension(nel)             , intent(in)    :: z2          !< Z-coordinate of node 2
   real(kind=WP), dimension(nel)             , intent(in)    :: z3          !< Z-coordinate of node 3
   real(kind=WP), dimension(nel)             , intent(in)    :: z4          !< Z-coordinate of node 4
   real(kind=WP), dimension(nel)             , intent(in)    :: z5          !< Z-coordinate of node 5
   real(kind=WP), dimension(nel)             , intent(in)    :: z6          !< Z-coordinate of node 6
   real(kind=WP), dimension(nel)             , intent(in)    :: z7          !< Z-coordinate of node 7
   real(kind=WP), dimension(nel)             , intent(in)    :: z8          !< Z-coordinate of node 8
   real(kind=WP), dimension(nel)             , intent(in)    :: vx1         !< X-velocity of node 1
   real(kind=WP), dimension(nel)             , intent(in)    :: vx2         !< X-velocity of node 2
   real(kind=WP), dimension(nel)             , intent(in)    :: vx3         !< X-velocity of node 3
   real(kind=WP), dimension(nel)             , intent(in)    :: vx4         !< X-velocity of node 4
   real(kind=WP), dimension(nel)             , intent(in)    :: vx5         !< X-velocity of node 5
   real(kind=WP), dimension(nel)             , intent(in)    :: vx6         !< X-velocity of node 6
   real(kind=WP), dimension(nel)             , intent(in)    :: vx7         !< X-velocity of node 7
   real(kind=WP), dimension(nel)             , intent(in)    :: vx8         !< X-velocity of node 8
   real(kind=WP), dimension(nel)             , intent(in)    :: vy1         !< Y-velocity of node 1
   real(kind=WP), dimension(nel)             , intent(in)    :: vy2         !< Y-velocity of node 2
   real(kind=WP), dimension(nel)             , intent(in)    :: vy3         !< Y-velocity of node 3
   real(kind=WP), dimension(nel)             , intent(in)    :: vy4         !< Y-velocity of node 4
   real(kind=WP), dimension(nel)             , intent(in)    :: vy5         !< Y-velocity of node 5
   real(kind=WP), dimension(nel)             , intent(in)    :: vy6         !< Y-velocity of node 6
   real(kind=WP), dimension(nel)             , intent(in)    :: vy7         !< Y-velocity of node 7
   real(kind=WP), dimension(nel)             , intent(in)    :: vy8         !< Y-velocity of node 8
   real(kind=WP), dimension(nel)             , intent(in)    :: vz1         !< Z-velocity of node 1
   real(kind=WP), dimension(nel)             , intent(in)    :: vz2         !< Z-velocity of node 2
   real(kind=WP), dimension(nel)             , intent(in)    :: vz3         !< Z-velocity of node 3
   real(kind=WP), dimension(nel)             , intent(in)    :: vz4         !< Z-velocity of node 4
   real(kind=WP), dimension(nel)             , intent(in)    :: vz5         !< Z-velocity of node 5
   real(kind=WP), dimension(nel)             , intent(in)    :: vz6         !< Z-velocity of node 6
   real(kind=WP), dimension(nel)             , intent(in)    :: vz7         !< Z-velocity of node 7
   real(kind=WP), dimension(nel)             , intent(in)    :: vz8         !< Z-velocity of node 8
   real(kind=WP), dimension(nel)             , intent(in)    :: nu          !< Poisson's ratio
   real(kind=WP), dimension(nel)             , intent(in)    :: off         !< Element activation flag
   real(kind=WP), dimension(nel)             , intent(in)    :: vol0        !< Initial volume
   real(kind=WP), dimension(nel)             , intent(inout) :: f11         !< X-force at node 1
   real(kind=WP), dimension(nel)             , intent(inout) :: f12         !< X-force at node 2
   real(kind=WP), dimension(nel)             , intent(inout) :: f13         !< X-force at node 3
   real(kind=WP), dimension(nel)             , intent(inout) :: f15         !< X-force at node 5
   real(kind=WP), dimension(nel)             , intent(inout) :: f16         !< X-force at node 6
   real(kind=WP), dimension(nel)             , intent(inout) :: f17         !< X-force at node 7
   real(kind=WP), dimension(nel)             , intent(inout) :: f21         !< Y-force at node 1
   real(kind=WP), dimension(nel)             , intent(inout) :: f22         !< Y-force at node 2
   real(kind=WP), dimension(nel)             , intent(inout) :: f23         !< Y-force at node 3
   real(kind=WP), dimension(nel)             , intent(inout) :: f25         !< Y-force at node 5
   real(kind=WP), dimension(nel)             , intent(inout) :: f26         !< Y-force at node 6
   real(kind=WP), dimension(nel)             , intent(inout) :: f27         !< Y-force at node 7
   real(kind=WP), dimension(nel)             , intent(inout) :: f31         !< Z-force at node 1
   real(kind=WP), dimension(nel)             , intent(inout) :: f32         !< Z-force at node 2
   real(kind=WP), dimension(nel)             , intent(inout) :: f33         !< Z-force at node 3
   real(kind=WP), dimension(nel)             , intent(inout) :: f35         !< Z-force at node 5
   real(kind=WP), dimension(nel)             , intent(inout) :: f36         !< Z-force at node 6
   real(kind=WP), dimension(nel)             , intent(inout) :: f37         !< Z-force at node 7
   real(kind=WP), dimension(nel,3,4)         , intent(inout) :: fhour       !< Hourglass forces
   real(kind=WP), dimension(nel)             , intent(inout) :: eint        !< Internal energy
!C-----------------------------------------------
!C   L O C A L   V A R I A B L E S
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
   integer :: i, j, mx, mt, write_flag, hourglass
   integer :: nindx, ninievo, ilen
   integer, dimension(nel) :: indx
   integer, dimension(nel, 2) :: ipos
   integer, dimension(:), allocatable :: initype, evotype, evoshap, comptyp, tab_id, &
                                                            tab_el, fcrit

   real(kind=WP) :: dett
   real(kind=WP) :: jaci1, jaci2, jaci3, jaci4, jaci5, jaci6, jaci7, jaci8, jaci9
   real(kind=WP) :: jaci12, jaci45, jaci78
   real(kind=WP) :: x_17_46, x_28_35, y_17_46, y_28_35, z_17_46, z_28_35
   real(kind=WP) :: hx, hy, hz, h1x, h1y, h1z, h2x, h2y, h2z, h3x, h3y, h3z, h4x, h4y, h4z
   real(kind=WP) :: cc, rho0, g0, c1, nuu, thk
   real(kind=WP) :: vx3478, vx2358, vx1467, vx1256
   real(kind=WP) :: vy3478, vy2358, vy1467, vy1256
   real(kind=WP) :: vz3478, vz2358, vz1467, vz1256
   real(kind=WP) :: vx17, vy17, vx28, vy28, vx35, vy35, vx46, vy46
   real(kind=WP) :: vz17, vz28, vz35, vz46, h1vz, h2vz
   real(kind=WP) :: e_r, e_s, e_t
   real(kind=WP) :: hq13p, hq13n, hq24p, hq24n, ff

   real(kind=WP), dimension(nel) :: x17, x28, x35, x46
   real(kind=WP), dimension(nel) :: y17, y28, y35, y46
   real(kind=WP), dimension(nel) :: z17, z28, z35, z46
   real(kind=WP), dimension(nel) :: jac_59_68, jac_67_49, jac_48_57, jac_19_37
   real(kind=WP), dimension(nel) :: px1, px2, px3, px4
   real(kind=WP), dimension(nel) :: py1, py2, py3, py4
   real(kind=WP), dimension(nel) :: pz1, pz2, pz3, pz4
   real(kind=WP), dimension(nel) :: px1h1, px2h1, px3h1, px4h1
   real(kind=WP), dimension(nel) :: px1h2, px2h2, px3h2, px4h2
   real(kind=WP), dimension(nel) :: px1h3, px2h3, px3h3, px4h3
   real(kind=WP), dimension(nel) :: px1h4, px2h4, px3h4, px4h4
   real(kind=WP), dimension(nel) :: jac1, jac2, jac3, jac4, jac5, jac6, jac7, jac8, jac9, det
   real(kind=WP), dimension(nel) :: fcl, nfhz1, nfhz2, hgz1, hgz2
   real(kind=WP), dimension(nel) :: gg, e_dt, thk_1
   real(kind=WP), dimension(nel) :: cxx, caq
   real(kind=WP), dimension(nel) :: g_3dt, e0
   real(kind=WP), dimension(nel) :: nu1, nu2, nu3, nu4
   real(kind=WP), dimension(nel) :: hgx1, hgx2, hgx3, hgx4
   real(kind=WP), dimension(nel) :: hgy1, hgy2, hgy3, hgy4
   real(kind=WP), dimension(nel) :: hgz3, hgz4
   real(kind=WP), dimension(nel) :: jr_1, js_1, jt_1
   real(kind=WP), dimension(nel) :: jr0, js0, jt0
   real(kind=WP), dimension(nel) :: h11, h22, h33, h12, h13, h23
   real(kind=WP), dimension(nel, 3, 4) :: fhour2, dfhour, nfhour
   real(kind=WP), dimension(3, 4) :: fhourt
   real(kind=WP), dimension(nel) :: nfhx1, nfhx2, nfhx3, nfhx4
   real(kind=WP), dimension(nel) :: nfhy1, nfhy2, nfhy3, nfhy4
   real(kind=WP), dimension(nel) :: nfhz3, nfhz4
   real(kind=WP), dimension(nel) :: f11_hgl, f12_hgl, f13_hgl, f14_hgl, f15_hgl, f16_hgl
   real(kind=WP), dimension(nel) :: f17_hgl, f18_hgl
   real(kind=WP), dimension(nel) :: f21_hgl, f22_hgl, f23_hgl, f24_hgl, f25_hgl, f26_hgl
   real(kind=WP), dimension(nel) :: f27_hgl, f28_hgl
   real(kind=WP), dimension(nel) :: f31_hgl, f32_hgl, f33_hgl, f34_hgl, f35_hgl, f36_hgl
   real(kind=WP), dimension(nel) :: f37_hgl, f38_hgl
!C=======================================================================
!C Same in Isolid24

      write_flag = 0
      hourglass = 1
       
      if (write_flag == 1) then
      do i=1,nel
      write(*,*) 'i = ', i
      write(*,*) 'x1(i) = ', x1(i)
      write(*,*) 'x2(i) = ', x2(i)
      write(*,*) 'x3(i) = ', x3(i)
      write(*,*) 'x4(i) = ', x4(i)
      write(*,*) 'x5(i) = ', x5(i)
      write(*,*) 'x6(i) = ', x6(i)
      write(*,*) 'x7(i) = ', x7(i)
      write(*,*) 'x8(i) = ', x8(i)
      write(*,*) 'y1(i) = ', y1(i)
      write(*,*) 'y2(i) = ', y2(i)
      write(*,*) 'y3(i) = ', y3(i)
      write(*,*) 'y4(i) = ', y4(i)
      write(*,*) 'y5(i) = ', y5(i)
      write(*,*) 'y6(i) = ', y6(i)
      write(*,*) 'y7(i) = ', y7(i)
      write(*,*) 'y8(i) = ', y8(i)
      write(*,*) 'z1(i) = ', z1(i)
      write(*,*) 'z2(i) = ', z2(i)
      write(*,*) 'z3(i) = ', z3(i)
      write(*,*) 'z4(i) = ', z4(i)
      write(*,*) 'z5(i) = ', z5(i)
      write(*,*) 'z6(i) = ', z6(i)
      write(*,*) 'z7(i) = ', z7(i)
      write(*,*) 'z8(i) = ', z8(i)

      write(*,*) 'vx1(i) = ', vx1(i)
      write(*,*) 'vx2(i) = ', vx2(i)
      write(*,*) 'vx3(i) = ', vx3(i)
      write(*,*) 'vx4(i) = ', vx4(i)
      write(*,*) 'vx5(i) = ', vx5(i)
      write(*,*) 'vx6(i) = ', vx6(i)
      write(*,*) 'vx7(i) = ', vx7(i)
      write(*,*) 'vx8(i) = ', vx8(i)
      write(*,*) 'vy1(i) = ', vy1(i)
      write(*,*) 'vy2(i) = ', vy2(i)
      write(*,*) 'vy3(i) = ', vy3(i)
      write(*,*) 'vy4(i) = ', vy4(i)
      write(*,*) 'vy5(i) = ', vy5(i)
      write(*,*) 'vy6(i) = ', vy6(i)
      write(*,*) 'vy7(i) = ', vy7(i)
      write(*,*) 'vy8(i) = ', vy8(i)
      write(*,*) 'vz1(i) = ', vz1(i)
      write(*,*) 'vz2(i) = ', vz2(i)
      write(*,*) 'vz3(i) = ', vz3(i)
      write(*,*) 'vz4(i) = ', vz4(i)
      write(*,*) 'vz5(i) = ', vz5(i)
      write(*,*) 'vz6(i) = ', vz6(i)
      write(*,*) 'vz7(i) = ', vz7(i)
      write(*,*) 'vz8(i) = ', vz8(i)
      !!pause
      end do 
      end if

       mx = mat(1) !//todo:(a verifier mat ? ) 
   !       mx = 1
   !//todo:input
   ! pm  !//todo:input
       rho0=pm(1,mx)
       !//todo: compare with nu(*)
       nuu=pm(21,mx) 
   !//todo:a distinguish
       g0=pm(22,mx)
       c1=pm(32,mx)  
       if (write_flag == 1) then
       write(*,*) 'mx = ', mx
       write(*,*) 'rho0=',rho0,' nuu=',nuu,' g0=',g0,' c1=',c1 
       end if
   ! rho0= 7.8500000000000008e-009
   ! nuu= 0.29999999999999999
   ! g0= 80769.230769230766
   ! c1= 174999.99999999997 
      
   !   do i=1,nel       
   !     gg(i)=half*rho0*cxx(i)*cxx(i)*(one -two*nu)/(one-nu)     
   !    end do
      do i=1,nel
        cxx(i) = ssp(i) !//todo:a verifier 
        
        gg(i)=half*rho0*cxx(i)*cxx(i)*(one -two*nuu)/(one-nuu)     
        !gg(i)=1.
        if (write_flag == 1) then
        write(*,*) 'nu(i)',nu(i) !//identique à nuu
        write(*,*) 'cxx(i)',cxx(i)
        write(*,*) 'gg',gg(i)
        end if
      end do
      

      mt = pid(1)

      do i=1,nel
        
         caq(i)=fourth*off(i)*geo(13,mt)
         if (write_flag == 1) then
         write(*,*) 'geo(13,mt)',geo(13,mt)
         write(*,*) 'caq',caq(i)
         end if
        !caq(i) = 0.25
        !fhour(i,1,1) = 0. 
        !fhour(i,1,2) = 0. 
        !fhour(i,1,3) = 0.
        !fhour(i,1,4) = 0.  
        !fhour(i,2,1) = 0. 
        !fhour(i,2,2) = 0. 
        !fhour(i,2,3) = 0.
        !fhour(i,2,4) = 0. 
        !fhour2(i,3,1) = 0. 
        !fhour2(i,3,1) = fhour(i,1)
        !fhour2(i,3,2) = 0. 
        !fhour2(i,3,2) = fhour(i,2)
        !fhour2(i,3,3) = 0.
        !fhour2(i,3,4) = 0. 

      end do

   !   do i=1,nel
   !     g_3dt(i)=third*off(i)*gg(i)*dt1
   !     e0(i)=two*(one+nu)*gg(i)
    !  enddo

      do i=1,nel
        g_3dt(i)=third*off(i)*gg(i)*dt1
        e0(i)=two*(one+nu(i))*gg(i)
        if (write_flag == 1) then
        write(*,*) 'g_3dt',g_3dt(i)
        write(*,*) 'e0',e0(i)
        endif
      enddo      

!c//todo:a verifier
   do i=1,nel
    ! write(*,*) 'nuuuuuuuuuuuuu'
     nu1(i) =two/(one-nuu)
     nu2(i) =nuu*nu1(i)
     nu3(i) =two_third*(one + nuu)
     nu4(i) =nuu
     if (write_flag == 1) then
     write(*,*) 'nu1=',nu1(i)
     write(*,*) 'nu2=',nu2(i)
     write(*,*) 'nu3=',nu3(i)
     write(*,*) 'nu4=',nu4(i)
     endif
   enddo   
   !!pause

   do i=1,nel
     fcl(i)=caq(i)*rho(i)*vol(i)**third
     fcl(i)=zep00666666667*fcl(i)*cxx(i)
     if (write_flag == 1) then
     write(*,*) 'fcl=',fcl(i)
     endif
   enddo


    
!c
   do i=1,nel
!c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//szderi3        
     x17(i)=x7(i)-x1(i)
     x28(i)=x8(i)-x2(i)
     x35(i)=x5(i)-x3(i)
     x46(i)=x6(i)-x4(i)

     y17(i)=y7(i)-y1(i)
     y28(i)=y8(i)-y2(i)
     y35(i)=y5(i)-y3(i)
     y46(i)=y6(i)-y4(i)

     z17(i)=z7(i)-z1(i)
     z28(i)=z8(i)-z2(i)
     z35(i)=z5(i)-z3(i)
     z46(i)=z6(i)-z4(i)
   end do

!c jacobian matrix
   do i=1,nel
!c//
     jac4(i)=x17(i)+x28(i)-x35(i)-x46(i)
     jac5(i)=y17(i)+y28(i)-y35(i)-y46(i)
     jac6(i)=z17(i)+z28(i)-z35(i)-z46(i)

     x_17_46=x17(i)+x46(i)
     x_28_35=x28(i)+x35(i)
     y_17_46=y17(i)+y46(i)
     y_28_35=y28(i)+y35(i)
     z_17_46=z17(i)+z46(i)
     z_28_35=z28(i)+z35(i)

     jac7(i)=x_17_46+x_28_35
     jac8(i)=y_17_46+y_28_35
     jac9(i)=z_17_46+z_28_35
     jac1(i)=x_17_46-x_28_35
     jac2(i)=y_17_46-y_28_35
     jac3(i)=z_17_46-z_28_35
     
     jac_59_68(i)=jac5(i)*jac9(i)-jac6(i)*jac8(i)
     jac_67_49(i)=jac6(i)*jac7(i)-jac4(i)*jac9(i)
     jac_19_37(i)=jac1(i)*jac9(i)-jac3(i)*jac7(i)
     jac_48_57(i)=jac4(i)*jac8(i)-jac5(i)*jac7(i)


     det(i)=one_over_64*(jac1(i)*jac_59_68(i)+jac2(i)*jac_67_49(i)+jac3(i)*jac_48_57(i))
     if (write_flag == 1) then
     write(*,*) 'jac1 = ', jac1(i)
     write(*,*) 'jac2 = ', jac2(i)
     write(*,*) 'jac3 = ', jac3(i)
     write(*,*) 'jac4 = ', jac4(i)
     write(*,*) 'jac5 = ', jac5(i)
     write(*,*) 'jac6 = ', jac6(i)
     write(*,*) 'jac7 = ', jac7(i)
     write(*,*) 'jac8 = ', jac8(i)
     write(*,*) 'jac9 = ', jac9(i)
     write(*,*) 'det = ', det(i)
     !pause
     endif 

     thk =fourth*jac5(i) !//todo: a verifier
     thk_1(i) =one/thk !//todo: a verifier
   enddo


  
!c jacobian matrix inverse 
   do i=1,nel
!c//same in the szderi3    
     dett=one_over_64/det(i)
     jaci1=dett*jac_59_68(i)
     jaci4=dett*jac_67_49(i)
     jaci7=dett*jac_48_57(i)
     jaci2=dett*(-jac2(i)*jac9(i)+jac3(i)*jac8(i))
     jaci5=dett*( jac1(i)*jac9(i)-jac3(i)*jac7(i))
     jaci8=dett*(-jac1(i)*jac8(i)+jac2(i)*jac7(i))
     jaci3=dett*( jac2(i)*jac6(i)-jac3(i)*jac5(i))
     jaci6=dett*(-jac1(i)*jac6(i)+jac3(i)*jac4(i))
     jaci9=dett*( jac1(i)*jac5(i)-jac2(i)*jac4(i))
!c
     jaci12=jaci1-jaci2
     jaci45=jaci4-jaci5
     jaci78=jaci7-jaci8
     px2(i)= jaci12-jaci3
     py2(i)= jaci45-jaci6
     pz2(i)= jaci78-jaci9
     px4(i)=-jaci12-jaci3
     py4(i)=-jaci45-jaci6
     pz4(i)=-jaci78-jaci9

     jaci12=jaci1+jaci2
     jaci45=jaci4+jaci5
     jaci78=jaci7+jaci8
     px1(i)=-jaci12-jaci3
     py1(i)=-jaci45-jaci6
     pz1(i)=-jaci78-jaci9
     px3(i)=jaci12-jaci3
     py3(i)=jaci45-jaci6
     pz3(i)=jaci78-jaci9
     if (write_flag == 1) then
     write(*,*) 'jaci1 = ', jaci1
     write(*,*) 'jaci2 = ', jaci2
     write(*,*) 'jaci3 = ', jaci3
     write(*,*) 'jaci4 = ', jaci4
     write(*,*) 'jaci5 = ', jaci5
     write(*,*) 'jaci6 = ', jaci6
     write(*,*) 'jaci7 = ', jaci7
     write(*,*) 'jaci8 = ', jaci8
     write(*,*) 'jaci9 = ', jaci9

     
      write(*,*) 'px1(i) = ', px1(i)
      write(*,*) 'px2(i) = ', px2(i)
      write(*,*) 'px3(i) = ', px3(i)
      write(*,*) 'px4(i) = ', px4(i)
      write(*,*) 'py1(i) = ', py1(i)
      write(*,*) 'py2(i) = ', py2(i)
      write(*,*) 'py3(i) = ', py3(i)
      write(*,*) 'py4(i) = ', py4(i)
      write(*,*) 'pz1(i) = ', pz1(i)
      write(*,*) 'pz2(i) = ', pz2(i)
      write(*,*) 'pz3(i) = ', pz3(i)
      write(*,*) 'pz4(i) = ', pz4(i)      
      write(*,*) 'i = ', i
     
   !pause
     endif



   enddo

!c we do it in the same order of szderi3
!c h3
!c 1 -1 1 -1 1 -1 1 -1
   do i=1,nel
     h3x=x1(i)-x2(i)+x3(i)-x4(i)+x5(i)-x6(i)+x7(i)-x8(i)
     h3y=y1(i)-y2(i)+y3(i)-y4(i)+y5(i)-y6(i)+y7(i)-y8(i)
     h3z=z1(i)-z2(i)+z3(i)-z4(i)+z5(i)-z6(i)+z7(i)-z8(i)   
     hx=one_over_8*h3x
     hy=one_over_8*h3y
     hz=one_over_8*h3z
     px1h3(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
     px2h3(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
     px3h3(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
     px4h3(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
     if (write_flag == 1) then
     write(*,*) 'px1h3(i) = ', px1h3(i)
     write(*,*) 'px2h3(i) = ', px2h3(i)
     write(*,*) 'px3h3(i) = ', px3h3(i)
     write(*,*) 'px4h3(i) = ', px4h3(i)
     write(*,*) 'i = ', i
     !pause
     endif

   end do   
!//A VERIFIER       
!       DO I=1,NEL
!         G13(I)= ONE_OVER_8-PX1H3(I)
!         G23(I)=-ONE_OVER_8-PX2H3(I)
!         G33(I)= ONE_OVER_8-PX3H3(I)
!         G43(I)=-ONE_OVER_8-PX4H3(I)
!         G53(I)= ONE_OVER_8+PX3H3(I)
!         G63(I)=-ONE_OVER_8+PX4H3(I)
!         G73(I)= ONE_OVER_8+PX1H3(I)
!         G83(I)=-ONE_OVER_8+PX2H3(I)
!       ENDDO
!C!    H1
!c! 1 1 -1 -1 -1 -1 1 1
   do i=1,nel
     h1x=x1(i)+x2(i)-x3(i)-x4(i)-x5(i)-x6(i)+x7(i)+x8(i)
     h1y=y1(i)+y2(i)-y3(i)-y4(i)-y5(i)-y6(i)+y7(i)+y8(i)
     h1z=z1(i)+z2(i)-z3(i)-z4(i)-z5(i)-z6(i)+z7(i)+z8(i)
     hx=one_over_8*h1x
     hy=one_over_8*h1y
     hz=one_over_8*h1z
     px1h1(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
     px2h1(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
     px3h1(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
     px4h1(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
     if (write_flag == 1) then
     write(*,*) 'px1h1(i) = ', px1h1(i)
     write(*,*) 'px2h1(i) = ', px2h1(i)
     write(*,*) 'px3h1(i) = ', px3h1(i)
     write(*,*) 'px4h1(i) = ', px4h1(i)
     write(*,*) 'i = ', i
     !pause 
     endif
   end do
!//A VERIFIER  
!       DO I=1,NEL
!         G11(I)= ONE_OVER_8-PX1H1(I)
!         G21(I)= ONE_OVER_8-PX2H1(I)
!         G31(I)=-ONE_OVER_8-PX3H1(I)
!         G41(I)=-ONE_OVER_8-PX4H1(I)
!         G51(I)=-ONE_OVER_8+PX3H1(I)
!         G61(I)=-ONE_OVER_8+PX4H1(I)
!         G71(I)= ONE_OVER_8+PX1H1(I)
!         G81(I)= ONE_OVER_8+PX2H1(I)
!       ENDDO
!C   H2
!c 1 -1 -1 1 -1 1 1 -1
   do i=1,nel
     h2x=x1(i)-x2(i)-x3(i)+x4(i)-x5(i)+x6(i)+x7(i)-x8(i)
     h2y=y1(i)-y2(i)-y3(i)+y4(i)-y5(i)+y6(i)+y7(i)-y8(i)
     h2z=z1(i)-z2(i)-z3(i)+z4(i)-z5(i)+z6(i)+z7(i)-z8(i)
     hx=one_over_8*h2x
     hy=one_over_8*h2y
     hz=one_over_8*h2z   
     px1h2(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
     px2h2(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
     px3h2(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
     px4h2(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
     if (write_flag == 1) then
     write(*,*) 'px1h2(i) = ', px1h2(i)
     write(*,*) 'px2h2(i) = ', px2h2(i)
     write(*,*) 'px3h2(i) = ', px3h2(i)
     write(*,*) 'px4h2(i) = ', px4h2(i)
     write(*,*) 'i = ', i
     !pause
     endif
   end do
!//A VERIFIER  
!       DO I=1,NEL
!         G12(I)= ONE_OVER_8-PX1H2(I)
!         G22(I)=-ONE_OVER_8-PX2H2(I)
!         G32(I)=-ONE_OVER_8-PX3H2(I)
!         G42(I)= ONE_OVER_8-PX4H2(I)
!         G52(I)=-ONE_OVER_8+PX3H2(I)
!         G62(I)= ONE_OVER_8+PX4H2(I)
!         G72(I)= ONE_OVER_8+PX1H2(I)
!         G82(I)=-ONE_OVER_8+PX2H2(I)
!       ENDDO
!C   H4       
!C -1 1 -1 1 1 -1 1 -1
      do i=1,nel
        h4x=-x1(i)+x2(i)-x3(i)+x4(i)+x5(i)-x6(i)+x7(i)-x8(i)
        h4y=-y1(i)+y2(i)-y3(i)+y4(i)+y5(i)-y6(i)+y7(i)-y8(i)
        h4z=-z1(i)+z2(i)-z3(i)+z4(i)+z5(i)-z6(i)+z7(i)-z8(i)   
        hx=one_over_8*h4x
        hy=one_over_8*h4y
        hz=one_over_8*h4z   
        px1h4(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
        px2h4(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
        px3h4(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
        px4h4(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz

        if (write_flag == 1) then
        write(*,*) 'px1h4(i) = ', px1h4(i)
        write(*,*) 'px2h4(i) = ', px2h4(i)
        write(*,*) 'px3h4(i) = ', px3h4(i)
        write(*,*) 'px4h4(i) = ', px4h4(i)
        write(*,*) 'i = ', i
        !pause  
        endif
      end do


!//A VERIFIER 
!       DO I=1,NEL
!         G14(I)=-ONE_OVER_8-PX1H4(I)
!         G24(I)= ONE_OVER_8-PX2H4(I)
!         G34(I)=-ONE_OVER_8-PX3H4(I)
!         G44(I)= ONE_OVER_8-PX4H4(I)
!         G54(I)= ONE_OVER_8+PX3H4(I)
!         G64(I)=-ONE_OVER_8+PX4H4(I)
!         G74(I)= ONE_OVER_8+PX1H4(I)
!         G84(I)=-ONE_OVER_8+PX2H4(I)
!       ENDDO

!C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//szderi3 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCccccccccccc
!       DO I=1,NEL
!         VZ17=VZ1(I)-VZ7(I)
!         VZ28=VZ2(I)-VZ8(I)
!         VZ35=VZ3(I)-VZ5(I)
!         VZ46=VZ4(I)-VZ6(I)
!         H1VZ=VZ1(I)+VZ2(I)-VZ3(I)-VZ4(I)
!     .           -VZ5(I)-VZ6(I)+VZ7(I)+VZ8(I)
!         HGZ1(I)=ONE_OVER_8*H1VZ-
!     .         (PX1H1(I)*VZ17+PX2H1(I)*VZ28+
!     .          PX3H1(I)*VZ35+PX4H1(I)*VZ46)
!         H2VZ=VZ1(I)-VZ2(I)-VZ3(I)+VZ4(I)
!     .         -VZ5(I)+VZ6(I)+VZ7(I)-VZ8(I)
!         HGZ2(I)=ONE_OVER_8*H2VZ-
!     .         (PX1H2(I)*VZ17+PX2H2(I)*VZ28+
!     .          PX3H2(I)*VZ35+PX4H2(I)*VZ46)
!       ENDDO

!//
!!!To Calculate q/dt
!C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//szhour3       
      do i=1,nel
       vx3478=vx3(i)-vx4(i)-vx7(i)+vx8(i)
       vx2358=vx2(i)-vx3(i)-vx5(i)+vx8(i)
       vx1467=vx1(i)-vx4(i)-vx6(i)+vx7(i)
       vx1256=vx1(i)-vx2(i)-vx5(i)+vx6(i)
   !c
       vy3478=vy3(i)-vy4(i)-vy7(i)+vy8(i)
       vy2358=vy2(i)-vy3(i)-vy5(i)+vy8(i)
       vy1467=vy1(i)-vy4(i)-vy6(i)+vy7(i)
       vy1256=vy1(i)-vy2(i)-vy5(i)+vy6(i)
   !c
       vz3478=vz3(i)-vz4(i)-vz7(i)+vz8(i)
       vz2358=vz2(i)-vz3(i)-vz5(i)+vz8(i)
       vz1467=vz1(i)-vz4(i)-vz6(i)+vz7(i)
       vz1256=vz1(i)-vz2(i)-vz5(i)+vz6(i)
       if (write_flag == 1) then
       write(*,*) 'vx1467',vx1467
       write(*,*) 'vx2358',vx2358
       write(*,*) 'hgx3',hgx3(i) 
       endif

       hgx3(i)=(vx1467-vx2358)*one_over_8


       hgx1(i)=(vx1467+vx2358)*one_over_8
       hgx2(i)=(vx1256-vx3478)*one_over_8
       hgx4(i)=-(vx1256+vx3478)*one_over_8       
   !c
       hgy3(i)=(vy1467-vy2358)*one_over_8
       hgy1(i)=(vy1467+vy2358)*one_over_8
       hgy2(i)=(vy1256-vy3478)*one_over_8
       hgy4(i)=-(vy1256+vy3478)*one_over_8       
   !c
       hgz3(i)=(vz1467-vz2358)*one_over_8
       hgz1(i)=(vz1467+vz2358)*one_over_8
       hgz2(i)=(vz1256-vz3478)*one_over_8
       hgz4(i)=-(vz1256+vz3478)*one_over_8      
   !a verifier    
       if (write_flag == 1) then    

         write(*,*) 'atention please !!!!! hgx3'
         write(*,*) 'hgx1',hgx1(i)
         write(*,*) 'hgx2',hgx2(i)
         write(*,*) 'hgx3',hgx3(i)
         write(*,*) 'hgx4',hgx4(i)

         write(*,*) 'hgy1',hgy1(i)
         write(*,*) 'hgy2',hgy2(i)
         write(*,*) 'hgy3',hgy3(i)
         write(*,*) 'hgy4',hgy4(i)

         write(*,*) 'hgz1',hgz1(i)
         write(*,*) 'hgz2',hgz2(i)
         write(*,*) 'hgz3',hgz3(i)
         write(*,*) 'hgz4',hgz4(i)
         write(*,*) 'i = ', i
       endif  
      enddo

      do i=1,nel
        vx17=vx1(i)-vx7(i)
        vx28=vx2(i)-vx8(i)
        vx35=vx3(i)-vx5(i)
        vx46=vx4(i)-vx6(i)
        vy17=vy1(i)-vy7(i)
        vy28=vy2(i)-vy8(i)
        vy35=vy3(i)-vy5(i)
        vy46=vy4(i)-vy6(i)
        vz17=vz1(i)-vz7(i)
        vz28=vz2(i)-vz8(i)
        vz35=vz3(i)-vz5(i)
        vz46=vz4(i)-vz6(i)


!C   alpha =1 ->eta zeta   
!C 1 1 -1 -1 -1 -1 1 1
!vy1467=vy1(i)-vy4(i)-vy6(i)+vy7(i)
!vz2358=vz2(i)-vz3(i)-vz5(i)+vz8(i)
!hgx1(i)=(vx1467+vx2358)*one_over_8
    
    hgx1(i)= hgx1(i) &
             -(px1h1(i)*vx17+px2h1(i)*vx28 &
               +px3h1(i)*vx35+px4h1(i)*vx46)
    hgy1(i)= hgy1(i) &
             -(px1h1(i)*vy17+px2h1(i)*vy28 &
               +px3h1(i)*vy35+px4h1(i)*vy46)
    hgz1(i)= hgz1(i) &
             -(px1h1(i)*vz17+px2h1(i)*vz28 &
               +px3h1(i)*vz35+px4h1(i)*vz46)
! write(*,*) 'hgx1',hgx1(i)
! write(*,*) 'hgy1',hgy1(i)
! write(*,*) 'hgz1',hgz1(i)
!c   alpha =2 ->zeta ksi   
!c 1 -1 -1 1 -1 1 1 -1
    hgx2(i)= hgx2(i) &
             -(px1h2(i)*vx17+px2h2(i)*vx28 &
               +px3h2(i)*vx35+px4h2(i)*vx46)
    hgy2(i)= hgy2(i) &
             -(px1h2(i)*vy17+px2h2(i)*vy28 &
               +px3h2(i)*vy35+px4h2(i)*vy46)
    hgz2(i)= hgz2(i) &
             -(px1h2(i)*vz17+px2h2(i)*vz28 &
               +px3h2(i)*vz35+px4h2(i)*vz46)
! write(*,*) 'hgx2',hgx2(i)
! write(*,*) 'hgy2',hgy2(i)
! write(*,*) 'hgz2',hgz2(i)
!c   alpha =3 ->ksi eta    
!c 1 -1 1 -1 1 -1 1 -1
    hgx3(i)= hgx3(i) &
             -(px1h3(i)*vx17+px2h3(i)*vx28 &
               +px3h3(i)*vx35+px4h3(i)*vx46)
    hgy3(i)= hgy3(i) &
             -(px1h3(i)*vy17+px2h3(i)*vy28 &
               +px3h3(i)*vy35+px4h3(i)*vy46)
    hgz3(i)= hgz3(i) &
             -(px1h3(i)*vz17+px2h3(i)*vz28 &
               +px3h3(i)*vz35+px4h3(i)*vz46)
!  write(*,*) 'hgx3',hgx3(i)
! write(*,*) 'hgy3',hgy3(i)
!  write(*,*) 'hgz3',hgz3(i)
!c
!c   alpha =4 ->ksi eta zeta
!c -1 1 -1 1 1 -1 1 -1
    hgx4(i)= hgx4(i) &
             -(px1h4(i)*vx17+px2h4(i)*vx28 &
               +px3h4(i)*vx35+px4h4(i)*vx46)
    hgy4(i)= hgy4(i) &
             -(px1h4(i)*vy17+px2h4(i)*vy28 &
               +px3h4(i)*vy35+px4h4(i)*vy46)
    hgz4(i)= hgz4(i) &
             -(px1h4(i)*vz17+px2h4(i)*vz28 &
               +px3h4(i)*vz35+px4h4(i)*vz46)
! write(*,*) 'hgx4',hgx4(i)
! write(*,*) 'hgy4',hgy4(i)
! write(*,*) 'hgz4',hgz4(i)
     if (write_flag == 1) then
     write(*,*) 'hgx1',hgx1(i)
     write(*,*) 'hgx2',hgx2(i)
     write(*,*) 'hgx3',hgx3(i)
     write(*,*) 'hgx4',hgx4(i)

     write(*,*) 'hgy1',hgy1(i)
     write(*,*) 'hgy2',hgy2(i)
     write(*,*) 'hgy3',hgy3(i)
     write(*,*) 'hgy4',hgy4(i)

     write(*,*) 'hgz1',hgz1(i)
     write(*,*) 'hgz2',hgz2(i)
     write(*,*) 'hgz3',hgz3(i)
     write(*,*) 'hgz4',hgz4(i)

     write(*,*) 'i = ', i
     !pause
     endif 
     enddo
      !q!!!!!!! above
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   do i=1,nel 
! write(*,*) 'a'
     jr0(i) = jac1(i)
     js0(i) = jac5(i)
     jt0(i) = jac9(i)
!  jac1 r,         jac5 s,         jac9 t
   jr_1(i) = one/max(em20,jr0(i))
   js_1(i) = one/max(em20,js0(i))
   jt_1(i) = one/max(em20,jt0(i))
   h11(i) = js0(i)*jt0(i)*jr_1(i)
   h22(i) = jr0(i)*jt0(i)*js_1(i)
   h33(i) = jr0(i)*js0(i)*jt_1(i)
   h12(i) = jt0(i)
   h13(i) = js0(i)
   h23(i) = jr0(i)
   if (write_flag == 1) then
   write(*,*) 'jr0',jr0(i)
   write(*,*) 'js0',js0(i)
   write(*,*) 'jt0',jt0(i)

   write(*,*) 'jr_1',jr_1(i)
   write(*,*) 'js_1',js_1(i)
   write(*,*) 'jt_1',jt_1(i)
     

   write(*,*) 'h11',h11(i)
   write(*,*) 'h22',h22(i)
   write(*,*) 'h33',h33(i)
   write(*,*) 'h12',h12(i)
   write(*,*) 'h13',h13(i)
   write(*,*) 'h23',h23(i)

   write(*,*) 'i = ', i
   write(*,*) 'off',off(i)
   !pause
   endif

!      !hii
   enddo
!ccccccccccccccccccccccccccccccccccc      
    !   do i=1,nel
    !    write(*,*) 'b'
    !    fhour(i,1,1) = fhour(i,1,1)*off(i)
    !    fhour(i,1,2) = fhour(i,1,2)*off(i)
    !    fhour(i,1,3) = fhour(i,1,3)*off(i)
    !    fhour(i,1,4) = fhour(i,1,4)*off(i)
    !    fhour(i,2,1) = fhour(i,2,1)*off(i)
    !    fhour(i,2,2) = fhour(i,2,2)*off(i)
    !    fhour(i,2,3) = fhour(i,2,3)*off(i)
    !    fhour(i,2,4) = fhour(i,2,4)*off(i)
    !    fhour(i,3,1) = fhour(i,3,1)*off(i)
    !    fhour(i,3,2) = fhour(i,3,2)*off(i)
    !    fhour(i,3,3) = fhour(i,3,3)*off(i)
    !    fhour(i,3,4) = fhour(i,3,4)*off(i)
    !   enddo
   do i=1,nel
!        !//todo: a verifier g_3dt         
     e_r =g_3dt(i)*jr_1(i)
     e_s =g_3dt(i)*js_1(i)
     e_t =g_3dt(i)*jt_1(i)

     
     if (write_flag == 1) then
     write(*,*) 'i = ', i
     write(*,*) 'e_r = ', e_r
     write(*,*) 'e_s = ', e_s
     write(*,*) 'e_t = ', e_t
     endif

     dfhour(i,1,1) = e_r*hgx1(i)
     dfhour(i,1,2) = e_r*hgx2(i)
     dfhour(i,1,3) = e_r*hgx3(i)
     dfhour(i,1,4) = e_r*hgx4(i)

     dfhour(i,2,1) = e_s*hgy1(i)
     dfhour(i,2,2) = e_s*hgy2(i)
     dfhour(i,2,3) = e_s*hgy3(i)
     dfhour(i,2,4) = e_s*hgy4(i)
   
     dfhour(i,3,1) = e_t*hgz1(i)
     dfhour(i,3,2) = e_t*hgz2(i)
     dfhour(i,3,3) = e_t*hgz3(i)
     dfhour(i,3,4) = e_t*hgz4(i)  

     if (write_flag == 1) then   
     write(*,*) 'fhour(i,1,1) = ',fhour(i,1,1)
     write(*,*) 'fhour(i,1,2) = ',fhour(i,1,2)
     write(*,*) 'fhour(i,1,3) = ',fhour(i,1,3)
     write(*,*) 'fhour(i,1,4) = ',fhour(i,1,4)
     write(*,*) 'fhour(i,2,1) = ',fhour(i,2,1)
     write(*,*) 'fhour(i,2,2) = ',fhour(i,2,2)
     write(*,*) 'fhour(i,2,3) = ',fhour(i,2,3)
     write(*,*) 'fhour(i,2,4) = ',fhour(i,2,4)
     write(*,*) 'fhour(i,3,1) = ',fhour(i,3,1)
     write(*,*) 'fhour(i,3,2) = ',fhour(i,3,2)
     write(*,*) 'fhour(i,3,3) = ',fhour(i,3,3)
     write(*,*) 'fhour(i,3,4) = ',fhour(i,3,4)

     write(*,*) 'dfhour(i,1,1) = ',dfhour(i,1,1)
     write(*,*) 'dfhour(i,1,2) = ',dfhour(i,1,2)
     write(*,*) 'dfhour(i,1,3) = ',dfhour(i,1,3)
     write(*,*) 'dfhour(i,1,4) = ',dfhour(i,1,4)
     write(*,*) 'dfhour(i,2,1) = ',dfhour(i,2,1)
     write(*,*) 'dfhour(i,2,2) = ',dfhour(i,2,2)
     write(*,*) 'dfhour(i,2,3) = ',dfhour(i,2,3)
     write(*,*) 'dfhour(i,2,4) = ',dfhour(i,2,4)
     write(*,*) 'dfhour(i,3,1) = ',dfhour(i,3,1)
     write(*,*) 'dfhour(i,3,2) = ',dfhour(i,3,2)
     write(*,*) 'dfhour(i,3,3) = ',dfhour(i,3,3)
     write(*,*) 'dfhour(i,3,4) = ',dfhour(i,3,4)
     endif
!c
     fhour(i,1,1) = fhour(i,1,1) + dfhour(i,1,1)
     fhour(i,1,2) = fhour(i,1,2) + dfhour(i,1,2)
     fhour(i,1,3) = fhour(i,1,3) + dfhour(i,1,3)
     fhour(i,1,4) = fhour(i,1,4) + dfhour(i,1,4)
     fhour(i,2,1) = fhour(i,2,1) + dfhour(i,2,1)
     fhour(i,2,2) = fhour(i,2,2) + dfhour(i,2,2)
     fhour(i,2,3) = fhour(i,2,3) + dfhour(i,2,3)
     fhour(i,2,4) = fhour(i,2,4) + dfhour(i,2,4)
     fhour(i,3,1) = fhour(i,3,1) + dfhour(i,3,1)
     fhour(i,3,2) = fhour(i,3,2) + dfhour(i,3,2)
     fhour(i,3,3) = fhour(i,3,3) + dfhour(i,3,3)
     fhour(i,3,4) = fhour(i,3,4) + dfhour(i,3,4)
     
     if (write_flag == 1) then   
     write(*,*) 'fhour(i,1,1) = ',fhour(i,1,1)
     write(*,*) 'fhour(i,1,2) = ',fhour(i,1,2)
     write(*,*) 'fhour(i,1,3) = ',fhour(i,1,3)
     write(*,*) 'fhour(i,1,4) = ',fhour(i,1,4)
 
     write(*,*) 'fhour(i,2,1) = ',fhour(i,2,1)
     write(*,*) 'fhour(i,2,2) = ',fhour(i,2,2)
     write(*,*) 'fhour(i,2,3) = ',fhour(i,2,3)
     write(*,*) 'fhour(i,2,4) = ',fhour(i,2,4)
 
     write(*,*) 'fhour(i,3,1) = ',fhour(i,3,1)
     write(*,*) 'fhour(i,3,2) = ',fhour(i,3,2)
     write(*,*) 'fhour(i,3,3) = ',fhour(i,3,3)
     write(*,*) 'fhour(i,3,4) = ',fhour(i,3,4)
     endif 
     !pause
   enddo
   !!pause
   do i=1,nel

!       write(*,*) 'fhour(i,1,1)*jr0(i)', fhour(i,1,1)*jr0(i)  
!       write(*,*) 'fcl(i)*hgx1(i)', fcl(i)*hgx1(i)  
!!       write(*,*) 'fhour(i,2,2)*jr0(i)', fhour(i,2,2)*jr0(i)  
!       write(*,*) 'fcl(i)*hgy2(i)', fcl(i)*hgy2(i) 
!       write(*,*) 'fhour(i,3,3)*jr0(i)', fhour(i,3,3)*jr0(i)  
!       write(*,*) 'fcl(i)*hgz3(i)', fcl(i)*hgz3(i)
!       !//todo:fcl
     fhourt(1,1) = fhour(i,1,1)*jr0(i)+fcl(i)*hgx1(i)
     fhourt(1,2) = fhour(i,1,2)*jr0(i)+fcl(i)*hgx2(i)
     fhourt(1,3) = fhour(i,1,3)*jr0(i)+fcl(i)*hgx3(i)
     fhourt(1,4) = fhour(i,1,4)*jr0(i)+fcl(i)*hgx4(i)
     fhourt(2,1) = fhour(i,2,1)*js0(i)+fcl(i)*hgy1(i)
     fhourt(2,2) = fhour(i,2,2)*js0(i)+fcl(i)*hgy2(i)
     fhourt(2,3) = fhour(i,2,3)*js0(i)+fcl(i)*hgy3(i)
     fhourt(2,4) = fhour(i,2,4)*js0(i)+fcl(i)*hgy4(i)
     fhourt(3,1) = fhour(i,3,1)*jt0(i)+fcl(i)*hgz1(i)
     fhourt(3,2) = fhour(i,3,2)*jt0(i)+fcl(i)*hgz2(i)
     fhourt(3,3) = fhour(i,3,3)*jt0(i)+fcl(i)*hgz3(i)
     fhourt(3,4) = fhour(i,3,4)*jt0(i)+fcl(i)*hgz4(i)
     if (write_flag == 1) then
     write(*,*) 'fhourt(1,1) = ',fhourt(1,1)
     write(*,*) 'fhourt(1,2) = ',fhourt(1,2)
     write(*,*) 'fhourt(1,3) = ',fhourt(1,3)
     write(*,*) 'fhourt(1,4) = ',fhourt(1,4)
 
     write(*,*) 'fhourt(2,1) = ',fhourt(2,1)
     write(*,*) 'fhourt(2,2) = ',fhourt(2,2)
     write(*,*) 'fhourt(2,3) = ',fhourt(2,3)
     write(*,*) 'fhourt(2,4) = ',fhourt(2,4)
 
     write(*,*) 'fhourt(3,1) = ',fhourt(3,1)
     write(*,*) 'fhourt(3,2) = ',fhourt(3,2)
     write(*,*) 'fhourt(3,3) = ',fhourt(3,3)
     write(*,*) 'fhourt(3,4) = ',fhourt(3,4)
     endif
!C NFHX1 NFHOUR(I,3(XYZ),4(1,2,3,4))
!//TODO:NU1 NNU2 NU3
!      !nfhour(i,1,1) nfhx1(i)
   nfhour(i,1,1) = (h22(i)+h33(i))*fhourt(1,1) &
                   +h12(i)*fhourt(2,2)+h13(i)*fhourt(3,3)
       !nfhour(i,2,2) nfhy2(i)
   nfhour(i,2,2) = (h11(i)+h33(i))*fhourt(2,2) &
                   +h23(i)*fhourt(3,3)+h12(i)*fhourt(1,1)
       !nfhour(i,3,3) nfhz3(i)
   nfhour(i,3,3) = (h11(i)+h22(i))*fhourt(3,3) &
                   +h13(i)*fhourt(1,1)+h23(i)*fhourt(2,2)
       !nfhour(i,1,2) nfhx2(i)
   nfhour(i,1,2) = nu1(i)*h11(i)*fhourt(1,2) &
                   +nu2(i)*h12(i)*fhourt(2,1)
       !nfhour(i,1,3) nfhx3(i)
   nfhour(i,1,3) = nu1(i)*h11(i)*fhourt(1,3) &
                   +nu2(i)*h13(i)*fhourt(3,1)
       !nfhour(i,2,1) nfhy1(i)
   nfhour(i,2,1) = nu1(i)*h22(i)*fhourt(2,1) &
                   +nu2(i)*h12(i)*fhourt(1,2)
       !nfhour(i,3,1)  nfhz1(i) nfhz1(i) = cc*fhour(i,1) + fcl(i)*hgz1(i)
   nfhour(i,3,1) = nu1(i)*h33(i)*fhourt(3,1) &
                   +nu2(i)*h13(i)*fhourt(1,3)
       !nfhour(i,2,3) nfhy3(i)
   nfhour(i,2,3) = nu1(i)*h22(i)*fhourt(2,3) &
                   +nu2(i)*h23(i)*fhourt(3,2)
       !nfhour(i,3,2)  nfhz2(i) nfhz2(i) = cc*fhour(i,2) + fcl(i)*hgz2(i)
   nfhour(i,3,2)= nu1(i)*h33(i)*fhourt(3,2) &
                  +nu2(i)*h23(i)*fhourt(2,3)
       !nfhour(i,1,4) nfhx4(i)
   nfhour(i,1,4) = nu3(i)*h11(i)*fhourt(1,4)
       !nfhour(i,2,4) nfhy4(i)
   nfhour(i,2,4) = nu3(i)*h22(i)*fhourt(2,4)
       !nfhour(i,3,4)  nfhz4(i)
   nfhour(i,3,4) = nu3(i)*h33(i)*fhourt(3,4)
   
   if (write_flag == 1) then
      write(*,*) 'nfhour(i,1,1) = ',nfhour(i,1,1)
      write(*,*) 'nfhour(i,1,2) = ',nfhour(i,1,2)
      write(*,*) 'nfhour(i,1,3) = ',nfhour(i,1,3)
      write(*,*) 'nfhour(i,1,4) = ',nfhour(i,1,4)
 
      write(*,*) 'nfhour(i,2,1) = ',nfhour(i,2,1)
      write(*,*) 'nfhour(i,2,2) = ',nfhour(i,2,2)
      write(*,*) 'nfhour(i,2,3) = ',nfhour(i,2,3)
      write(*,*) 'nfhour(i,2,4) = ',nfhour(i,2,4)
 
      write(*,*) 'nfhour(i,3,1) = ',nfhour(i,3,1)
      write(*,*) 'nfhour(i,3,2) = ',nfhour(i,3,2)
      write(*,*) 'nfhour(i,3,3) = ',nfhour(i,3,3)
      write(*,*) 'nfhour(i,3,4) = ',nfhour(i,3,4)
      
      write(*,*) 'i = ', i
   end if
   end do
   !!pause
   
   do i=1,nel
     hq13p = (nfhour(i,1,1)+nfhour(i,1,3))*one_over_8
     hq13n = (nfhour(i,1,1)-nfhour(i,1,3))*one_over_8
     hq24p = (nfhour(i,1,2)+nfhour(i,1,4))*one_over_8
     hq24n = (nfhour(i,1,2)-nfhour(i,1,4))*one_over_8
     ff =-px1h1(i)*nfhour(i,1,1)-px1h2(i)*nfhour(i,1,2) &
         -px1h3(i)*nfhour(i,1,3)-px1h4(i)*nfhour(i,1,4)
     f11_hgl(i) =-(hq13p+hq24n+ff)
     f17_hgl(i) =-(hq13p+hq24p-ff)
     ff =-px2h1(i)*nfhour(i,1,1)-px2h2(i)*nfhour(i,1,2) &
         -px2h3(i)*nfhour(i,1,3)-px2h4(i)*nfhour(i,1,4)
     f12_hgl(i) =-(hq13n-hq24n+ff)
     f18_hgl(i) =-(hq13n-hq24p-ff)
     ff =-px3h1(i)*nfhour(i,1,1)-px3h2(i)*nfhour(i,1,2) &
         -px3h3(i)*nfhour(i,1,3)-px3h4(i)*nfhour(i,1,4)
     f13_hgl(i) =-(-hq13n-hq24p+ff)
     f15_hgl(i) =-(-hq13n-hq24n-ff)
     ff =-px4h1(i)*nfhour(i,1,1)-px4h2(i)*nfhour(i,1,2) &
         -px4h3(i)*nfhour(i,1,3)-px4h4(i)*nfhour(i,1,4)
     f14_hgl(i) =-(-hq13p+hq24p+ff)
     f16_hgl(i) =-(-hq13p+hq24n-ff)
   end do
   do i=1,nel
     hq13p = (nfhour(i,2,1)+nfhour(i,2,3))*one_over_8
     hq13n = (nfhour(i,2,1)-nfhour(i,2,3))*one_over_8
     hq24p = (nfhour(i,2,2)+nfhour(i,2,4))*one_over_8
     hq24n = (nfhour(i,2,2)-nfhour(i,2,4))*one_over_8
     ff =-px1h1(i)*nfhour(i,2,1)-px1h2(i)*nfhour(i,2,2) &
         -px1h3(i)*nfhour(i,2,3)-px1h4(i)*nfhour(i,2,4)
     f21_hgl(i) =-(hq13p+hq24n+ff)
     f27_hgl(i) =-(hq13p+hq24p-ff)
     ff =-px2h1(i)*nfhour(i,2,1)-px2h2(i)*nfhour(i,2,2) &
         -px2h3(i)*nfhour(i,2,3)-px2h4(i)*nfhour(i,2,4)
     f22_hgl(i) =-(hq13n-hq24n+ff)
     f28_hgl(i) =-(hq13n-hq24p-ff)
     ff =-px3h1(i)*nfhour(i,2,1)-px3h2(i)*nfhour(i,2,2) &
         -px3h3(i)*nfhour(i,2,3)-px3h4(i)*nfhour(i,2,4)
     f23_hgl(i) =-(-hq13n-hq24p+ff)
     f25_hgl(i) =-(-hq13n-hq24n-ff)
     ff =-px4h1(i)*nfhour(i,2,1)-px4h2(i)*nfhour(i,2,2) &
         -px4h3(i)*nfhour(i,2,3)-px4h4(i)*nfhour(i,2,4)
     f24_hgl(i) =-(-hq13p+hq24p+ff)
     f26_hgl(i) =-(-hq13p+hq24n-ff)
   end do
   do i=1,nel
     hq13p = (nfhour(i,3,1)+nfhour(i,3,3))*one_over_8
     hq13n = (nfhour(i,3,1)-nfhour(i,3,3))*one_over_8
     hq24p = (nfhour(i,3,2)+nfhour(i,3,4))*one_over_8
     hq24n = (nfhour(i,3,2)-nfhour(i,3,4))*one_over_8
     ff =-px1h1(i)*nfhour(i,3,1)-px1h2(i)*nfhour(i,3,2) &
         -px1h3(i)*nfhour(i,3,3)-px1h4(i)*nfhour(i,3,4)
     f31_hgl(i) =-(hq13p+hq24n+ff)
     f37_hgl(i) =-(hq13p+hq24p-ff)
     ff =-px2h1(i)*nfhour(i,3,1)-px2h2(i)*nfhour(i,3,2) &
         -px2h3(i)*nfhour(i,3,3)-px2h4(i)*nfhour(i,3,4)
     f32_hgl(i) =-(hq13n-hq24n+ff)
     f38_hgl(i) =-(hq13n-hq24p-ff)
     ff =-px3h1(i)*nfhour(i,3,1)-px3h2(i)*nfhour(i,3,2) &
         -px3h3(i)*nfhour(i,3,3)-px3h4(i)*nfhour(i,3,4)
     f33_hgl(i) =-(-hq13n-hq24p+ff)
     f35_hgl(i) =-(-hq13n-hq24n-ff)
     ff =-px4h1(i)*nfhour(i,3,1)-px4h2(i)*nfhour(i,3,2) &
         -px4h3(i)*nfhour(i,3,3)-px4h4(i)*nfhour(i,3,4)
     f34_hgl(i) =-(-hq13p+hq24p+ff)
     f36_hgl(i) =-(-hq13p+hq24n-ff)
   end do
!c------------------------------------------------
!c

      do i=1,nel
    if (write_flag == 1) then
     write(*,*) 'i = ', i
     write(*,*) 'f11_hgl(i) = ', f11_hgl(i)
     write(*,*) 'f21_hgl(i) = ', f21_hgl(i)
     write(*,*) 'f31_hgl(i) = ', f31_hgl(i) 
     write(*,*) 'f12_hgl(i) = ', f12_hgl(i)
     write(*,*) 'f22_hgl(i) = ', f22_hgl(i)
     write(*,*) 'f32_hgl(i) = ', f32_hgl(i) 
     write(*,*) 'f13_hgl(i) = ', f13_hgl(i)   
     write(*,*) 'f23_hgl(i) = ', f23_hgl(i)        
     write(*,*) 'f33_hgl(i) = ', f33_hgl(i) 
     write(*,*) 'f14_hgl(i) = ', f14_hgl(i)
     write(*,*) 'f24_hgl(i) = ', f24_hgl(i)
     write(*,*) 'f34_hgl(i) = ', f34_hgl(i) 
     write(*,*) 'f15_hgl(i) = ', f15_hgl(i)
     write(*,*) 'f25_hgl(i) = ', f25_hgl(i)
     write(*,*) 'f35_hgl(i) = ', f35_hgl(i) 
     write(*,*) 'f16_hgl(i) = ', f16_hgl(i)  
     write(*,*) 'f26_hgl(i) = ', f26_hgl(i)
     write(*,*) 'f36_hgl(i) = ', f36_hgl(i) 
     write(*,*) 'f17_hgl(i) = ', f17_hgl(i)
     write(*,*) 'f27_hgl(i) = ', f27_hgl(i)
     write(*,*) 'f37_hgl(i) = ', f37_hgl(i)          
     write(*,*) 'f18_hgl(i) = ', f18_hgl(i)
     write(*,*) 'f28_hgl(i) = ', f28_hgl(i)
     write(*,*) 'f38_hgl(i) = ', f38_hgl(i)
     end if 
       
      end do

      do i=1,nel
           
          if (hourglass == 0) then
          f11_hgl(i)=zero
          f21_hgl(i)=zero
          f31_hgl(i)=zero 
          f12_hgl(i)=zero
          f22_hgl(i)=zero
          f32_hgl(i)=zero 
          f13_hgl(i)=zero   
          f23_hgl(i)=zero        
          f33_hgl(i)=zero 
          f14_hgl(i)=zero
          f24_hgl(i)=zero
          f34_hgl(i)=zero 
          f15_hgl(i)=zero
          f25_hgl(i)=zero
          f35_hgl(i)=zero 
          f16_hgl(i)=zero  
          f26_hgl(i)=zero
          f36_hgl(i)=zero 
          f17_hgl(i)=zero
          f27_hgl(i)=zero
          f37_hgl(i)=zero       
          f18_hgl(i)=zero
          f28_hgl(i)=zero
          f38_hgl(i)=zero 

   !          write(*,*) 'ncycle =', nc_debug 
   !          pause
          endif

        f11(i) = f11(i) + f11_hgl(i)    
        f12(i) = f12(i) + f12_hgl(i)
        f13(i) = f13(i) + f13_hgl(i) + f14_hgl(i) ! f14                
        f15(i) = f15(i) + f15_hgl(i) 
        f16(i) = f16(i) + f16_hgl(i)
        f17(i) = f17(i) + f17_hgl(i) + f18_hgl(i) ! f18             
        f21(i) = f21(i) + f21_hgl(i)
        f22(i) = f22(i) + f22_hgl(i)
        f23(i) = f23(i) + f23_hgl(i) + f24_hgl(i) ! f24               
        f25(i) = f25(i) + f25_hgl(i) 
        f26(i) = f26(i) + f26_hgl(i)
        f27(i) = f27(i) + f27_hgl(i) + f28_hgl(i) ! f28
        f31(i) = f31(i) + f31_hgl(i)
        f32(i) = f32(i) + f32_hgl(i)
        f33(i) = f33(i) + f33_hgl(i) + f34_hgl(i) ! f34          
        f35(i) = f35(i) + f35_hgl(i)
        f36(i) = f36(i) + f36_hgl(i)
        f37(i) = f37(i) + f37_hgl(i) + f38_hgl(i) ! f38

      !f11(i) = f11(i) ! f14     
      !f12(i) = f12(i)
      !f13(i) = f13(i)                 
      !f15(i) = f15(i) ! f18
      !f16(i) = f16(i)
      !f17(i) = f17(i)
      !f21(i) = f21(i) ! f24
      !f22(i) = f22(i)
      !f23(i) = f23(i)             
      !f25(i) = f25(i) ! f28
      !f26(i) = f26(i)
      !f27(i) = f27(i)
      !f31(i) = f31(i) ! f34
      !f32(i) = f32(i)
      !f33(i) = f33(i)               
      !f35(i) = f35(i) ! f38
      !f36(i) = f36(i)
      !f37(i) = f37(i)       
            
    
      end do

      
   !c------------------------------------------------
   !c
   !      do i=1,nel
   !        f11(i) = f11(i) - g11(i)*nfhx1(i) - g12(i)*nfhx2(i) - g13(i)*nfhx3(i) - g14(i)*nfhx4(i)
   !        f12(i) = f12(i) - g21(i)*nfhx1(i) - g22(i)*nfhx2(i) - g23(i)*nfhx3(i) - g24(i)*nfhx4(i)
   !        f13(i) = f13(i) - g31(i)*nfhx1(i) - g32(i)*nfhx2(i) - g33(i)*nfhx3(i) - g34(i)*nfhx4(i)
   !     .                - g41(i)*nfhx1(i) - g42(i)*nfhx2(i) - g43(i)*nfhx3(i) - g44(i)*nfhx4(i) ! f14        
   !        f15(i) = f15(i) - g51(i)*nfhx1(i) - g52(i)*nfhx2(i) - g53(i)*nfhx3(i) - g54(i)*nfhx4(i)
   !        f16(i) = f16(i) - g61(i)*nfhx1(i) - g62(i)*nfhx2(i) - g63(i)*nfhx3(i) - g64(i)*nfhx4(i)
   !        f17(i) = f17(i) - g71(i)*nfhx1(i) - g72(i)*nfhx2(i) - g73(i)*nfhx3(i) - g74(i)*nfhx4(i)
   !     .                - g81(i)*nfhx1(i) - g82(i)*nfhx2(i) - g83(i)*nfhx3(i) - g84(i)*nfhx4(i) ! f18



   !        f21(i) = f21(i) - g11(i)*nfhy1(i) - g12(i)*nfhy2(i) - g13(i)*nfhy3(i) - g14(i)*nfhy4(i)
   !        f22(i) = f22(i) - g21(i)*nfhy1(i) - g22(i)*nfhy2(i) - g23(i)*nfhy3(i) - g24(i)*nfhy4(i)
   !        f23(i) = f23(i) - g31(i)*nfhy1(i) - g32(i)*nfhy2(i) - g33(i)*nfhy3(i) - g34(i)*nfhy4(i)
   !     .                - g41(i)*nfhy1(i) - g42(i)*nfhy2(i) - g43(i)*nfhy3(i) - g44(i)*nfhy4(i) ! f24
   !        f25(i) = f25(i) - g51(i)*nfhy1(i) - g52(i)*nfhy2(i) - g53(i)*nfhy3(i) - g54(i)*nfhy4(i)
   !        f26(i) = f26(i) - g61(i)*nfhy1(i) - g62(i)*nfhy2(i) - g63(i)*nfhy3(i) - g64(i)*nfhy4(i)
   !        f27(i) = f27(i) - g71(i)*nfhy1(i) - g72(i)*nfhy2(i) - g73(i)*nfhy3(i) - g74(i)*nfhy4(i)
   !     .                - g81(i)*nfhy1(i) - g82(i)*nfhy2(i) - g83(i)*nfhy3(i) - g84(i)*nfhy4(i) ! f28!



   !        f31(i) = f31(i) - g11(i)*nfhz1(i) - g12(i)*nfhz2(i) - g13(i)*nfhz3(i) - g14(i)*nfhz4(i)
   !        f32(i) = f32(i) - g21(i)*nfhz1(i) - g22(i)*nfhz2(i) - g23(i)*nfhz3(i) - g24(i)*nfhz4(i)
   !        f33(i) = f33(i) - g31(i)*nfhz1(i) - g32(i)*nfhz2(i) - g33(i)*nfhz3(i) - g34(i)*nfhz4(i)
   !     .                - g41(i)*nfhz1(i) - g42(i)*nfhz2(i) - g43(i)*nfhz3(i) - g44(i)*nfhz4(i) ! f34
   !        f35(i) = f35(i) - g51(i)*nfhz1(i) - g52(i)*nfhz2(i) - g53(i)*nfhz3(i) - g54(i)*nfhz4(i)
   !        f36(i) = f36(i) - g61(i)*nfhz1(i) - g62(i)*nfhz2(i) - g63(i)*nfhz3(i) - g64(i)*nfhz4(i)
   !        f37(i) = f37(i) - g71(i)*nfhz1(i) - g72(i)*nfhz2(i) - g73(i)*nfhz3(i) - g74(i)*nfhz4(i)
   !     .                - g81(i)*nfhz1(i) - g82(i)*nfhz2(i) - g83(i)*nfhz3(i) - g84(i)*nfhz4(i) ! f38
    
   !      end do
      do i=1,nel
        eint(i) = eint(i) + dt1*( &
             nfhz1(i)*hgz1(i) + nfhz2(i)*hgz2(i) ) &
           /max(em20,vol0(i)) 
      end do
   !c-----------
      return
   !c-----------
      end subroutine s6hour3_2
   end module s6hour3_2_mod