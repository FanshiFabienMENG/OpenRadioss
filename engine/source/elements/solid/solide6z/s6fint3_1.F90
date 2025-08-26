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
!! \brief Internal force computation for 6-node solid elements
!! \details Computes internal forces for solid elements using stress and strain data
!!          Includes multi-layer averaging and various material properties

      module s6fint3_2_mod
         contains
         !! \brief Internal force computation for 6-node solid elements
         !! \details Computes internal forces for solid elements using stress and strain data
         !!          Includes multi-layer averaging and various material properties
         subroutine s6fint3_2( &
            sig,     px1,     px2,     px3, &
            px4,     py1,     py2,     py3, &
            py4,     pz1,     pz2,     pz3, &
            pz4,     px1h,    px2h,    px3h, &
            py1h,    py2h,    py3h,    pz1h, &
            pz2h,    pz3h,    ji33,    b1x, &
            b1y,     b2y,     b2x,     b1122, &
            b1221,   b2212,   b1121,   b1xh, &
            b1yh,    b2xh,    b2yh,    b1122h, &
            b1221h,  b2212h,  b1121h,  f11, &
            f21,     f31,     f12,     f22, &
            f32,     f13,     f23,     f33, &
            f14,     f24,     f34,     f15, &
            f25,     f35,     f16,     f26, &
            f36,     vol,     qvis,    eint, &
            rho,     q,       epla,    epsd, &
            epsdm,   sigm,    eintm,   rhom, &
            qm,      eplasm,  zi, &
            volg,    off,     nu,      vol0, &
            vol0g,   g_pla,   g_epsd,  nel, &
            svis,    nlay, &
            px5,     py5,     pz5, &
            px6,     py6,     pz6)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
  use debug_mod, only : nc_debug
  use precision_mod, only : wp
  use mvsiz_mod, only : mvsiz
  use constant_mod, only : one, half, three

! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none

! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  integer,                           intent(in)    :: g_pla           !< Plastic flag
  integer,                           intent(in)    :: g_epsd          !< Strain rate flag
  integer,                           intent(in)    :: nel             !< Number of elements
  integer,                           intent(in)    :: nlay            !< Number of layers
  real(kind=wp),                     intent(in)    :: zi              !< Z coordinate factor
  real(kind=wp), dimension(nel,6),   intent(in)    :: sig             !< Stress tensor
  real(kind=wp), dimension(*),       intent(in)    :: px1, px2, px3, px4, px5, px6  !< X coordinates
  real(kind=wp), dimension(*),       intent(in)    :: py1, py2, py3, py4, py5, py6  !< Y coordinates
  real(kind=wp), dimension(*),       intent(in)    :: pz1, pz2, pz3, pz4, pz5, pz6  !< Z coordinates
  real(kind=wp), dimension(*),       intent(in)    :: px1h, px2h, px3h  !< X coordinates (hourglass)
  real(kind=wp), dimension(*),       intent(in)    :: py1h, py2h, py3h  !< Y coordinates (hourglass)
  real(kind=wp), dimension(*),       intent(in)    :: pz1h, pz2h, pz3h  !< Z coordinates (hourglass)
  real(kind=wp), dimension(*),       intent(in)    :: ji33             !< Jacobian component
  real(kind=wp), dimension(mvsiz,2), intent(in)    :: b1x, b1y         !< B matrix components
  real(kind=wp), dimension(mvsiz,2), intent(in)    :: b2x, b2y         !< B matrix components
  real(kind=wp), dimension(*),       intent(in)    :: b1122, b1221, b2212, b1121  !< B matrix terms
  real(kind=wp), dimension(mvsiz,2), intent(in)    :: b1xh, b1yh       !< B matrix hourglass components
  real(kind=wp), dimension(mvsiz,2), intent(in)    :: b2xh, b2yh       !< B matrix hourglass components
  real(kind=wp), dimension(*),       intent(in)    :: b1122h, b1221h, b2212h, b1121h  !< B matrix hourglass terms
  real(kind=wp), dimension(*),       intent(in)    :: vol              !< Element volume
  real(kind=wp), dimension(*),       intent(in)    :: qvis             !< Viscous pressure
  real(kind=wp), dimension(*),       intent(in)    :: eint             !< Internal energy
  real(kind=wp), dimension(*),       intent(in)    :: rho              !< Density
  real(kind=wp), dimension(*),       intent(in)    :: q                !< Artificial viscosity
  real(kind=wp), dimension(*),       intent(in)    :: epla             !< Plastic strain
  real(kind=wp), dimension(*),       intent(in)    :: epsd             !< Strain rate
  real(kind=wp), dimension(*),       intent(in)    :: volg             !< Global volume
  real(kind=wp), dimension(*),       intent(in)    :: off              !< Element off flag
  real(kind=wp), dimension(*),       intent(in)    :: nu               !< Poisson's ratio
  real(kind=wp), dimension(*),       intent(in)    :: vol0             !< Initial volume
  real(kind=wp), dimension(*),       intent(in)    :: vol0g            !< Initial global volume
  real(kind=wp), dimension(mvsiz,6), intent(inout) :: svis             !< Viscous stress
  real(kind=wp), dimension(*),       intent(inout) :: f11, f21, f31    !< Forces on node 1
  real(kind=wp), dimension(*),       intent(inout) :: f12, f22, f32    !< Forces on node 2
  real(kind=wp), dimension(*),       intent(inout) :: f13, f23, f33    !< Forces on node 3
  real(kind=wp), dimension(*),       intent(inout) :: f14, f24, f34    !< Forces on node 4
  real(kind=wp), dimension(*),       intent(inout) :: f15, f25, f35    !< Forces on node 5
  real(kind=wp), dimension(*),       intent(inout) :: f16, f26, f36    !< Forces on node 6
  real(kind=wp), dimension(*),       intent(inout) :: eplasm           !< Mean plastic strain
  real(kind=wp), dimension(nel,6),   intent(inout) :: sigm             !< Mean stress
  real(kind=wp), dimension(*),       intent(inout) :: eintm            !< Mean internal energy
  real(kind=wp), dimension(*),       intent(inout) :: rhom             !< Mean density
  real(kind=wp), dimension(*),       intent(inout) :: qm               !< Mean artificial viscosity
  real(kind=wp), dimension(*),       intent(inout) :: epsdm            !< Mean strain rate

! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
  integer :: i                                                          ! Loop counter
  real(kind=wp), dimension(mvsiz) :: s1, s2, s3, s4, s5, s6           ! Stress components
  real(kind=wp), dimension(mvsiz) :: s1_old, s2_old, s3_old           ! Original stress components
  real(kind=wp), dimension(mvsiz) :: s4_old, s5_old, s6_old           ! Original stress components
  real(kind=wp), dimension(mvsiz) :: fac                              ! Volume factor
  real(kind=wp) :: fint, fintx, finty, fintz                          ! Internal force components
  real(kind=wp) :: fxc, fyc, finsx, finsy, finsz                      ! Force components
  real(kind=wp) :: nu1                                                 ! Modified Poisson's ratio

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
   do i=1,nel
!       write(*,*) 'ncycle =', nc_debug
!       write(*,*) 'vol(i) = ', vol(i)
               s1(i)=(sig(i,1)+svis(i,1)-qvis(i))*vol(i)
               s2(i)=(sig(i,2)+svis(i,2)-qvis(i))*vol(i)
               s3(i)=(sig(i,3)+svis(i,3)-qvis(i))*vol(i)
               s4(i)=(sig(i,4)+svis(i,4))*vol(i)
               s5(i)=(sig(i,5)+svis(i,5))*vol(i)
               s6(i)=(sig(i,6)+svis(i,6))*vol(i)
!       write(*,*) 's1(i)', s1(i)
!       write(*,*) 's2(i)', s2(i)
!       write(*,*) 's3(i)', s3(i)
!       write(*,*) 's4(i)', s4(i)
!       write(*,*) 's5(i)', s5(i)
!       write(*,*) 's6(i)', s6(i)
             end do
!c                                                                     12
!c  -------constant part---------
               do i=1,nel
                fint=s1(i)*px1(i)+s4(i)*py1(i)
                fxc = ji33(i)*s6(i)
                fyc = ji33(i)*s5(i)
                fintx=s1(i)*px4(i)+s4(i)*py4(i) + three*fxc
                finsx=s6(i)*(b1x(i,1)-b1x(i,2))-s5(i)*(b2x(i,1)-b2x(i,2))-fxc
                f11(i)=f11(i)-fint+fintx+finsx
                f14(i)=f14(i)-fint-fintx-finsx
                fint=s2(i)*py1(i)+s4(i)*px1(i)
                finty=s2(i)*py4(i)+s4(i)*px4(i) + three*fyc
                finsy=s6(i)*(b1y(i,1)-b1y(i,2))-s5(i)*(b2y(i,1)-b2y(i,2))-fyc
                f21(i)=f21(i)-fint+finty+finsy
                f24(i)=f24(i)-fint-finty-finsy
                fint=s3(i)*pz1(i)+half*(s6(i)*px1(i)+s5(i)*py1(i))
                fintz=s3(i)*pz4(i)
                f31(i)=f31(i)-fint+fintz
                f34(i)=f34(i)-fint-fintz
!c
                fint=s1(i)*px2(i)+s4(i)*py2(i)
                finsx=s6(i)*(b1221(i)+b1x(i,2))-s5(i)*(b1121(i)+b2x(i,2))
                f12(i)=f12(i)-fint+fintx+finsx
                f15(i)=f15(i)-fint-fintx-finsx
                fint=s2(i)*py2(i)+s4(i)*px2(i)
                finsy=s6(i)*(b2212(i)+b1y(i,2))-s5(i)*(b1122(i)+b2y(i,2))
                f22(i)=f22(i)-fint+finty+finsy
                f25(i)=f25(i)-fint-finty-finsy
                fint=s3(i)*pz2(i)+(s6(i)*px2(i)+s5(i)*py2(i))*half
                f32(i)=f32(i)-fint+fintz
                f35(i)=f35(i)-fint-fintz
!c
                fint=s1(i)*px3(i)+s4(i)*py3(i)
                finsx=-s6(i)*(b1122(i)+b1x(i,1))+s5(i)*(b1121(i)+b2x(i,1))
                f13(i)=f13(i)-fint+fintx+finsx
                f16(i)=f16(i)-fint-fintx-finsx
                fint=s2(i)*py3(i)+s4(i)*px3(i)
                finsy=-s6(i)*(b2212(i)+b1y(i,1))+s5(i)*(b1221(i)+b2y(i,1))
                f23(i)=f23(i)-fint+finty+finsy
                f26(i)=f26(i)-fint-finty-finsy
                fint=s3(i)*pz3(i)+(s6(i)*px3(i)+s5(i)*py3(i))*half
                f33(i)=f33(i)-fint+fintz
                f36(i)=f36(i)-fint-fintz
               end do
!c   --- non constante part------------
!c                                                                     12
             do i=1,nel
               s1_old(i) = s1(i)
               s2_old(i) = s2(i)
               s3_old(i) = s3(i)
               s4_old(i) = s4(i)
               s5_old(i) = s5(i)
               s6_old(i) = s6(i)
               s1(i) = zi*s1(i)
               s2(i) = zi*s2(i)
               s3(i) = zi*s3(i)
               s4(i) = zi*s4(i)
               s5(i) = zi*s5(i)
               s6(i) = zi*s6(i)
             end do
    
                  do i=1,nel
                  nu1 = nu(i)/(one - nu(i))
                  fxc =s1(i)-(s2(i)+s3(i))*nu(i)
                  fyc =s2(i)-(s1(i)+s3(i))*nu(i)
                  finsx = s1(i)*nu1
                  finsy = s2(i)*nu1
                  finsz = s3(i)*nu1
                  fint=fxc*px1h(i)+s4(i)*py1h(i)
                  fintx=(s1(i)-finsz)*px1(i)+s4(i)*py1(i)
                  fintx=fintx+s6(i)*(b1xh(i,1)-b1xh(i,2)) &
                            -s5(i)*(b2xh(i,1)-b2xh(i,2))
                  f11(i)=f11(i)-fint+fintx
                  f14(i)=f14(i)-fint-fintx
                  fint=(s2(i)-finsz)*py1h(i)+s4(i)*px1h(i)
                  finty=fyc*py1(i)+s4(i)*px1(i)
                  finty=finty+s6(i)*(b1yh(i,1)-b1yh(i,2)) &
                            -s5(i)*(b2yh(i,1)-b2yh(i,2))
                  f21(i)=f21(i)-fint+finty
                  f24(i)=f24(i)-fint-finty
                  fint=(s3(i)-finsy)*pz1h(i)+ &
                        (s6(i)*px1h(i)+s5(i)*py1h(i))*half
                  fintz=(s3(i)-finsx)*pz1(i)
                  f31(i)=f31(i)-fint+fintz
                  f34(i)=f34(i)-fint-fintz
!c
                  fint=fxc*px2h(i)+s4(i)*py2h(i)
                  fintx=(s1(i)-finsz)*px2(i)+s4(i)*py2(i)
                  fintx=fintx+ &
                        s6(i)*(b1221h(i)+b1xh(i,2))-s5(i)*(b1121h(i)+b2xh(i,2))
                  f12(i)=f12(i)-fint+fintx
                  f15(i)=f15(i)-fint-fintx
                  fint=(s2(i)-finsz)*py2h(i)+s4(i)*px2h(i)
                  finty=fyc*py2(i)+s4(i)*px2(i)
                  finty=finty+ &
                        s6(i)*(b2212h(i)+b1yh(i,2))-s5(i)*(b1122h(i)+b2yh(i,2))
                  f22(i)=f22(i)-fint+finty
                  f25(i)=f25(i)-fint-finty
                  fint=(s3(i)-finsy)*pz2h(i)+ &
                        (s6(i)*px2h(i)+s5(i)*py2h(i))*half
                  fintz=(s3(i)-finsx)*pz2(i)
                  f32(i)=f32(i)-fint+fintz
                  f35(i)=f35(i)-fint-fintz
!c
                  fint=fxc*px3h(i)+s4(i)*py3h(i)
                  fintx=(s1(i)-finsz)*px3(i)+s4(i)*py3(i)
                  fintx=fintx &
                        -s6(i)*(b1122h(i)+b1xh(i,1))+s5(i)*(b1121h(i)+b2xh(i,1))
                  f13(i)=f13(i)-fint+fintx
                  f16(i)=f16(i)-fint-fintx
                  fint=(s2(i)-finsz)*py3h(i)+s4(i)*px3h(i)
                  finty=fyc*py3(i)+s4(i)*px3(i)
                  finty=finty &
                        -s6(i)*(b2212h(i)+b1yh(i,1))+s5(i)*(b1221h(i)+b2yh(i,1))
                  f23(i)=f23(i)-fint+finty
                  f26(i)=f26(i)-fint-finty
                  fint=(s3(i)-finsy)*pz3h(i)+ &
                        (s6(i)*px3h(i)+s5(i)*py3h(i))*half
                  fintz=(s3(i)-finsx)*pz3(i)
                  f33(i)=f33(i)-fint+fintz
                  f36(i)=f36(i)-fint-fintz
                  end do
    
    
    
    
                do i=1,nel
    
!   f(xyz)( n 1 2 3 4 5 6)//todo: a regerder coordonne de chaque node; function forme 1, 
!            write(*,*) 'px1', px1(i)
!            write(*,*) 'py1', py1(i)
!            write(*,*) 'pz1', pz1(i)
!    
!            write(*,*) 'px2', px2(i)
!            write(*,*) 'py2', py2(i)
!            write(*,*) 'pz2', pz2(i)
!    
!            write(*,*) 'px3', px3(i)
!            write(*,*) 'py3', py3(i)
!            write(*,*) 'pz3', pz3(i)
!    
!            write(*,*) 'px4', px4(i)
!            write(*,*) 'py4', py4(i)
!            write(*,*) 'pz4', pz4(i)
!            write(*,*) 'px5', px5(i)
!            write(*,*) 'py5', py5(i)
!            write(*,*) 'pz5', pz5(i)
!    
!            write(*,*) 'px6', px6(i)
!            write(*,*) 'py6', py6(i)
!            write(*,*) 'pz6', pz6(i)
                     
                      f11(i) = 0.
                      f21(i) = 0.
                      f31(i) = 0. 
             
                      f12(i) = 0.
                      f22(i) = 0.
                      f32(i) = 0. 
             
                      f13(i) = 0.
                      f23(i) = 0.
                      f33(i) = 0. 
             
                      f14(i) = 0.
                      f24(i) = 0.
                      f34(i) = 0. 
             
                      f15(i) = 0.
                      f25(i) = 0.
                      f35(i) = 0. 
             
                      f16(i) = 0.
                      f26(i) = 0.
                      f36(i) = 0. 
    
!            write(*,*) 'f11(i) = ', f11(i)
!            write(*,*) 'f21(i) = ', f21(i)
!            write(*,*) 'f31(i) = ', f31(i) 
!  
!            write(*,*) 'f12(i) = ', f12(i)
!            write(*,*) 'f22(i) = ', f22(i)
!            write(*,*) 'f32(i) = ', f32(i) 
!  
!            write(*,*) 'f13(i) = ', f13(i)
!            write(*,*) 'f23(i) = ', f23(i)
!            write(*,*) 'f33(i) = ', f33(i) 
!  
!            write(*,*) 'f14(i) = ', f14(i)
!            write(*,*) 'f24(i) = ', f24(i)
!            write(*,*) 'f34(i) = ', f34(i) 
!  
!            write(*,*) 'f15(i) = ', f15(i)
!            write(*,*) 'f25(i) = ', f25(i)
!            write(*,*) 'f35(i) = ', f35(i) 
!  
!            write(*,*) 'f16(i) = ', f16(i)
!            write(*,*) 'f26(i) = ', f26(i)
!            write(*,*) 'f36(i) = ', f36(i) 
    
          
                   f11(i)=f11(i)-(s1_old(i)*px1(i)+s4_old(i)*py1(i)+s6_old(i)*pz1(i))
                   f21(i)=f21(i)-(s2_old(i)*py1(i)+s5_old(i)*pz1(i)+s4_old(i)*px1(i))
                   f31(i)=f31(i)-(s3_old(i)*pz1(i)+s6_old(i)*px1(i)+s5_old(i)*py1(i))
    
                   !!!!!!!!!!!!!!!!!!!
                   f12(i)=f12(i)-(s1_old(i)*px2(i)+s4_old(i)*py2(i)+s6_old(i)*pz2(i))
                   f22(i)=f22(i)-(s2_old(i)*py2(i)+s5_old(i)*pz2(i)+s4_old(i)*px2(i))
                   f32(i)=f32(i)-(s3_old(i)*pz2(i)+s6_old(i)*px2(i)+s5_old(i)*py2(i))
    
                   !!!!!!!!!!!!!!!!!!!
                   f13(i)=f13(i)-(s1_old(i)*px3(i)+s4_old(i)*py3(i)+s6_old(i)*pz3(i))
                   f23(i)=f23(i)-(s2_old(i)*py3(i)+s5_old(i)*pz3(i)+s4_old(i)*px3(i))
                   f33(i)=f33(i)-(s3_old(i)*pz3(i)+s6_old(i)*px3(i)+s5_old(i)*py3(i))
    
                   !!!!!!!!!!!!!!!!!!!          
                   f14(i)=f14(i)-(s1_old(i)*px4(i)+s4_old(i)*py4(i)+s6_old(i)*pz4(i))
                   f24(i)=f24(i)-(s2_old(i)*py4(i)+s5_old(i)*pz4(i)+s4_old(i)*px4(i))
                   f34(i)=f34(i)-(s3_old(i)*pz4(i)+s6_old(i)*px4(i)+s5_old(i)*py4(i))
    
                   !!!!!!!!!!!!!!!!!!!          
                   f15(i)=f15(i)-(s1_old(i)*px5(i)+s4_old(i)*py5(i)+s6_old(i)*pz5(i))
                   f25(i)=f25(i)-(s2_old(i)*py5(i)+s5_old(i)*pz5(i)+s4_old(i)*px5(i))
                   f35(i)=f35(i)-(s3_old(i)*pz5(i)+s6_old(i)*px5(i)+s5_old(i)*py5(i))
    
                   !!!!!!!!!!!!!!!!!!!          
                   f16(i)=f16(i)-(s1_old(i)*px6(i)+s4_old(i)*py6(i)+s6_old(i)*pz6(i))
                   f26(i)=f26(i)-(s2_old(i)*py6(i)+s5_old(i)*pz6(i)+s4_old(i)*px6(i))
                   f36(i)=f36(i)-(s3_old(i)*pz6(i)+s6_old(i)*px6(i)+s5_old(i)*py6(i))
             
!         write(*,*) 'new for,ulation'
!          write(*,*) 'f11(i) = ', f11(i)
!          write(*,*) 'f21(i) = ', f21(i)
!          write(*,*) 'f31(i) = ', f31(i) !

!          write(*,*) 'f12(i) = ', f12(i)
!          write(*,*) 'f22(i) = ', f22(i)
!          write(*,*) 'f32(i) = ', f32(i) !

!          write(*,*) 'f13(i) = ', f13(i)
!          write(*,*) 'f23(i) = ', f23(i)
!          write(*,*) 'f33(i) = ', f33(i) !

!          write(*,*) 'f14(i) = ', f14(i)
!          write(*,*) 'f24(i) = ', f24(i)
!          write(*,*) 'f34(i) = ', f34(i) !

!          write(*,*) 'f15(i) = ', f15(i)
!          write(*,*) 'f25(i) = ', f25(i)
!          write(*,*) 'f35(i) = ', f35(i) !

!          write(*,*) 'f16(i) = ', f16(i)
!          write(*,*) 'f26(i) = ', f26(i)
!          write(*,*) 'f36(i) = ', f36(i) 
    
!    fx(i,n)=fx(i,n)-(s1(i)*px(i,n)+s4(i)*py(i,n)+s6(i)*pz(i,n))
!    fy(i,n)=fy(i,n)-(s2(i)*py(i,n)+s5(i)*pz(i,n)+s4(i)*px(i,n))
!    fz(i,n)=fz(i,n)-(s3(i)*pz(i,n)+s6(i)*px(i,n)+s5(i)*py(i,n))
    
    
    
! f13(i)=f13(i)-(s1(i)*px3(i)+s4(i)*py3(i)+s6(i)*pz3(i))
! 
! f23(i)=f23(i)-(s2(i)*py3(i)+s5(i)*pz3(i)+s4(i)*px3(i))
!
!f32(i)=f32(i)-(s3(i)*pz2(i)+s6(i)*px2(i)+s5(i)*py2(i))
             end do
!c----------------------------------------------    /
!c----------------------------------------------    /
!c   - post-traitement-valeur moyenne au sens a'=(_/  a dv ) /v
             if (nlay > 1) then 
             do i=1,nel
                  fac(i) = off(i)*vol(i)/volg(i)
                  sigm(i,1) = sigm(i,1) + fac(i) * sig(i,1)
                  sigm(i,2) = sigm(i,2) + fac(i) * sig(i,2)
                  sigm(i,3) = sigm(i,3) + fac(i) * sig(i,3)
                  sigm(i,4) = sigm(i,4) + fac(i) * sig(i,4)
                  sigm(i,5) = sigm(i,5) + fac(i) * sig(i,5)
                  sigm(i,6) = sigm(i,6) + fac(i) * sig(i,6)
                  rhom(i)   = rhom(i)   + fac(i) * rho(i)
                  eintm(i)  = eintm(i)  + eint(i)*vol0(i)/vol0g(i)
                  qm(i)     = qm(i)     + fac(i) * q(i)
             end do
             if (g_pla > 0) then
                do i=1,nel
                   eplasm(i)  = eplasm(i)  + fac(i) * epla(i)
                end do
             end if
             if (g_epsd > 0) then
                do i=1,nel
                   epsdm(i) = epsdm(i) + fac(i) * epsd(i)
                end do
             end if
             end if
!      pause

! ----------------------------------------------------------------------------------------------------------------------
end subroutine s6fint3_2
end module s6fint3_2_mod