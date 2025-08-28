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
!! \brief S6RCOOR3_2 - Coordinate transformation for 6-node solid elements
!! \details Transforms nodal coordinates and velocities to corotational frame
!!          for 6-node solid elements (pentahedral elements)


      module s6rcoor3_2_mod
            contains
subroutine s6rcoor3_2(                         &
                       x, ixs, v, gama0, gama, &
                       x1, x2, x3, x4, x5, x6, &
                       y1, y2, y3, y4, y5, y6, &
                       z1, z2, z3, z4, z5, z6, &
                       vx1, vx2, vx3, vx4, vx5, vx6, &
                       vy1, vy2, vy3, vy4, vy5, vy6, &
                       vz1, vz2, vz3, vz4, vz5, vz6, &
                       vd2, vis, offg, off, sav, rho, rhoo, &
                       r11, r12, r13, r21, r22, r23, r31, r32, r33, &
                       nc1, nc2, nc3, nc4, nc5, nc6, ngl, mxt, ngeo, &
                       ioutprt, vgax, vgay, vgaz, vga2, di, &
                       nel, xgax, xgay, xgaz, xgxa2,xgya2, xgza2, &
                       xgxya, xgyza, xgzxa, iparg, gama_r, &
                       NIXS,irep,ismstr,isorth,jlag)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
  use precision_mod, only : WP
  !use mvsiz_mod, only : MVSIZ
  use constant_mod, only : ZERO, ONE, TWO, ONE_OVER_6, EM20

! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none

!C-----------------------------------------------
!C   I m p l i c i t   T y p e s
!C-----------------------------------------------
!#include      "implicit_f.inc"
!C-----------------------------------------------
!C   G l o b a l   P a r a m e t e r s
!C-----------------------------------------------
!#include      "mvsiz_p.inc"
!C-----------------------------------------------
!C   C o m m o n   B l o c k s
!C-----------------------------------------------
!#include      "vect01_c.inc"
!C-----------------------------------------------
!C   D u m m y   A r g u m e n t s
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  
      integer,             intent(in)    :: NIXS                      !< Number of columns in connectivity array
      integer,             intent(in)    :: irep                      !< Iteration flag for coordinate system
      integer,             intent(in)    :: ismstr                    !< Small strain flag
      integer,             intent(in)    :: isorth                    !< Orthotropic material flag  
      integer,             intent(in)    :: jlag                      !< Lagrangian flag

      integer,             intent(in)    :: nel                    !< Number of elements
      real(kind=WP),       intent(in)    :: x(3,*)                !< Nodal coordinates
      integer,             intent(in)    :: ixs(NIXS,*)            !< Element connectivity
      real(kind=WP),       intent(in)    :: v(3,*)                !< Nodal velocities
      real(kind=WP),       intent(inout) :: gama(nel,6)           !< Material orientation matrix
      real(kind=WP),       intent(in)    :: gama0(nel,6)          !< Initial material orientation
      real(kind=WP),       intent(out)   :: x1(nel)               !< Local x-coordinates node 1
      real(kind=WP),       intent(out)   :: x2(nel)               !< Local x-coordinates node 2
      real(kind=WP),       intent(out)   :: x3(nel)               !< Local x-coordinates node 3
      real(kind=WP),       intent(out)   :: x4(nel)               !< Local x-coordinates node 4
      real(kind=WP),       intent(out)   :: x5(nel)               !< Local x-coordinates node 5
      real(kind=WP),       intent(out)   :: x6(nel)               !< Local x-coordinates node 6
      real(kind=WP),       intent(out)   :: y1(nel)               !< Local y-coordinates node 1
      real(kind=WP),       intent(out)   :: y2(nel)               !< Local y-coordinates node 2
      real(kind=WP),       intent(out)   :: y3(nel)               !< Local y-coordinates node 3
      real(kind=WP),       intent(out)   :: y4(nel)               !< Local y-coordinates node 4
      real(kind=WP),       intent(out)   :: y5(nel)               !< Local y-coordinates node 5
      real(kind=WP),       intent(out)   :: y6(nel)               !< Local y-coordinates node 6
      real(kind=WP),       intent(out)   :: z1(nel)               !< Local z-coordinates node 1
      real(kind=WP),       intent(out)   :: z2(nel)               !< Local z-coordinates node 2
      real(kind=WP),       intent(out)   :: z3(nel)               !< Local z-coordinates node 3
      real(kind=WP),       intent(out)   :: z4(nel)               !< Local z-coordinates node 4
      real(kind=WP),       intent(out)   :: z5(nel)               !< Local z-coordinates node 5
      real(kind=WP),       intent(out)   :: z6(nel)               !< Local z-coordinates node 6
      real(kind=WP),       intent(out)   :: vx1(nel)              !< Local x-velocities node 1
      real(kind=WP),       intent(out)   :: vx2(nel)              !< Local x-velocities node 2
      real(kind=WP),       intent(out)   :: vx3(nel)              !< Local x-velocities node 3
      real(kind=WP),       intent(out)   :: vx4(nel)              !< Local x-velocities node 4
      real(kind=WP),       intent(out)   :: vx5(nel)              !< Local x-velocities node 5
      real(kind=WP),       intent(out)   :: vx6(nel)              !< Local x-velocities node 6
      real(kind=WP),       intent(out)   :: vy1(nel)              !< Local y-velocities node 1
      real(kind=WP),       intent(out)   :: vy2(nel)              !< Local y-velocities node 2
      real(kind=WP),       intent(out)   :: vy3(nel)              !< Local y-velocities node 3
      real(kind=WP),       intent(out)   :: vy4(nel)              !< Local y-velocities node 4
      real(kind=WP),       intent(out)   :: vy5(nel)              !< Local y-velocities node 5
      real(kind=WP),       intent(out)   :: vy6(nel)              !< Local y-velocities node 6
      real(kind=WP),       intent(out)   :: vz1(nel)              !< Local z-velocities node 1
      real(kind=WP),       intent(out)   :: vz2(nel)              !< Local z-velocities node 2
      real(kind=WP),       intent(out)   :: vz3(nel)              !< Local z-velocities node 3
      real(kind=WP),       intent(out)   :: vz4(nel)              !< Local z-velocities node 4
      real(kind=WP),       intent(out)   :: vz5(nel)              !< Local z-velocities node 5
      real(kind=WP),       intent(out)   :: vz6(nel)              !< Local z-velocities node 6
      real(kind=WP),       intent(out)   :: vd2(nel)              !< Viscous damping coefficient
      real(kind=WP),       intent(inout) :: vis(nel)              !< Viscosity
      real(kind=WP),       intent(in)    :: offg(nel)             !< Global offset flag
      real(kind=WP),       intent(out)   :: off(nel)              !< Local offset flag
      real(kind=WP),       intent(in)    :: sav(nel,15)           !< Saved variables array (nel x 15)
      real(kind=WP),       intent(in)    :: rho(nel)              !< Density
      real(kind=WP),       intent(out)   :: rhoo(nel)             !< Initial density
      real(kind=WP),       intent(out)   :: r11(nel)              !< Rotation matrix component (1,1)
      real(kind=WP),       intent(out)   :: r12(nel)              !< Rotation matrix component (1,2)
      real(kind=WP),       intent(out)   :: r13(nel)              !< Rotation matrix component (1,3)
      real(kind=WP),       intent(out)   :: r21(nel)              !< Rotation matrix component (2,1)
      real(kind=WP),       intent(out)   :: r22(nel)              !< Rotation matrix component (2,2)
      real(kind=WP),       intent(out)   :: r23(nel)              !< Rotation matrix component (2,3)
      real(kind=WP),       intent(out)   :: r31(nel)              !< Rotation matrix component (3,1)
      real(kind=WP),       intent(out)   :: r32(nel)              !< Rotation matrix component (3,2)
      real(kind=WP),       intent(out)   :: r33(nel)              !< Rotation matrix component (3,3)
      integer,             intent(out)   :: nc1(nel)              !< Node connectivity index 1
      integer,             intent(out)   :: nc2(nel)              !< Node connectivity index 2
      integer,             intent(out)   :: nc3(nel)              !< Node connectivity index 3
      integer,             intent(out)   :: nc4(nel)              !< Node connectivity index 4
      integer,             intent(out)   :: nc5(nel)              !< Node connectivity index 5
      integer,             intent(out)   :: nc6(nel)              !< Node connectivity index 6
      integer,             intent(out)   :: ngl(nel)              !< Global element number
      integer,             intent(out)   :: mxt(nel)              !< Material type
      integer,             intent(out)   :: ngeo(nel)             !< Geometry number
      integer,             intent(in)    :: ioutprt               !< Output print flag
      real(kind=WP),       intent(out)   :: vgax(nel)             !< Global velocity x-component
      real(kind=WP),       intent(out)   :: vgay(nel)             !< Global velocity y-component
      real(kind=WP),       intent(out)   :: vgaz(nel)             !< Global velocity z-component
      real(kind=WP),       intent(out)   :: vga2(nel)             !< Velocity squared
      real(kind=WP),       intent(out)   :: di(nel,6)             !< Inertia tensor
      real(kind=WP),       intent(out)   :: xgax(nel)             !< Global coordinate x-component
      real(kind=WP),       intent(out)   :: xgay(nel)             !< Global coordinate y-component
      real(kind=WP),       intent(out)   :: xgaz(nel)             !< Global coordinate z-component
      real(kind=WP),       intent(out)   :: xgxa2(nel)            !< X-coordinate squared term
      real(kind=WP),       intent(out)   :: xgya2(nel)            !< Y-coordinate squared term
      real(kind=WP),       intent(out)   :: xgza2(nel)            !< Z-coordinate squared term
      real(kind=WP),       intent(out)   :: xgxya(nel)            !< XY cross product term
      real(kind=WP),       intent(out)   :: xgyza(nel)            !< YZ cross product term
      real(kind=WP),       intent(out)   :: xgzxa(nel)            !< ZX cross product term
      integer,             intent(in)    :: iparg(*)              !< Integer parameters
      real(kind=WP),       intent(out)   :: gama_r(nel,6)         !< Rotation matrix storage
!CMasParINCLUDE 'scoor3.intmap.inc'
!C-----------------------------------------------
!C   L o c a l   V a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i                                                     ! Loop counter
      integer :: write_flag                                            ! Debug write flag
      real(kind=WP) :: dt05                                           ! Time step parameter
      real(kind=WP), dimension(nel) :: g11, g12, g13                 ! Geometry matrix components row 1
      real(kind=WP), dimension(nel) :: g21, g22, g23                 ! Geometry matrix components row 2
      real(kind=WP), dimension(nel) :: g31, g32, g33                 ! Geometry matrix components row 3
      real(kind=WP), dimension(nel) :: t11, t12, t13                 ! Transformation matrix components row 1
      real(kind=WP), dimension(nel) :: t21, t22, t23                 ! Transformation matrix components row 2
      real(kind=WP), dimension(nel) :: t31, t32, t33                 ! Transformation matrix components row 3
      real(kind=WP) :: xl, yl, zl                                    ! Local centroid coordinates
      real(kind=WP) :: xx, yy, zz, xy, xz, yz                       ! Coordinate products
      real(kind=WP), dimension(6) :: rtr                             ! Rotation tensor array
      real(kind=WP) :: abc, xxyz2, zzxy2, yyxz2, deta               ! Determinant calculation variables
      real(kind=WP) :: off_l                                         ! Local offset flag
      real(kind=WP), dimension(3,nel) :: xdp                        ! Coordinate derivatives
      real(kind=WP), dimension(nel,8) :: x0, y0, z0                 ! Initial coordinates
      real(kind=WP), dimension(nel) :: xd1, xd2, xd3, xd4           ! Degenerated x-coordinates 1-4
      real(kind=WP), dimension(nel) :: xd5, xd6, xd7, xd8           ! Degenerated x-coordinates 5-8
      real(kind=WP), dimension(nel) :: yd1, yd2, yd3, yd4           ! Degenerated y-coordinates 1-4
      real(kind=WP), dimension(nel) :: yd5, yd6, yd7, yd8           ! Degenerated y-coordinates 5-8
      real(kind=WP), dimension(nel) :: zd1, zd2, zd3, zd4           ! Degenerated z-coordinates 1-4
      real(kind=WP), dimension(nel) :: zd5, zd6, zd7, zd8           ! Degenerated z-coordinates 5-8
      real(kind=WP), dimension(nel) :: vxd1, vxd2, vxd3, vxd4       ! Degenerated x-velocities 1-4
      real(kind=WP), dimension(nel) :: vxd5, vxd6, vxd7, vxd8       ! Degenerated x-velocities 5-8
      real(kind=WP), dimension(nel) :: vyd1, vyd2, vyd3, vyd4       ! Degenerated y-velocities 1-4
      real(kind=WP), dimension(nel) :: vyd5, vyd6, vyd7, vyd8       ! Degenerated y-velocities 5-8
      real(kind=WP), dimension(nel) :: vzd1, vzd2, vzd3, vzd4       ! Degenerated z-velocities 1-4
      real(kind=WP), dimension(nel) :: vzd5, vzd6, vzd7, vzd8       ! Degenerated z-velocities 5-8
      real(kind=WP) :: xdl, ydl, zdl                                ! Local degenerated coordinates
      real(kind=WP), dimension(nel) :: rx, ry, rz                   ! Rotation matrix row 1
      real(kind=WP), dimension(nel) :: sx, sy, sz                   ! Rotation matrix row 2
      real(kind=WP), dimension(nel) :: tx, ty, tz                   ! Rotation matrix row 3
!c-----------------------------------------------
      off_l  = zero 
      write_flag = 0
      do i = 1,nel
        vis(i)=zero
        ngeo(i)=ixs(10,i)
        ngl(i)=ixs(11,i)
        mxt(i)=ixs(1,i)
        nc1(i)=ixs(2,i)
        nc2(i)=ixs(3,i)
        nc3(i)=ixs(4,i)
        nc4(i)=ixs(6,i)
        nc5(i)=ixs(7,i)
        nc6(i)=ixs(8,i)
        rhoo(i)=rho(i)
      enddo
!ok
!c----------------------------
!c     nodal coordinates     |
!c----------------------------
      do i = 1,nel
        x1(i)=x(1,nc1(i))
        y1(i)=x(2,nc1(i))
        z1(i)=x(3,nc1(i))
        x2(i)=x(1,nc2(i))
        y2(i)=x(2,nc2(i))
        z2(i)=x(3,nc2(i))
        x3(i)=x(1,nc3(i))
        y3(i)=x(2,nc3(i))
        z3(i)=x(3,nc3(i))
        x4(i)=x(1,nc4(i))
        y4(i)=x(2,nc4(i))
        z4(i)=x(3,nc4(i))
        x5(i)=x(1,nc5(i))
        y5(i)=x(2,nc5(i))
        z5(i)=x(3,nc5(i))
        x6(i)=x(1,nc6(i))
        y6(i)=x(2,nc6(i))
        z6(i)=x(3,nc6(i))
        off(i) = min(one,abs(offg(i)))
        off_l  = min(off_l,offg(i))

        !//todo:conversion from penta to degenerated solid 24
        xd1(i)=x1(i)
        yd1(i)=y1(i)
        zd1(i)=z1(i)

        xd2(i)=x2(i)
        yd2(i)=y2(i)
        zd2(i)=z2(i)

        xd3(i)=x3(i)
        yd3(i)=y3(i)
        zd3(i)=z3(i)

        xd4(i)=x3(i)  
        yd4(i)=y3(i)  
        zd4(i)=z3(i)  

        xd5(i)=x4(i)  
        yd5(i)=y4(i)  
        zd5(i)=z4(i)  

        xd6(i)=x5(i)  
        yd6(i)=y5(i)  
        zd6(i)=z5(i)  

        xd7(i)=x6(i)
        yd7(i)=y6(i)
        zd7(i)=z6(i)

        xd8(i)=x6(i)  
        yd8(i)=y6(i)  
        zd8(i)=z6(i)
        
        if (write_flag == 1) then
          write(*,*) 'i = ', i
          write(*,*) 'xd1(i) = ', xd1(i)
          write(*,*) 'xd2(i) = ', xd2(i)
          write(*,*) 'xd3(i) = ', xd3(i)
          write(*,*) 'xd4(i) = ', xd4(i)
          write(*,*) 'xd5(i) = ', xd5(i)
          write(*,*) 'xd6(i) = ', xd6(i)
          write(*,*) 'xd7(i) = ', xd7(i)
          write(*,*) 'xd8(i) = ', xd8(i)
          write(*,*) 'yd1(i) = ', yd1(i)
          write(*,*) 'yd2(i) = ', yd2(i)
          write(*,*) 'yd3(i) = ', yd3(i)
          write(*,*) 'yd4(i) = ', yd4(i)
          write(*,*) 'yd5(i) = ', yd5(i)
          write(*,*) 'yd6(i) = ', yd6(i)
          write(*,*) 'yd7(i) = ', yd7(i)
          write(*,*) 'yd8(i) = ', yd8(i)
          write(*,*) 'zd1(i) = ', zd1(i)
          write(*,*) 'zd2(i) = ', zd2(i)
          write(*,*) 'zd3(i) = ', zd3(i)
          write(*,*) 'zd4(i) = ', zd4(i)
          write(*,*) 'zd5(i) = ', zd5(i)
          write(*,*) 'zd6(i) = ', zd6(i)
          write(*,*) 'zd7(i) = ', zd7(i)
          write(*,*) 'zd8(i) = ', zd8(i)
        endif
      enddo
!c-----------
      do i = 1,nel
       vx1(i)=v(1,nc1(i))
       vy1(i)=v(2,nc1(i))
       vz1(i)=v(3,nc1(i))
       vx2(i)=v(1,nc2(i))
       vy2(i)=v(2,nc2(i))
       vz2(i)=v(3,nc2(i))
       vx3(i)=v(1,nc3(i))
       vy3(i)=v(2,nc3(i))
       vz3(i)=v(3,nc3(i))
       vx4(i)=v(1,nc4(i))
       vy4(i)=v(2,nc4(i))
       vz4(i)=v(3,nc4(i))
       vx5(i)=v(1,nc5(i))
       vy5(i)=v(2,nc5(i))
       vz5(i)=v(3,nc5(i))
       vx6(i)=v(1,nc6(i))
       vy6(i)=v(2,nc6(i))
       vz6(i)=v(3,nc6(i))

       vxd1(i)=vx1(i)
       vyd1(i)=vy1(i)
       vzd1(i)=vz1(i)

       vxd2(i)=vx2(i)
       vyd2(i)=vy2(i)
       vzd2(i)=vz2(i)

       vxd3(i)=vx3(i)
       vyd3(i)=vy3(i)
       vzd3(i)=vz3(i)

       vxd4(i)=vx3(i)  
       vyd4(i)=vy3(i)  
       vzd4(i)=vz3(i)  

       vxd5(i)=vx4(i)  
       vyd5(i)=vy4(i)  
       vzd5(i)=vz4(i)  

       vxd6(i)=vx5(i)  
       vyd6(i)=vy5(i)  
       vzd6(i)=vz5(i)  

       vxd7(i)=vx6(i)
       vyd7(i)=vy6(i)
       vzd7(i)=vz6(i)

       vxd8(i)=vx6(i)  
       vyd8(i)=vy6(i)  
       vzd8(i)=vz6(i) 

       if (write_flag == 1) then              
       write(*,*) 'vxd1(i) = ', vxd1(i)
       write(*,*) 'vxd2(i) = ', vxd2(i)
       write(*,*) 'vxd3(i) = ', vxd3(i)
       write(*,*) 'vxd4(i) = ', vxd4(i)
       write(*,*) 'vxd5(i) = ', vxd5(i)
       write(*,*) 'vxd6(i) = ', vxd6(i)
       write(*,*) 'vxd7(i) = ', vxd7(i)
       write(*,*) 'vxd8(i) = ', vxd8(i)
       write(*,*) 'vyd1(i) = ', vyd1(i)
       write(*,*) 'vyd2(i) = ', vyd2(i)
       write(*,*) 'vyd3(i) = ', vyd3(i)
       write(*,*) 'vyd4(i) = ', vyd4(i)
       write(*,*) 'vyd5(i) = ', vyd5(i)
       write(*,*) 'vyd6(i) = ', vyd6(i)
       write(*,*) 'vyd7(i) = ', vyd7(i)
       write(*,*) 'vyd8(i) = ', vyd8(i)   
       write(*,*) 'vzd1(i) = ', vzd1(i)
       write(*,*) 'vzd2(i) = ', vzd2(i)
       write(*,*) 'vzd3(i) = ', vzd3(i)
       write(*,*) 'vzd4(i) = ', vzd4(i)
       write(*,*) 'vzd5(i) = ', vzd5(i)
       write(*,*) 'vzd6(i) = ', vzd6(i)
       write(*,*) 'vzd7(i) = ', vzd7(i)
       write(*,*) 'vzd8(i) = ', vzd8(i)  
       endif     

      enddo
      if(off_l<zero)then
        do  i = 1,nel
          if(offg(i)<zero)then
            vx1(i)=zero
            vy1(i)=zero
            vz1(i)=zero
            vx2(i)=zero
            vy2(i)=zero
            vz2(i)=zero
            vx3(i)=zero
            vy3(i)=zero
            vz3(i)=zero
            vx4(i)=zero
            vy4(i)=zero
            vz4(i)=zero
            vx5(i)=zero
            vy5(i)=zero
            vz5(i)=zero
            vx6(i)=zero
            vy6(i)=zero
            vz6(i)=zero
          endif
        enddo
      endif
!c-----------
!c     prepare les sorties par part.
!c-----------
      if(ioutprt/=0)then
       do  i = 1,nel
        vgax(i)=vx1(i)+vx2(i)+vx3(i)+vx4(i)+vx5(i)+vx6(i)
        vgay(i)=vy1(i)+vy2(i)+vy3(i)+vy4(i)+vy5(i)+vy6(i)
        vgaz(i)=vz1(i)+vz2(i)+vz3(i)+vz4(i)+vz5(i)+vz6(i)
        vga2(i)=vx1(i)*vx1(i)+vx2(i)*vx2(i)+vx3(i)*vx3(i)+vx4(i)*vx4(i) &
               +vx5(i)*vx5(i)+vx6(i)*vx6(i) &
               +vy1(i)*vy1(i)+vy2(i)*vy2(i)+vy3(i)*vy3(i)+vy4(i)*vy4(i) &
               +vy5(i)*vy5(i)+vy6(i)*vy6(i) &
               +vz1(i)*vz1(i)+vz2(i)*vz2(i)+vz3(i)*vz3(i)+vz4(i)*vz4(i) &
               +vz5(i)*vz5(i)+vz6(i)*vz6(i)
       enddo
       if(iparg(80)==1) then
         do i = 1,nel
          xgax(i)=x1(i)+x2(i)+x3(i)+x4(i)+x5(i)+x6(i)
          xgay(i)=y1(i)+y2(i)+y3(i)+y4(i)+y5(i)+y6(i)
          xgaz(i)=z1(i)+z2(i)+z3(i)+z4(i)+z5(i)+z6(i)
          xgxa2(i)=x1(i)**2+x2(i)**2+x3(i)**2+x4(i)**2 &
                  +x5(i)**2+x6(i)**2
          xgya2(i)=y1(i)**2+y2(i)**2+y3(i)**2+y4(i)**2 &
                  +y5(i)**2+y6(i)**2
          xgza2(i)=z1(i)**2+z2(i)**2+z3(i)**2+z4(i)**2 &
                  +z5(i)**2+z6(i)**2
          xgxya(i)=x1(i)*y1(i)+x2(i)*y2(i)+x3(i)*y3(i)+x4(i)*y4(i) &
                  +x5(i)*y5(i)+x6(i)*y6(i)
          xgyza(i)=y1(i)*z1(i)+y2(i)*z2(i)+y3(i)*z3(i)+y4(i)*z4(i) &
                  +y5(i)*z5(i)+y6(i)*z6(i)
          xgzxa(i)=z1(i)*x1(i)+z2(i)*x2(i)+z3(i)*x3(i)+z4(i)*x4(i) &
                  +z5(i)*x5(i)+z6(i)*x6(i)
         enddo
       endif
      endif

!c-----------
!c     repere convecte (iterations).
!c-----------
      call srepiso3( &
          xd1,     xd2,     xd3,     xd4, &
          xd5,     xd6,     xd7,     xd8, &
          yd1,     yd2,     yd3,     yd4, &
          yd5,     yd6,     yd7,     yd8, &
          zd1,     zd2,     zd3,     zd4, &
          zd5,     zd6,     zd7,     zd8, &
          rx,      ry,      rz,      sx, &
          sy,      sz,      tx,      ty, &
          tz,      nel)

! do i = 1,nel
! write(*,*) 'rx(1)', rx(i)
! write(*,*) 'ry(i)', ry(i)  
! write(*,*) 'rz(i)', rz(i)
! write(*,*) 'sx(i)', sx(i)
! write(*,*) 'sy(i)', sy(i)
! write(*,*) 'sz(i)', sz(i)
! write(*,*) 'tx(i)', tx(i)
! write(*,*) 'ty(i)', ty(i)
! write(*,*) 'tz(i)', tz(i)
! enddo    
!c---

      call sortho3( &
          rx,      ry,      rz,      sx, &
          sy,      sz,      tx,      ty, &
          tz,      r12,     r13,     r11, &
          r22,     r23,     r21,     r32, &
          r33,     r31,     nel)

!c------stocker [r] in %gama_r--------------     
       gama_r(1:nel,1) = r11(1:nel) ! dir1_x                            
       gama_r(1:nel,2) = r21(1:nel) ! dir1_y                            
       gama_r(1:nel,3) = r31(1:nel) ! dir1_z                            
       gama_r(1:nel,4) = r12(1:nel) ! dir2_x                            
       gama_r(1:nel,5) = r22(1:nel) ! dir2_y                           
       gama_r(1:nel,6) = r32(1:nel) ! dir2_z     

!  do i=lft,llt
!    write(*,*) 'gama_r(i,1)', gama_r(i,1)
!    write(*,*) 'gama_r(i,2)', gama_r(i,2)  
!    write(*,*) 'gama_r(i,3)', gama_r(i,3)
!       write(*,*) 'gama_r(i,4)', gama_r(i,4)
!       write(*,*) 'gama_r(i,5)', gama_r(i,5)
!       write(*,*) 'gama_r(i,6)', gama_r(i,6)
!  enddo
     
!      write(*,*) 'isorth = ', isorth
       if (isorth == 0) then
!            write(*,*) 'in the srcoor3 subroutine, the isorth variable is 0.'
          do i = 1,nel                                    
            gama(i,1) = one                             
            gama(i,2) = zero                              
            gama(i,3) = zero
            gama(i,4) = zero                              
            gama(i,5) = one                              
            gama(i,6) = zero
          enddo      
      else    
        call sorthdir3( &
          rx, ry, rz, sx, &
          sy, sz, tx, ty, &
          tz, r11, r12, r13, &
          r21, r22, r23, r31, &
          r32, r33, gama0, gama, &
          nel, irep)
      endif



!c  -----------
    !  do i=lft,llt
    !    xl=one_over_6*(x1(i)+x2(i)+x3(i)+x4(i)+x5(i)+x6(i))
    !    yl=one_over_6*(y1(i)+y2(i)+y3(i)+y4(i)+y5(i)+y6(i))
    !    zl=one_over_6*(z1(i)+z2(i)+z3(i)+z4(i)+z5(i)+z6(i))
    !    x1(i)=x1(i)-xl
    !    y1(i)=y1(i)-yl
    !    z1(i)=z1(i)-zl
    !    x2(i)=x2(i)-xl
    !    y2(i)=y2(i)-yl
    !    z2(i)=z2(i)-zl
    !    x3(i)=x3(i)-xl
    !    y3(i)=y3(i)-yl
    !    z3(i)=z3(i)-zl
    !    x4(i)=x4(i)-xl
    !    y4(i)=y4(i)-yl
    !    z4(i)=z4(i)-zl
    !    x5(i)=x5(i)-xl
    !    y5(i)=y5(i)-yl
    !    z5(i)=z5(i)-zl
    !    x6(i)=x6(i)-xl
    !    y6(i)=y6(i)-yl
    !    z6(i)=z6(i)-zl
    !  enddo
!c-----------
!c     repere convecte 
!c-----------
   !    call s6cortho3(
   !  1   x1,      x2,      x3,      x4,
   !  2   x5,      x6,      y1,      y2,
   !  3   y3,      y4,      y5,      y6,
   !  4   z1,      z2,      z3,      z4,
   !  5   z5,      z6,      r11,     r12,
   !  6   r13,     r21,     r22,     r23,
   !  7   r31,     r32,     r33,     rx,
   !  8   ry,      rz,      sx,      sy,
   !  9   sz,      tx,      ty,      tz,
   !  a   nel)
!c------stocker [r] in %gama_r--------------     
    !   gama_r(lft:llt,1) = r11(lft:llt)                             
    !   gama_r(lft:llt,2) = r21(lft:llt)                             
    !   gama_r(lft:llt,3) = r31(lft:llt)                             
    !   gama_r(lft:llt,4) = r12(lft:llt)                             
    !   gama_r(lft:llt,5) = r22(lft:llt)                             
    !   gama_r(lft:llt,6) = r32(lft:llt)                             
!c-----------
!c     passage au repere convecte.
!c-----------
!c   x=rx' <=> x'=t(r)x chgt de base.
      
      if(ismstr<=4.and.jlag>0) then
       do i = 1,nel
       if(offg(i)>one)then
       !//todo:not touched
      x1(i)=sav(i,1)
      y1(i)=sav(i,2)
      z1(i)=sav(i,3)
      x2(i)=sav(i,4)
      y2(i)=sav(i,5)
      z2(i)=sav(i,6)
      x3(i)=sav(i,7)
      y3(i)=sav(i,8)
      z3(i)=sav(i,9)
      x4(i)=sav(i,10)
      y4(i)=sav(i,11)
      z4(i)=sav(i,12)
      x5(i)=sav(i,13)
      y5(i)=sav(i,14)
      z5(i)=sav(i,15)
      x6(i)=zero
      y6(i)=zero
      z6(i)=zero
      off(i) = offg(i) -one
      xl=one_over_6*(x1(i)+x2(i)+x3(i)+x4(i)+x5(i)+x6(i))
      yl=one_over_6*(y1(i)+y2(i)+y3(i)+y4(i)+y5(i)+y6(i))
      zl=one_over_6*(z1(i)+z2(i)+z3(i)+z4(i)+z5(i)+z6(i))
      x1(i)=x1(i)-xl
      y1(i)=y1(i)-yl
      z1(i)=z1(i)-zl
      x2(i)=x2(i)-xl
      y2(i)=y2(i)-yl
      z2(i)=z2(i)-zl
      x3(i)=x3(i)-xl
      y3(i)=y3(i)-yl
      z3(i)=z3(i)-zl
      x4(i)=x4(i)-xl
      y4(i)=y4(i)-yl
      z4(i)=z4(i)-zl
      x5(i)=x5(i)-xl
      y5(i)=y5(i)-yl
      z5(i)=z5(i)-zl
      x6(i)=x6(i)-xl
      y6(i)=y6(i)-yl
      z6(i)=z6(i)-zl
       else

        !  write(*,*) 'in the s6rcoor3_new subroutine, the ismstr variable is 4.'
        !  write(*,*) 'r11(i)', r11(i)
        !  write(*,*) 'r12(i)', r12(i)
        !  write(*,*) 'r13(i)', r13(i)
        !  write(*,*) 'r21(i)', r21(i)
        !  write(*,*) 'r22(i)', r22(i)
        !  write(*,*) 'r23(i)', r23(i)
        !  write(*,*) 'r31(i)', r31(i)
        !  write(*,*) 'r32(i)', r32(i)
        !  write(*,*) 'r33(i)', r33(i)
          xdl=r11(i)*xd1(i)+r21(i)*yd1(i)+r31(i)*zd1(i)
          ydl=r12(i)*xd1(i)+r22(i)*yd1(i)+r32(i)*zd1(i)
          zdl=r13(i)*xd1(i)+r23(i)*yd1(i)+r33(i)*zd1(i)
          xd1(i)=xdl
          yd1(i)=ydl
          zd1(i)=zdl
          xdl=r11(i)*xd2(i)+r21(i)*yd2(i)+r31(i)*zd2(i)
          ydl=r12(i)*xd2(i)+r22(i)*yd2(i)+r32(i)*zd2(i)
          zdl=r13(i)*xd2(i)+r23(i)*yd2(i)+r33(i)*zd2(i)
          xd2(i)=xdl
          yd2(i)=ydl
          zd2(i)=zdl
          xdl=r11(i)*xd3(i)+r21(i)*yd3(i)+r31(i)*zd3(i)
          ydl=r12(i)*xd3(i)+r22(i)*yd3(i)+r32(i)*zd3(i)
          zdl=r13(i)*xd3(i)+r23(i)*yd3(i)+r33(i)*zd3(i)
          xd3(i)=xdl
          yd3(i)=ydl
          zd3(i)=zdl
          xdl=r11(i)*xd4(i)+r21(i)*yd4(i)+r31(i)*zd4(i)
          ydl=r12(i)*xd4(i)+r22(i)*yd4(i)+r32(i)*zd4(i)
          zdl=r13(i)*xd4(i)+r23(i)*yd4(i)+r33(i)*zd4(i)
          xd4(i)=xdl
          yd4(i)=ydl
          zd4(i)=zdl
          xdl=r11(i)*xd5(i)+r21(i)*yd5(i)+r31(i)*zd5(i)
          ydl=r12(i)*xd5(i)+r22(i)*yd5(i)+r32(i)*zd5(i)
          zdl=r13(i)*xd5(i)+r23(i)*yd5(i)+r33(i)*zd5(i)
          xd5(i)=xdl
          yd5(i)=ydl
          zd5(i)=zdl
          xdl=r11(i)*xd6(i)+r21(i)*yd6(i)+r31(i)*zd6(i)
          ydl=r12(i)*xd6(i)+r22(i)*yd6(i)+r32(i)*zd6(i)
          zdl=r13(i)*xd6(i)+r23(i)*yd6(i)+r33(i)*zd6(i)
          xd6(i)=xdl
          yd6(i)=ydl
          zd6(i)=zdl
          xdl=r11(i)*xd7(i)+r21(i)*yd7(i)+r31(i)*zd7(i)
          ydl=r12(i)*xd7(i)+r22(i)*yd7(i)+r32(i)*zd7(i)
          zdl=r13(i)*xd7(i)+r23(i)*yd7(i)+r33(i)*zd7(i)
          xd7(i)=xdl
          yd7(i)=ydl
          zd7(i)=zdl
          xdl=r11(i)*xd8(i)+r21(i)*yd8(i)+r31(i)*zd8(i)
          ydl=r12(i)*xd8(i)+r22(i)*yd8(i)+r32(i)*zd8(i)
          zdl=r13(i)*xd8(i)+r23(i)*yd8(i)+r33(i)*zd8(i)
          xd8(i)=xdl
          yd8(i)=ydl
          zd8(i)=zdl              
          off(i) = offg(i)
          !//todo: a verify off_l  = min(off_l,offg(i))
       endif
       enddo
!c
      else
!       write(*,*) 'in the s6rcoor3_new subroutine, the ismstr variable is greater than 4.'
      call vrrota3( &
          r11, r12, r13, r21, &
          r22, r23, r31, r32, &
          r33, x1, y1, z1, &
          nel)
      call vrrota3( &
  r11, r12, r13, r21, &
  r22, r23, r31, r32, &
  r33, x2, y2, z2, &
  nel)
  call vrrota3( &
  r11, r12, r13, r21, &
  r22, r23, r31, r32, &
  r33, x3, y3, z3, &
  nel)
       do i = 1,nel
      xl=r11(i)*x4(i)+r21(i)*y4(i)+r31(i)*z4(i)
      yl=r12(i)*x4(i)+r22(i)*y4(i)+r32(i)*z4(i)
!c          zl=r13(i)*x4(i)+r23(i)*y4(i)+r33(i)*z4(i)
      x4(i)=xl
      y4(i)=yl
      z4(i)=-z1(i)
      xl=r11(i)*x5(i)+r21(i)*y5(i)+r31(i)*z5(i)
      yl=r12(i)*x5(i)+r22(i)*y5(i)+r32(i)*z5(i)
!c          zl=r13(i)*x5(i)+r23(i)*y5(i)+r33(i)*z5(i)
      x5(i)=xl
      y5(i)=yl
      z5(i)=-z2(i)
      xl=r11(i)*x6(i)+r21(i)*y6(i)+r31(i)*z6(i)
      yl=r12(i)*x6(i)+r22(i)*y6(i)+r32(i)*z6(i)
!c          zl=r13(i)*x6(i)+r23(i)*y6(i)+r33(i)*z6(i)
      x6(i)=xl
      y6(i)=yl
      z6(i)=-z3(i)
      off(i) = min(one,offg(i))
       enddo
!c
       endif


  
!-----------
!c     passage des vitesses au repere convecte (ou orthotrope).
! Transform velocities to corotational frame
call vrrota3( &
  r11, r12, r13, r21, &
  r22, r23, r31, r32, &
  r33, vx1, vy1, vz1, &
  nel)
call vrrota3( &
  r11, r12, r13, r21, &
  r22, r23, r31, r32, &
  r33, vx2, vy2, vz2, &
  nel)
call vrrota3( &
  r11, r12, r13, r21, &
  r22, r23, r31, r32, &
  r33, vx3, vy3, vz3, &
  nel)
call vrrota3( &
  r11, r12, r13, r21, &
  r22, r23, r31, r32, &
  r33, vx4, vy4, vz4, &
  nel)
call vrrota3( &
  r11, r12, r13, r21, &
  r22, r23, r31, r32, &
  r33, vx5, vy5, vz5, &
  nel)
call vrrota3( &
  r11, r12, r13, r21, &
  r22, r23, r31, r32, &
  r33, vx6, vy6, vz6, &
  nel)
!c-----------
      do i = 1,nel
      vd2(i) = zero
      enddo
!c-----projection----
      do i = 1,nel
      xx = x1(i)*x1(i) + x2(i)*x2(i) + x3(i)*x3(i) + &
           x4(i)*x4(i) + x5(i)*x5(i) + x6(i)*x6(i)
      yy = y1(i)*y1(i) + y2(i)*y2(i) + y3(i)*y3(i) + &
           y4(i)*y4(i) + y5(i)*y5(i) + y6(i)*y6(i)
      xy = x1(i)*y1(i) + x2(i)*y2(i) + x3(i)*y3(i) + &
           x4(i)*y4(i) + x5(i)*y5(i) + x6(i)*y6(i)
      xz = x1(i)*z1(i) + x2(i)*z2(i) + x3(i)*z3(i) + &
           x4(i)*z4(i) + x5(i)*z5(i) + x6(i)*z6(i)
      yz = y1(i)*z1(i) + y2(i)*z2(i) + y3(i)*z3(i) + &
           y4(i)*z4(i) + y5(i)*z5(i) + y6(i)*z6(i)
      zz = z1(i)*z1(i) + z2(i)*z2(i) + z3(i)*z3(i) + &
           z4(i)*z4(i) + z5(i)*z5(i) + z6(i)*z6(i)
      rtr(1)= yy+zz
      rtr(2)= xx+zz
      rtr(3)= xx+yy
      rtr(4)= -xy
      rtr(5)= -xz
      rtr(6)= -yz
!c       
      abc = rtr(1)*rtr(2)*rtr(3)
      xxyz2 = rtr(1)*rtr(6)*rtr(6)
      yyxz2 = rtr(2)*rtr(5)*rtr(5)
      zzxy2 = rtr(3)*rtr(4)*rtr(4)
      deta = abc + two*rtr(4)*rtr(5)*rtr(6)-xxyz2-yyxz2-zzxy2
      if (deta<em20) then
        deta=one
      else
       deta=one/deta
      endif
      di(i,1) = (abc-xxyz2)*deta/rtr(1)
      di(i,2) = (abc-yyxz2)*deta/rtr(2)
      di(i,3) = (abc-zzxy2)*deta/rtr(3)
      di(i,4) = (rtr(5)*rtr(6)-rtr(4)*rtr(3))*deta
      di(i,5) = (rtr(6)*rtr(4)-rtr(5)*rtr(2))*deta
      di(i,6) = (rtr(4)*rtr(5)-rtr(6)*rtr(1))*deta
      enddo
      
      ! Project coordinates and velocities to corotational frame
      call s6proj3( &
            x1, x2, x3, x4, &
            x5, x6, y1, y2, &
            y3, y4, y5, y6, &
            z1, z2, z3, z4, &
            z5, z6, vx1, vx2, &
            vx3, vx4, vx5, vx6, &
            vy1, vy2, vy3, vy4, &
            vy5, vy6, vz1, vz2, &
            vz3, vz4, vz5, vz6, &
            di, nel)

      ! Apply rotation transformation to degenerated velocities
      call srrota3( &
            r11, r12, r13, r21, &
            r22, r23, r31, r32, &
            r33, vxd1, vxd2, vxd3, &
            vxd4, vxd5, vxd6, vxd7, &
            vxd8, vyd1, vyd2, vyd3, &
            vyd4, vyd5, vyd6, vyd7, &
            vyd8, vzd1, vzd2, vzd3, &
            vzd4, vzd5, vzd6, vzd7, &
            vzd8, nel)


      do i = 1,nel
      x1(i)= xd1(i) 
      y1(i)= yd1(i) 
      z1(i)= zd1(i)

      x2(i)= xd2(i)  
      y2(i)= yd2(i)  
      z2(i)= zd2(i) 

      x3(i)= xd3(i) 
      y3(i)= yd3(i) 
      z3(i)= zd3(i) 

      x4(i)= xd5(i) 
      y4(i)= yd5(i) 
      z4(i)= zd5(i) 

      x5(i)= xd6(i) 
      y5(i)= yd6(i)
      z5(i)= zd6(i) 

      x6(i)= xd7(i) 
      y6(i)= yd7(i) 
      z6(i)= zd7(i) 
 
      !  write(*,*) 'i = ', i
      !  write(*,*) 'x1(i) = ', x1(i)
      !  write(*,*) 'x2(i) = ', x2(i)
      !  write(*,*) 'x3(i) = ', x3(i)
      !  write(*,*) 'x4(i) = ', x4(i)
      !  write(*,*) 'x5(i) = ', x5(i)
      !  write(*,*) 'x6(i) = ', x6(i)

      !  write(*,*) 'y1(i) = ', y1(i)
      !  write(*,*) 'y2(i) = ', y2(i)
      !  write(*,*) 'y3(i) = ', y3(i)
      !  write(*,*) 'y4(i) = ', y4(i)
      !  write(*,*) 'y5(i) = ', y5(i)
      !  write(*,*) 'y6(i) = ', y6(i)

      !  write(*,*) 'z1(i) = ', z1(i)
      !  write(*,*) 'z2(i) = ', z2(i)
      !  write(*,*) 'z3(i) = ', z3(i)
      !  write(*,*) 'z4(i) = ', z4(i)
      !  write(*,*) 'z5(i) = ', z5(i)
      !  write(*,*) 'z6(i) = ', z6(i)


      vx1(i)= vxd1(i) 
      vy1(i)= vyd1(i) 
      vz1(i)= vzd1(i)

      vx2(i)= vxd2(i)  
      vy2(i)= vyd2(i)  
      vz2(i)= vzd2(i) 

      vx3(i)= vxd3(i) 
      vy3(i)= vyd3(i) 
      vz3(i)= vzd3(i) 

      vx4(i)= vxd5(i) 
      vy4(i)= vyd5(i) 
      vz4(i)= vzd5(i) 

      vx5(i)= vxd6(i) 
      vy5(i)= vyd6(i)
      vz5(i)= vzd6(i) 

      vx6(i)= vxd7(i) 
      vy6(i)= vyd7(i) 
      vz6(i)= vzd7(i) 

      enddo  
!   call srrota3(
!  1   r11,     r12,     r13,     r21,
!  2   r22,     r23,     r31,     r32,
!  3   r33,     vx1,     vx2,     vx3,
!  4   vx1,     vx4,     vx5,     vx6,
!  5   vx6,     vy1,     vy2,     vy3,
!  6   vy1,     vy4,     vy5,     vy6,
!  7   vy6,     vz1,     vz2,     vz3,
!  8   vz1,     vz4,     vz5,     vz6,
!  9   vz6,     nel)
 
!  do i=lft,llt
!        x1(i) =   -6.0150095500754581     
!        x2(i) =   -1.1487646027368061     
!        x3(i) =   -0.0000000000000000     
!      
!        x4(i) =   -6.0150095500754581     
!        x5(i) =   -1.1487646027368061     
!       x6(i) =   -0.0000000000000000     
!     

! y1(i) =   -3.7174803446018458     
! y2(i) =   -4.8662449473386520     
! y3(i) =    0.0000000000000000        

!           y4(i) =   -3.7174803446018458     
!         y5(i) =   -4.8662449473386520     
!        y6(i) =    0.0000000000000000     

!       z1(i) =   -5.0000000000000009     
!       z2(i) =   -5.0000000000000009     
!       z3(i) =   -5.0000000000000009     
           
!      z4(i) =    0.0000000000000000     
!       z5(i) =    0.0000000000000000     
!     z6(i) =    0.0000000000000000     
     

!enddo


      return
end subroutine s6rcoor3_2
end module s6rcoor3_2_mod

