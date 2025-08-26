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
      !||    scdefo3    ../engine/source/elements/thickshell/solidec/scdefo3.F
      !||--- called by ------------------------------------------------------
      !||    s6cforc3   ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    scforc3    ../engine/source/elements/thickshell/solidec/scforc3.F
      !||====================================================================
      module s6def03_2_mod
            contains
      
      subroutine scdefo3_2( &
            dxx,     dxy,     dxz,     dyx, &
            dyy,     dyz,     dzx,     dzy, &
            dzz,     d4,      d5,      d6, &
            dcxx,    dcxy,    dcxz,    dcyx, &
            dcyy,    dcyz,    dczx,    dczy, &
            dczz, &
            dhxx,    dhxy,    dhxz,    dhyx, &
            dhyy,    dhyz,    dhzx,    dhzy, &
            dhzz, &
            zi,      wi,      vzl,     vol, &
            volg,    volo,    ddhv,    sig, &
            sigzm,   volm,    usb,     eint, &
            off,     offg,    offs, &
            dvc,     vol0dp,  voldp,   ipres, &
            nel,     dt1,     IRESP, ismdisp, iscau)

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   MODULES
      ! ----------------------------------------------------------------------------------------------------------------------
            use precision_mod, only : wp
            use constant_mod, only : zero, one, two, half, em20
            !use timestep_mod, only : dt1
            !use control_mod, only : iresp, ismdisp, iscau

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   IMPLICIT NONE
      ! ----------------------------------------------------------------------------------------------------------------------
            implicit none
      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   ARGUMENTS
      ! ----------------------------------------------------------------------------------------------------------------------
            integer,                    intent(in)    :: nel                      !< Number of elements
            integer,                    intent(in)    :: ismdisp
            integer,                    intent(in)    :: iscau
            INTEGER ,INTENT(IN) :: IRESP
            real(kind=WP),              intent(in) :: dt1 !< time step
            integer,                    intent(in)    :: ipres                    !< Pressure flag
            real(kind=wp),              intent(inout) :: dxx(nel)                 !< Strain rate component XX
            real(kind=wp),              intent(inout) :: dxy(nel)                 !< Strain rate component XY
            real(kind=wp),              intent(inout) :: dxz(nel)                 !< Strain rate component XZ
            real(kind=wp),              intent(inout) :: dyx(nel)                 !< Strain rate component YX
            real(kind=wp),              intent(inout) :: dyy(nel)                 !< Strain rate component YY
            real(kind=wp),              intent(inout) :: dyz(nel)                 !< Strain rate component YZ
            real(kind=wp),              intent(inout) :: dzx(nel)                 !< Strain rate component ZX
            real(kind=wp),              intent(inout) :: dzy(nel)                 !< Strain rate component ZY
            real(kind=wp),              intent(inout) :: dzz(nel)                 !< Strain rate component ZZ
            real(kind=wp),              intent(inout) :: d4(nel)                  !< Strain rate component 4
            real(kind=wp),              intent(inout) :: d5(nel)                  !< Strain rate component 5
            real(kind=wp),              intent(inout) :: d6(nel)                  !< Strain rate component 6
            real(kind=wp),              intent(in)    :: dcxx(nel)                !< Constant strain rate XX
            real(kind=wp),              intent(in)    :: dcxy(nel)                !< Constant strain rate XY
            real(kind=wp),              intent(in)    :: dcxz(nel)                !< Constant strain rate XZ
            real(kind=wp),              intent(in)    :: dcyx(nel)                !< Constant strain rate YX
            real(kind=wp),              intent(in)    :: dcyy(nel)                !< Constant strain rate YY
            real(kind=wp),              intent(in)    :: dcyz(nel)                !< Constant strain rate YZ
            real(kind=wp),              intent(in)    :: dczx(nel)                !< Constant strain rate ZX
            real(kind=wp),              intent(in)    :: dczy(nel)                !< Constant strain rate ZY
            real(kind=wp),              intent(in)    :: dczz(nel)                !< Constant strain rate ZZ
            real(kind=wp),              intent(in)    :: dhxx(nel)                !< Variable strain rate XX
            real(kind=wp),              intent(in)    :: dhxy(nel)                !< Variable strain rate XY
            real(kind=wp),              intent(in)    :: dhxz(nel)                !< Variable strain rate XZ
            real(kind=wp),              intent(in)    :: dhyx(nel)                !< Variable strain rate YX
            real(kind=wp),              intent(in)    :: dhyy(nel)                !< Variable strain rate YY
            real(kind=wp),              intent(in)    :: dhyz(nel)                !< Variable strain rate YZ
            real(kind=wp),              intent(in)    :: dhzx(nel)                !< Variable strain rate ZX
            real(kind=wp),              intent(in)    :: dhzy(nel)                !< Variable strain rate ZY
            real(kind=wp),              intent(in)    :: dhzz(nel)                !< Variable strain rate ZZ
            real(kind=wp),              intent(in)    :: zi                       !< Thickness coordinate
            real(kind=wp),              intent(in)    :: wi                       !< Weight factor
            real(kind=wp),              intent(in)    :: vzl(nel)                 !< Volume velocity
            real(kind=wp),              intent(inout) :: vol(nel)                 !< Current volume
            real(kind=wp),              intent(in)    :: volg(nel)                !< Global volume
            real(kind=wp),              intent(inout) :: volo(nel)                !< Old volume
            real(kind=wp),              intent(in)    :: ddhv(nel)                !< Volume strain rate
            real(kind=wp),              intent(in)    :: sig(nel,6)               !< Stress tensor
            real(kind=wp),              intent(in)    :: sigzm(nel)               !< Z stress component
            real(kind=wp),              intent(in)    :: volm(nel)                !< Modified volume
            real(kind=wp),              intent(in)    :: usb(nel)                 !< USB factor
            real(kind=wp),              intent(inout) :: eint(nel)                !< Internal energy
            real(kind=wp),              intent(inout) :: off(nel)                 !< Element flag
            real(kind=wp),              intent(in)    :: offg(nel)                !< Global element flag
            real(kind=wp),              intent(in)    :: offs(nel)                !< Shell element flag
            real(kind=wp),              intent(out)   :: dvc(nel)                 !< Volume change
            real(kind=8),               intent(inout) :: vol0dp(nel)              !< Double precision volume
            real(kind=8),               intent(out)   :: voldp(nel)               !< Double precision volume

      ! ----------------------------------------------------------------------------------------------------------------------
      !                                                   LOCAL VARIABLES
      ! ----------------------------------------------------------------------------------------------------------------------
            integer :: i                                                          !< Loop counter
            real(kind=wp) :: dv, dvz, ddv, tol, dt1d2, dt1d1                     !< Local work variables
!C=======================================================================
      tol = one - em20
      do i = 1, nel
        voldp(i) = half * wi * (volg(i) + vzl(i) * zi)
        vol(i) = voldp(i)
        off(i) = offg(i)
        if (vol(i) <= zero) then
           vol(i) = em20
           off(i) = zero
        elseif(off(i) == zero .or. offs(i) == two .or. ismdisp > 0) then
           voldp(i) = max(em20, voldp(i))
           vol(i) = max(em20, vol(i))
        endif
      enddo
      if (ipres == 1) then
        do i = 1, nel
!c  ------due to the assumed strain terms----
          dv = ddhv(i) * dt1 * zi
!c  ------due to the sigzz traitement----
          dvz = -(sig(i,3) - sigzm(i) / max(em20, volm(i))) * usb(i)
          ddv = (dv + dvz) * off(i)
          dvc(i) = ddv
          if (ddv > tol) then
           vol(i) = em20
           off(i) = zero
           ddv = zero
          elseif(off(i) == zero .or. offs(i) == two .or. ismdisp > 0) then
           ddv = zero
          end if
!c         
          if (iresp == 1) vol0dp(i) = vol0dp(i) * (one - ddv)
          if(ismdisp > 0 .or. offs(i) == two) dvc(i) = dvz
!c      
          volo(i) = volo(i) * (one - ddv)
          eint(i) = eint(i) / (one - ddv)
        enddo
      else
        do i = 1, nel
          dvc(i) = zero
        enddo
      end if
!c   +++ -----partie non constante------
      do i = 1, nel
!        write(*,*)  'zi = ', zi
        dxx(i) = dcxx(i) + zi * dhxx(i)
        dyy(i) = dcyy(i) + zi * dhyy(i)
        dzz(i) = dczz(i) + zi * dhzz(i)
        dxy(i) = dcxy(i) + zi * dhxy(i)
        dyx(i) = dcyx(i) + zi * dhyx(i)
        dzx(i) = dczx(i) + zi * dhzx(i)
        dzy(i) = dczy(i) + zi * dhzy(i)
        dxz(i) = dcxz(i) + zi * dhxz(i)
        dyz(i) = dcyz(i) + zi * dhyz(i)
!c        d4(i)=dc4(i)+zi*dh4(i)
!c        d5(i)=dc5(i)+zi*dh5(i)
!c        d6(i)=dc6(i)+zi*dh6(i)
      enddo
!c-----------
      dt1d1 = dt1
      if (ismdisp > 0 .and. iscau == 0) dt1d1 = zero
!      write(*,*) 'no second order strain terms in scdefo3'
      dt1d2 = half * dt1d1
       do i = 1, nel
        d4(i)   = dxy(i) + dyx(i) &
     &           - dt1d1 * (dxx(i) * dxy(i) + dyx(i) * dyy(i) + dzx(i) * dzy(i))
        d5(i)   = dyz(i) + dzy(i) &
     &           - dt1d1 * (dyy(i) * dyz(i) + dzy(i) * dzz(i) + dxy(i) * dxz(i))
        d6(i)   = dxz(i) + dzx(i) &
     &           - dt1d1 * (dzz(i) * dzx(i) + dxz(i) * dxx(i) + dyz(i) * dyx(i))
        dxx(i)  = dxx(i) &
     &           - dt1d2 * (dxx(i) * dxx(i) + dyx(i) * dyx(i) + dzx(i) * dzx(i))
        dyy(i)  = dyy(i) &
     &           - dt1d2 * (dyy(i) * dyy(i) + dzy(i) * dzy(i) + dxy(i) * dxy(i))
        dzz(i)  = dzz(i) &
     &           - dt1d2 * (dzz(i) * dzz(i) + dxz(i) * dxz(i) + dyz(i) * dyz(i))
       enddo
!c       
      return
     

      ! (existing subroutine code remains here)
! ----------------------------------------------------------------------------------------------------------------------
    end subroutine scdefo3_2
    end module s6def03_2_mod