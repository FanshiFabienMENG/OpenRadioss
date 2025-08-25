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
! ======================================================================================================================
! \brief   initiation of penta solid element
! ======================================================================================================================
!||====================================================================
!||    s6init3               ../starter/source/elements/solid/solide6z/s6init3.F90
!||--- called by ------------------------------------------------------
!||    initia                 ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    atheri                 ../starter/source/ale/atheri.F
!||    dtmain                 ../starter/source/materials/time_step/dtmain.F
!||    failini                ../starter/source/elements/solid/solide/failini.F
!||    fretitl2               ../starter/source/starter/freform.F
!||    matini                 ../starter/source/materials/mat_share/matini.F
!||    s6ccoor3               ../starter/source/elements/thickshell/solide6c/s6ccoor3.F
!||    s6cderi3               ../starter/source/elements/thickshell/solide6c/s6cderi3.F
!||    s6mass3                ../starter/source/elements/thickshell/solide6c/s6mass3.F
!||    sbulk3                 ../starter/source/elements/solid/solide/sbulk3.F
!||    scmorth3               ../starter/source/elements/thickshell/solidec/scmorth3.F
!||    sczero3                ../starter/source/elements/thickshell/solidec/scinit3.F
!||    sdlensh3n              ../starter/source/elements/thickshell/solide6c/s6cinit3.F
!||    sigin20b               ../starter/source/elements/solid/solide20/s20mass3.F
!||    svalue0                ../starter/source/elements/thickshell/solidec/scinit3.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod           ../starter/source/modules/defaults_mod.F90
!||    detonators_mod         ../starter/share/modules1/detonators_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      module s6init3_mod
      contains
      subroutine s6init3(                                                   &                       
                         elbuf_str,mas     ,ixs     ,pm       ,x     ,      &
                         detonators,geo    ,ale_connectivity,iparg ,        &
                         dtelem   ,sigi    ,nel     ,skew     ,igeo  ,      &
                         stifn    ,partsav ,v       ,iparts   ,mss   ,      &
                         ipart    ,glob_therm,                              &
                         sigsp    ,nsigi   ,ipm     ,iuser    ,nsigs ,      &
                         volnod   ,bvolnod ,vns     ,bns      ,ptsol ,      &
                         bufmat   ,mcp     ,mcps    ,temp  ,                &
                         npf      ,tf      ,strsglob,straglob ,mssa  ,      &
                         fail_ini,iloadp  ,facload  ,rnoise,      &
                         perturb  ,mat_param,defaults_solid,                &
                         npropm   ,npropg  ,npropgi ,npropmi ,              &
                         lskew    ,sizloadp,lfacload,                       &
                         nixs     ,nperturb,ltitr   ,              &
                         nummat   ,numsol  ,lipart1,                        &
                         i7stifs  ,idttsh  ,isorth  ,istrain  ,             &
                         jthe     ,mtn     ,nft)                           
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
      use elbufdef_mod            
      use message_mod
      use detonators_mod      
      use ale_connectivity_mod
      use matparam_def_mod
      use defaults_mod
      use names_and_titles_mod, only : NCHARTITLE
      use glob_therm_mod
      use constant_mod
      use precision_mod, only : WP
      use eos_param_mod 
      use table_mat_vinterp_mod 

! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------------------------------------------------------
!c-----------------------------------------------
!                                               c i m p l i c i t t y p e
!c-----------------------------------------------
#include      "units_c.inc"
!#include      "implicit_f.inc"
!C-----------------------------------------------
!C   G l o b a l   P a r a m e t e r s
!C-----------------------------------------------
!#include      "mvsiz_p.inc"
!C-----------------------------------------------
!C   C o m m o n   B l o c k s
!C-----------------------------------------------
!#include      "com04_c.inc"
!#include      "param_c.inc"
!#include      "scr12_c.inc"
!#include      "scr17_c.inc"
!#include      "scry_c.inc"
!#include      "vect01_c.inc"
!C-----------------------------------------------
!C   D u m m y   A r g u m e n t s
!C-----------------------------------------------
! Integer arguments
integer, dimension(nixs, nel), intent(inout) :: ixs
integer, dimension(lipart1, nel), intent(inout) :: ipart
integer, dimension(npropmi, nel), intent(inout) :: ipm
integer, dimension(nel), intent(inout) :: iparg
integer, dimension(nel), intent(inout) :: iparts
integer, intent(in) :: nel
integer, intent(in) :: nsigi
integer, intent(in) :: iuser
integer, intent(in) :: nsigs
integer, dimension(nel), intent(in) :: npf
integer, dimension(sizloadp, nel), intent(in) :: iloadp
integer, dimension(npropgi, nel), intent(inout) :: igeo
integer, dimension(nel), intent(inout) :: strsglob
integer, dimension(nel), intent(inout) :: straglob
!integer, dimension(nel), intent(inout) :: orthoglob
integer, dimension(nel), intent(inout) :: fail_ini
integer, dimension(nperturb), intent(in) :: perturb
integer, dimension(*), intent(in) :: ptsol

! Real arguments
real(kind=8), dimension(nel), intent(inout) :: mas
real(kind=8), dimension(npropm, nel), intent(inout) :: pm
real(kind=8), dimension(nel), intent(inout) :: x
real(kind=8), dimension(npropg, nel), intent(inout) :: geo
real(kind=8), dimension(nel), intent(inout) :: dtelem
real(kind=8), dimension(nsigs, nel), intent(inout) :: sigi
real(kind=8), dimension(lskew, nel), intent(inout) :: skew
real(kind=8), dimension(nel), intent(inout) :: stifn
real(kind=8), dimension(20, nel), intent(inout) :: partsav
real(kind=8), dimension(nel), intent(inout) :: v
real(kind=8), dimension(8, nel), intent(inout) :: mss
real(kind=8), dimension(nsigi, nel), intent(inout) :: sigsp
real(kind=8), dimension(nel), intent(inout) :: volnod
real(kind=8), dimension(nel), intent(inout) :: bvolnod
real(kind=8), dimension(8, nel), intent(inout) :: vns
real(kind=8), dimension(8, nel), intent(inout) :: bns
real(kind=8), dimension(nel), intent(inout) :: bufmat
real(kind=8), dimension(nel), intent(inout) :: mcp
real(kind=8), dimension(8, nel), intent(inout) :: mcps
real(kind=8), dimension(nel), intent(inout) :: temp
real(kind=8), dimension(nel), intent(inout) :: tf
real(kind=8), dimension(nel), intent(inout) :: mssa
real(kind=8), dimension(nperturb, nel), intent(inout) :: rnoise
real(kind=8), dimension(lfacload, nel), intent(in) :: facload        ! Logical flag for shell elements

! Derived type arguments
type(elbuf_struct_), target, intent(inout) :: elbuf_str
type(detonators_struct_), intent(inout) :: detonators
type(t_ale_connectivity), intent(inout) :: ale_connectivity
type(matparam_struct_), dimension(nummat), intent(inout) :: mat_param
type(solid_defaults_), intent(in) :: defaults_solid
type(glob_therm_), intent(in) :: glob_therm

! New arguments
integer, intent(in) :: npropm       ! Number of material properties
integer, intent(in) :: npropg       ! Number of geometric properties
integer, intent(in) :: npropgi      ! Number of geometric integration properties
integer, intent(in) :: npropmi      ! Number of material integration properties
integer, intent(in) :: lskew        ! Logical flag for skew
integer, intent(in) :: sizloadp     ! Size of load parameter
integer, intent(in) :: lfacload     ! Load factor
integer, intent(in) :: nixs         ! Number of integration points
integer, intent(in) :: nperturb     ! Number of perturbations
integer, intent(in) :: ltitr        ! Logical flag for iterations
integer, intent(in) :: nummat       ! Number of materials
integer, intent(in) :: numsol       ! Number of solutions
integer, intent(in) :: lipart1      ! Logical flag for part 1
integer, intent(inout) :: i7stifs  ! Stiffness matrix index
integer, intent(inout) :: idttsh   ! Time step index
integer, intent(inout) :: isorth   ! Orthogonality index
integer, intent(inout) :: istrain  ! Strain index
integer, intent(inout) :: jthe     ! Thermal index
integer, intent(inout) :: mtn      ! Material type number
integer, intent(inout) :: nft      ! Number of failure types

!C-----------------------------------------------
!C   L o c a l   V a r i a b l e s
!C-----------------------------------------------
integer :: i, nf1, ibid, igtyp, irep, ip, ilay, nlay, nuvar, ncc, jhbe
integer :: nuvarr, idef, ipang, ipthk, ippos, ipmat, ig, im, mtn0, nlymax
integer :: ipid1, nptr, npts, nptt, l_pla, l_sigb, imas_ds
integer, dimension(nel) :: mat, pid, ngl, mat0
integer, dimension(nel) :: ix1, ix2, ix3, ix4, ix5, ix6


real(kind=8) :: bid, fv, sti, zi, wi
real(kind=8), dimension(nel) :: volu, dtx, vzl, vzq, rx, ry, rz
real(kind=8), dimension(nel) :: sx, sy, sz, tx, ty, tz
real(kind=8), dimension(nel) :: e1x, e1y, e1z, e2x, e2y, e2z, e3x, e3y, e3z
real(kind=8), dimension(nel) :: f1x, f1y, f1z, f2x, f2y, f2z
real(kind=8), dimension(nel) :: rhocp, temp0, deltax, aire
real(kind=8), dimension(51, nel) :: v8loc
real(kind=8) :: tempel(nel)
real(kind=8), dimension(nel) :: llsh
real(kind=8), dimension(nel) :: x1, x2, x3, x4, x5, x6
real(kind=8), dimension(nel) :: y1, y2, y3, y4, y5, y6
real(kind=8), dimension(nel) :: z1, z2, z3, z4, z5, z6

character(len=NCHARTITLE) :: titr1

type(g_bufel_), pointer :: gbuf
type(buf_lay_), pointer :: bufly
type(l_bufel_), pointer :: lbuf
type(buf_mat_), pointer :: mbuf


real(kind=8), dimension(9, 9) :: w_gauss, a_gauss
real(kind=8), dimension(nel) :: angle, dtx0
!wp = 8 !//TODO:why wp = 4

!C-----------------------------------------------
  w_gauss = reshape( [ &
  2.0_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  1.0_wp, 1.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.555555555555556_wp, 0.888888888888889_wp, 0.555555555555556_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.347854845137454_wp, 0.652145154862546_wp, 0.652145154862546_wp,&
  0.347854845137454_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.236926885056189_wp, 0.478628670499366_wp, 0.568888888888889_wp, &
  0.478628670499366_wp, 0.236926885056189_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.171324492379170_wp, 0.360761573048139_wp, 0.467913934572691_wp, &
  0.467913934572691_wp, 0.360761573048139_wp, 0.171324492379170_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.129484966168870_wp, 0.279705391489277_wp, 0.381830050505119_wp, &
  0.417959183673469_wp, 0.381830050505119_wp, 0.279705391489277_wp, &
  0.129484966168870_wp, 0.0_wp, 0.0_wp, &
  0.101228536290376_wp, 0.222381034453374_wp, 0.313706645877887_wp, &
  0.362683783378362_wp, 0.362683783378362_wp, 0.313706645877887_wp, &
  0.222381034453374_wp, 0.101228536290376_wp, 0.0_wp, &
  0.081274388361574_wp, 0.180648160694857_wp, 0.260610696402935_wp, &
  0.312347077040003_wp, 0.330239355001260_wp, 0.312347077040003_wp, & 
  0.260610696402935_wp, 0.180648160694857_wp, 0.081274388361574_wp ], &
  shape(w_gauss) )

  a_gauss = reshape( [ &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  -0.577350269189626_wp, 0.577350269189626_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  -0.774596669241483_wp, 0.0_wp, 0.774596669241483_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  -0.861136311594053_wp, -0.339981043584856_wp, 0.339981043584856_wp, &
  0.861136311594053_wp, 0.0_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  -0.906179845938664_wp, -0.538469310105683_wp, 0.0_wp, &
  0.538469310105683_wp, 0.906179845938664_wp, 0.0_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  -0.932469514203152_wp, -0.661209386466265_wp, -0.238619186083197_wp, &
  0.238619186083197_wp, 0.661209386466265_wp, 0.932469514203152_wp, &
  0.0_wp, 0.0_wp, 0.0_wp, &
  -0.949107912342759_wp, -0.741531185599394_wp, -0.405845151377397_wp, &
  0.0_wp, 0.405845151377397_wp, 0.741531185599394_wp, &
  0.949107912342759_wp, 0.0_wp, 0.0_wp, &
  -0.960289856497536_wp, -0.796666477413627_wp, -0.525532409916329_wp, &
  -0.183434642495650_wp, 0.183434642495650_wp, 0.525532409916329_wp, &
  0.796666477413627_wp, 0.960289856497536_wp, 0.0_wp, &
  -0.968160239507626_wp, -0.836031107326636_wp, -0.613371432700590_wp, &
  -0.324253423403809_wp, 0.0_wp, 0.324253423403809_wp, &
  0.613371432700590_wp, 0.836031107326636_wp, 0.968160239507626_wp ], &
  shape(A_GAUSS) )

!C-----------------------------------------------
!C   S o u r c e  L i n e s
!=======================================================================
      gbuf => elbuf_str%gbuf
      lbuf  => elbuf_str%bufly(1)%lbuf(1,1,1)
      mbuf  => elbuf_str%bufly(1)%mat(1,1,1)
      bufly => elbuf_str%bufly(1)
      nptr  =  elbuf_str%nptr
      npts  =  elbuf_str%npts
      nptt  =  elbuf_str%nptt
      nlay  =  elbuf_str%nlay
 
!=======================================================================
      jhbe  = iparg(23)
      irep  = iparg(35)
      igtyp = iparg(38)
      nf1 = nft+1
      idef = 0
      ibid = 0
      bid  = zero
      if (igtyp /= 22) then
        isorth = 0
      end if
      imas_ds = defaults_solid%imas
!=======================================================================
      do i = 1, nel
        rhocp(i) = pm(69, ixs(1, nft+i))
        temp0(i) = pm(79, ixs(1, nft+i))
      end do
!c
      call s6ccoor3(x     ,ixs(1,nf1)   ,geo  ,ngl  ,mat  ,pid  , &
                   rx   ,ry   ,rz   ,sx   ,sy   ,sz   ,tx   ,ty   ,tz   , &
                   e1x  ,e1y  ,e1z  ,e2x  ,e2y  ,e2z  ,e3x  ,e3y  ,e3z  , &
                   f1x  ,f1y  ,f1z  ,f2x  ,f2y  ,f2z  ,temp0, temp,glob_therm%nintemp, &
                   ix1, ix2, ix3, ix4, ix5, ix6, &
                   x1, x2, x3, x4, x5, x6, &
                   y1, y2, y3, y4, y5, y6, &
                   z1, z2, z3, z4, z5, z6)
!      if (igtyp == 21 .or. igtyp == 22) then
!        do i=1,nel
!         angle(i) =  geo(1,pid(i))
!        end do
!        call scmorth3(pid  ,geo  ,igeo ,skew ,irep ,gbuf%gama   , &
!                      rx   ,ry   ,rz   ,sx   ,sy   ,sz   ,tx   ,ty   ,tz   , &
!                      e1x  ,e1y  ,e1z  ,e2x  ,e2y  ,e2z  ,e3x  ,e3y  ,e3z  , &
!                      ngl  ,angle,nsigi,sigsp,nsigs,sigi ,ixs  ,1    , &
!                      orthoglob,ptsol,nel)
!       if (igtyp == 22) then
!        nlymax= 200
!        ipang = 200
!        ipthk = ipang+nlymax
!        ippos = ipthk+nlymax
!        ipmat = 100
!        ig=pid(1)
!        mtn0=mtn
!        do i=1,nel
!         mat0(i)=mat(i)
!         dtx0(i) = ep20
!        end do
!       end if
!      end if
      call s6cderi3(nel,gbuf%vol,geo,vzl,ngl,deltax,volu , &
                    x1, x2, x3, x4, x5, x6, &
                    y1, y2, y3, y4, y5, y6, &
                    z1, z2, z3, z4, z5, z6)
      if (idttsh > 0) then
         call sdlensh3n(nel,llsh, &
                        x1, x2, x3, x4, x5, x6, &
                        y1, y2, y3, y4, y5, y6, &
                        z1, z2, z3, z4, z5, z6)
!        do i=1,nel
!          if (gbuf%idt_tsh(i)>0) &
!             deltax(i)=max(llsh(i),deltax(i))
!        end do
      end if        
!c
!c     initialize element temperature from /initemp
!c      
      if (jthe == 0 .and. glob_therm%nintemp > 0) then
        do i=1,nel
          tempel(i) = one_over_8 *(temp(ixs(2,i)) + temp(ixs(3,i)) &
                                   + temp(ixs(4,i)) + temp(ixs(5,i)) &        
                                   + temp(ixs(6,i)) + temp(ixs(7,i)) &        
                                   + temp(ixs(8,i)) + temp(ixs(9,i))) 
        end do
      else
        tempel(1:nel) = temp0(1:nel)
      end if
!c
      ip=0
      call matini(pm      ,ixs    ,nixs       ,x      , &
                  geo     ,ale_connectivity  ,detonators ,iparg  , &
                  sigi    ,nel    ,skew       ,igeo   , &
                  ipart   ,iparts , &
                  mat     ,ipm    ,nsigs  ,numsol     ,ptsol  , &
                  ip      ,ngl    ,npf    ,tf         ,bufmat , &
                  gbuf    ,lbuf   ,mbuf   ,elbuf_str  ,iloadp , &
                  facload, deltax ,tempel )                         
!c
!      if (igtyp == 22) call sczero3(gbuf%rho,gbuf%sig,gbuf%eint,nel)
!c----------------------------------------
!c thermal initialization
      if(jthe /=0) call atheri(mat,pm,gbuf%temp)
!c-----------------------------
!c loop on integration points
!      do ilay=1,nlay
        ilay = 1
        lbuf => elbuf_str%bufly(ilay)%lbuf(1,1,1)
        mbuf => elbuf_str%bufly(ilay)%mat(1,1,1)
        l_pla = elbuf_str%bufly(ilay)%l_pla
        l_sigb= elbuf_str%bufly(ilay)%l_sigb
!c
!       if (igtyp == 22) then
!          zi = geo(ippos+ilay,ig)
!          wi = geo(ipthk+ilay,ig)
!          im=igeo(ipmat+ilay,ig)
!         mtn=nint(pm(19,im))
!         do i=1,nel
!          mat(i)=im
!            angle(i) = geo(ipang+ilay,pid(i))
!         end do
!       else
          zi = a_gauss(ilay,nlay)
          wi = w_gauss(ilay,nlay)
!       end if
!c
        do i=1,nel
          lbuf%vol0dp(i)= half*wi*(gbuf%vol(i)+vzl(i)*zi)
          lbuf%vol(i)= lbuf%vol0dp(i)
        end do
!        if (igtyp == 22) &
!           call scmorth3(pid  ,geo  ,igeo ,skew ,irep ,lbuf%gama   , &
!                         rx   ,ry   ,rz   ,sx   ,sy   ,sz   ,tx   ,ty   ,tz   , &
!                         e1x  ,e1y  ,e1z  ,e2x  ,e2y  ,e2z  ,e3x  ,e3y  ,e3z  , &
!                         ngl  ,angle,nsigi,sigsp,nsigs,sigi ,ixs,ilay, &
!                         orthoglob,ptsol,nel)
!c
!c     initialize element temperature from /initemp
!c      
        if (jthe == 0 .and. glob_therm%nintemp > 0) then
          do i=1,nel
            tempel(i) = one_over_8 *(temp(ixs(2,i)) + temp(ixs(3,i)) &
                                     + temp(ixs(4,i)) + temp(ixs(5,i)) &      
                                     + temp(ixs(6,i)) + temp(ixs(7,i)) &      
                                     + temp(ixs(8,i)) + temp(ixs(9,i))) 
          end do
        else
          tempel(1:nel) = temp0(1:nel)
        end if
!c
        call matini(pm      ,ixs    ,nixs   ,x         , &
                    geo     ,ale_connectivity  ,detonators,iparg  , &
                    sigi    ,nel    ,skew      ,igeo   , &
                    ipart   ,iparts , &
                    mat     ,ipm    ,nsigs  ,numsol    ,ptsol  , &
                    ilay    ,ngl    ,npf    ,tf        ,bufmat , &
                    gbuf    ,lbuf   ,mbuf   ,elbuf_str ,iloadp , &
                    facload, deltax ,tempel )
        if (mtn >= 28) then
           nuvar = ipm(8,ixs(1,nft+1))
           idef =1
        else
           nuvar = 0
            if(mtn == 14 .or. mtn == 12)then
               idef =1
            elseif(mtn == 24)then
              idef =1
            elseif(istrain == 1)then
             if(mtn == 1)then
               idef =1
             elseif(mtn == 2)then
               idef =1
             elseif(mtn == 4)then
               idef =1
            elseif(mtn == 3.or.mtn == 6.or.mtn == 10 &
                   .or.mtn == 21.or.mtn == 22.or.mtn == 23.or.mtn == 49)then
               idef =1
             end if
            end if
        end if
        call sigin20b( &
                      lbuf%sig,pm      ,lbuf%vol ,sigsp    , &
                      sigi    ,lbuf%eint,lbuf%rho,mbuf%var ,lbuf%stra, &
                      ixs     ,nixs     ,nsigi   ,ilay     ,nuvar    , &
                      nel     ,iuser    ,idef    ,nsigs    ,strsglob , &
                      straglob,jhbe     ,igtyp   ,x        ,lbuf%gama, &
                      mat     ,lbuf%pla ,l_pla   ,ptsol    ,lbuf%sigb, &
                      l_sigb  ,ipm      ,bufmat  ,lbuf%vol0dp)
!c
!        if(igtyp == 22) then
!c         moyene density,sig,...---
!          aire(:) = zero
!          call dtmain(geo       ,pm        ,ipm         ,pid     ,mat     ,fv    , &
!                      lbuf%eint ,lbuf%temp ,lbuf%deltax ,lbuf%rk ,lbuf%re ,bufmat, deltax, aire, &
!                      volu, dtx , igeo,igtyp)
!c
!          call svalue0( &
!                      lbuf%rho,lbuf%vol,lbuf%off,lbuf%sig,lbuf%eint,dtx, &
!                      gbuf%rho,gbuf%vol,gbuf%off,gbuf%sig,gbuf%eint,dtx0, &
!                      nel     )
!        end if
!      end do  ! ilay = 1,nlay
!c----------------------------------------
!      if(igtyp == 22) then
!       mtn=mtn0
!       do i=1,nel
!         mat(i)=mat0(i)
!       end do
!      end if
!c----------------------------------------
!c mass initialization
      call s6mass3(gbuf%rho,mas,partsav,x,v,iparts(nf1),mss(1,nf1), &
                   rhocp,mcp ,mcps(1,nf1),mssa(nf1),gbuf%fill, volu, &
                   ix1, ix2, ix3, ix4, ix5, ix6,imas_ds)
!c----------------------------------------
!c failure model initialization
      call failini(elbuf_str,nptr,npts,nptt,nlay, &
                   ipm,sigsp,nsigi,fail_ini , &
                   sigi,nsigs,ixs,nixs,ptsol, &
                   rnoise,perturb,mat_param)
!c------------------------------------------ 
!c  assemble nodal volumes and moduli for interface stiffness
!c  warning : ix1, ix2 ... ix6 <=> nc(mvsiz,6)
      if(i7stifs/=0)then
        ncc=6
        call sbulk3(volu  ,ix1    ,ncc,mat,pm , &
                    volnod,bvolnod,vns(1,nf1),bns(1,nf1),bid, &
                    bid ,gbuf%fill)
      end if
!c------------------------------------------
!c element time step
        aire(:) = zero
        call dtmain(geo       ,pm        ,ipm         ,pid     ,mat     ,fv    , &
                    lbuf%eint ,lbuf%temp ,lbuf%deltax ,lbuf%rk ,lbuf%re ,bufmat, deltax, aire, &
                    volu, dtx, igeo,igtyp)
!c------------------------------------------
!       if(igtyp == 22) then
!        do i=1,nel
!         dtx(i)=dtx0(i)
!        end do
!       end if
!c
      do i=1,nel
        if(ixs(10,i+nft) /= 0) then
          if (igtyp < 20 .or. igtyp > 22) then
             ipid1=ixs(nixs-1,i+nft)
             call fretitl2(titr1,igeo(npropgi-ltitr+1,ipid1),ltitr)
!             call ancmsg(msgid=226, &
!                         msgtype=msgerror, &
!                         anmode=aninfo_blind_1, &
!                         i1=igeo(1,ipid1), &
!                         c1=titr1, &
!                         i2=igtyp)
          end if
        end if
 !//TODO:why eight       
        dtelem(nft+i)=dtx(i)
        sti = fourth * gbuf%fill(i) * gbuf%rho(i) * volu(i) / &
              max(em20,dtx(i)*dtx(i))
        stifn(ixs(2,i+nft))=stifn(ixs(2,i+nft))+sti
        stifn(ixs(3,i+nft))=stifn(ixs(3,i+nft))+sti
        stifn(ixs(4,i+nft))=stifn(ixs(4,i+nft))+sti
        stifn(ixs(5,i+nft))=stifn(ixs(5,i+nft))+sti
        stifn(ixs(6,i+nft))=stifn(ixs(6,i+nft))+sti
        stifn(ixs(7,i+nft))=stifn(ixs(7,i+nft))+sti
        stifn(ixs(8,i+nft))=stifn(ixs(8,i+nft))+sti
        stifn(ixs(9,i+nft))=stifn(ixs(9,i+nft))+sti
      end do
!c-----------
      return
      end subroutine s6init3
end module s6init3_mod
