Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
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
      SUBROUTINE S6CHOUR3_1(
     .   PM, NPROPM, RHO,VOL,SSP,  !A VERIFIER CHAQUE PARAMETER
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,    
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   F11,F12,F13,F15,F16,F17,
     .   F21,F22,F23,F25,F26,F27,     
     .   F31,F32,F33,F35,F36,F37,
     .   NU ,FHOUR ,OFF,VOL0,EINT,NEL,
     .   MAT,NPROPG,GEO,PID) !A VERIFIER CHAQUE PARAMETER
!MAT PM, NPROPM,is verified
C-----------------------------------------------
C   M O D U L E S
C-----------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
       USE DEBUG_MOD
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER , INTENT(IN)  :: NEL, NPROPM,MAT(*),NPROPG,PID(*)
C     REAL
      my_real, DIMENSION(MVSIZ) , INTENT(IN)  :: 
     .   X1, X2, X3, X4, X5, X6, X7, X8, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,  
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,   
     .   VZ1,VZ2,VZ3,VZ4,VZ5,VZ6,VZ7,VZ8,
     .   VX1,VX2,VX3,VX4,VX5,VX6,VX7,VX8,
     .   VY1,VY2,VY3,VY4,VY5,VY6,VY7,VY8,
     .   RHO,VOL,SSP,NU,OFF,
     .   PM(NPROPM,*),GEO(NPROPG,*)
      my_real, DIMENSION(NEL) , INTENT(IN)  :: VOL0
      my_real, DIMENSION(NEL) , INTENT(INOUT)  :: EINT
      my_real, DIMENSION(MVSIZ) , INTENT(INOUT)  :: 
     .   F11,F12,F13,F15,F16,F17,
     .   F21,F22,F23,F25,F26,F27,
     .   F31,F32,F33,F35,F36,F37
      my_real, DIMENSION(NEL,3,4) , INTENT(INOUT)  :: FHOUR
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER :: I,J, MX,MT,Write_flag,hourglass   
C                                                                     12
      my_real
     .   DETT ,  
     .   JACI1, JACI2, JACI3,
     .   JACI4, JACI5, JACI6,
     .   JACI7, JACI8, JACI9,
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),JAC_19_37(MVSIZ),
     .   JACI12, JACI45, JACI78,
     .   X_17_46 , X_28_35 ,
     .   Y_17_46 , Y_28_35 ,
     .   Z_17_46 , Z_28_35 ,
     .   HX,HY,HZ,H1X,H1Y,H1Z,H2X,H2Y,H2Z,H3X,H3Y,H3Z,H4X,H4Y,H4Z,
     .   PX1(MVSIZ), PX2(MVSIZ), PX3(MVSIZ), PX4(MVSIZ),  
     .   PY1(MVSIZ), PY2(MVSIZ), PY3(MVSIZ), PY4(MVSIZ),  
     .   PZ1(MVSIZ), PZ2(MVSIZ), PZ3(MVSIZ), PZ4(MVSIZ),  
     .   PX1H1(MVSIZ),PX2H1(MVSIZ),PX3H1(MVSIZ),PX4H1(MVSIZ),   
     .   PX1H2(MVSIZ),PX2H2(MVSIZ),PX3H2(MVSIZ),PX4H2(MVSIZ),   
     .   PX1H3(MVSIZ),PX2H3(MVSIZ),PX3H3(MVSIZ),PX4H3(MVSIZ),   
     .   PX1H4(MVSIZ),PX2H4(MVSIZ),PX3H4(MVSIZ),PX4H4(MVSIZ),        
     .   JAC1(MVSIZ),JAC2(MVSIZ),JAC3(MVSIZ),
     .   JAC4(MVSIZ),JAC5(MVSIZ),JAC6(MVSIZ),
     .   JAC7(MVSIZ),JAC8(MVSIZ),JAC9(MVSIZ),DET(MVSIZ)
      my_real
!     .   G11(MVSIZ),G21(MVSIZ),G31(MVSIZ),G41(MVSIZ),
!     .   G51(MVSIZ),G61(MVSIZ),G71(MVSIZ),G81(MVSIZ),
!     .   G12(MVSIZ),G22(MVSIZ),G32(MVSIZ),G42(MVSIZ),
!     .   G52(MVSIZ),G62(MVSIZ),G72(MVSIZ),G82(MVSIZ),
!     .   G13(MVSIZ),G23(MVSIZ),G33(MVSIZ),G43(MVSIZ),
!     .   G53(MVSIZ),G63(MVSIZ),G73(MVSIZ),G83(MVSIZ),
!     .   G14(MVSIZ),G24(MVSIZ),G34(MVSIZ),G44(MVSIZ),
!     .   G54(MVSIZ),G64(MVSIZ),G74(MVSIZ),G84(MVSIZ),
     .   FCL(MVSIZ),NFHZ1(MVSIZ),NFHZ2(MVSIZ),VZ17,VZ28,VZ35,VZ46,
     .   H1VZ,H2VZ,HGZ1(MVSIZ),HGZ2(MVSIZ),CC,
     .   GG(MVSIZ) ,E_DT(MVSIZ),THK,THK_1(MVSIZ),
     .   RHO0,G0,C1,NUU,CXX(MVSIZ),CAQ(MVSIZ),
     .   G_3DT(MVSIZ),E0(MVSIZ),
     .   NU1(MVSIZ),NU2(MVSIZ),NU3(MVSIZ),NU4(MVSIZ),
     .   VX3478, VX2358, VX1467, VX1256, 
     .   VY3478, VY2358, VY1467, VY1256,
     .   VZ3478, VZ2358, VZ1467, VZ1256,
     .   VX17, VY17, 
     .   VX28, VY28, 
     .   VX35, VY35, 
     .   VX46, VY46, 
     .   HGX1(MVSIZ), HGX2(MVSIZ), HGX3(MVSIZ), HGX4(MVSIZ),
     .   HGY1(MVSIZ), HGY2(MVSIZ), HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ3(MVSIZ), HGZ4(MVSIZ),
     .   JR_1(MVSIZ),JS_1(MVSIZ),JT_1(MVSIZ),NFHOUR(MVSIZ,3,4),
     .   JR0(MVSIZ) ,JS0(MVSIZ) ,JT0(MVSIZ),
     .   H11(MVSIZ), H22(MVSIZ), H33(MVSIZ),
     .   H12(MVSIZ), H13(MVSIZ), H23(MVSIZ),
     .   FHOUR2(MVSIZ,3,4), !//TODO: A corriger Comme entrer
     .   E_R,E_S,E_T,
     .   DFHOUR(MVSIZ,3,4),FHOURT(3,4),
     ,   NFHX1(MVSIZ),NFHX2(MVSIZ),NFHX3(MVSIZ),NFHX4(MVSIZ),
     ,   NFHY1(MVSIZ),NFHY2(MVSIZ),NFHY3(MVSIZ),NFHY4(MVSIZ),
     ,   NFHZ3(MVSIZ),NFHZ4(MVSIZ),
     .   F11_hgl(MVSIZ),F12_hgl(MVSIZ),F13_hgl(MVSIZ),F14_hgl(MVSIZ),F15_hgl(MVSIZ),F16_hgl(MVSIZ),
     .   F17_hgl(MVSIZ),F18_hgl(MVSIZ),
     .   F21_hgl(MVSIZ),F22_hgl(MVSIZ),F23_hgl(MVSIZ),F24_hgl(MVSIZ),F25_hgl(MVSIZ),F26_hgl(MVSIZ),
     .   F27_hgl(MVSIZ),F28_hgl(MVSIZ),
     .   F31_hgl(MVSIZ),F32_hgl(MVSIZ),F33_hgl(MVSIZ),F34_hgl(MVSIZ),F35_hgl(MVSIZ),F36_hgl(MVSIZ),
     .   F37_hgl(MVSIZ),F38_hgl(MVSIZ),
     .   HQ13P,HQ13N,HQ24P,HQ24N,FF
C=======================================================================
C Same in Isolid24

      Write_flag = 0
      hourglass = 1
       
      if (Write_flag == 1) THEN
      DO I=1,NEL
      write(*,*) 'I = ', I
      write(*,*) 'X1(I) = ', X1(I)
      write(*,*) 'X2(I) = ', X2(I)
      write(*,*) 'X3(I) = ', X3(I)
      write(*,*) 'X4(I) = ', X4(I)
      write(*,*) 'X5(I) = ', X5(I)
      write(*,*) 'X6(I) = ', X6(I)
      write(*,*) 'X7(I) = ', X7(I)
      write(*,*) 'X8(I) = ', X8(I)
      write(*,*) 'Y1(I) = ', Y1(I)
      write(*,*) 'Y2(I) = ', Y2(I)
      write(*,*) 'Y3(I) = ', Y3(I)
      write(*,*) 'Y4(I) = ', Y4(I)
      write(*,*) 'Y5(I) = ', Y5(I)
      write(*,*) 'Y6(I) = ', Y6(I)
      write(*,*) 'Y7(I) = ', Y7(I)
      write(*,*) 'Y8(I) = ', Y8(I)
      write(*,*) 'Z1(I) = ', Z1(I)
      write(*,*) 'Z2(I) = ', Z2(I)
      write(*,*) 'Z3(I) = ', Z3(I)
      write(*,*) 'Z4(I) = ', Z4(I)
      write(*,*) 'Z5(I) = ', Z5(I)
      write(*,*) 'Z6(I) = ', Z6(I)
      write(*,*) 'Z7(I) = ', Z7(I)
      write(*,*) 'Z8(I) = ', Z8(I)

      write(*,*) 'VX1(I) = ', VX1(I)
      write(*,*) 'VX2(I) = ', VX2(I)
      write(*,*) 'VX3(I) = ', VX3(I)
      write(*,*) 'VX4(I) = ', VX4(I)
      write(*,*) 'VX5(I) = ', VX5(I)
      write(*,*) 'VX6(I) = ', VX6(I)
      write(*,*) 'VX7(I) = ', VX7(I)
      write(*,*) 'VX8(I) = ', VX8(I)
      write(*,*) 'VY1(I) = ', VY1(I)
      write(*,*) 'VY2(I) = ', VY2(I)
      write(*,*) 'VY3(I) = ', VY3(I)
      write(*,*) 'VY4(I) = ', VY4(I)
      write(*,*) 'VY5(I) = ', VY5(I)
      write(*,*) 'VY6(I) = ', VY6(I)
      write(*,*) 'VY7(I) = ', VY7(I)
      write(*,*) 'VY8(I) = ', VY8(I)
      write(*,*) 'VZ1(I) = ', VZ1(I)
      write(*,*) 'VZ2(I) = ', VZ2(I)
      write(*,*) 'VZ3(I) = ', VZ3(I)
      write(*,*) 'VZ4(I) = ', VZ4(I)
      write(*,*) 'VZ5(I) = ', VZ5(I)
      write(*,*) 'VZ6(I) = ', VZ6(I)
      write(*,*) 'VZ7(I) = ', VZ7(I)
      write(*,*) 'VZ8(I) = ', VZ8(I)
      !!pause
      ENDDO 
      ENDIF

       MX = MAT(1) !//TODO:(A VERIFIER MAT ? ) 
!       MX = 1
!//TODO:INPUT
! PM  !//TODO:INPUT
       RHO0=PM(1,MX)
       !//TODO: Compare with NU(*)
       NUU=PM(21,MX) 
!//TODO:A distinguish
       G0=PM(22,MX)
       C1=PM(32,MX)  
       if (Write_flag == 1) THEN
       write(*,*) 'MX = ', MX
       write(*,*) 'RHO0=',RHO0,' NUU=',NUU,' G0=',G0,' C1=',C1 
       endif
! RHO0= 7.8500000000000008E-009
! NUU= 0.29999999999999999
! G0= 80769.230769230766
! C1= 174999.99999999997 
      
!   DO I=1,NEL       
!     GG(I)=HALF*RHO0*CXX(I)*CXX(I)*(ONE -TWO*NU)/(ONE-NU)     
!    ENDDO
      DO I=1,NEL
        CXX(I) = SSP(I) !//TODO:A VERIFIER 
        
        GG(I)=HALF*RHO0*CXX(I)*CXX(I)*(ONE -TWO*NUU)/(ONE-NUU)     
        !GG(I)=1.
        if (Write_flag == 1) THEN
        write(*,*) 'NU(I)',NU(I) !//Identique à NUU
        write(*,*) 'CXX(I)',CXX(I)
        write(*,*) 'GG',GG(I)
        endif
      ENDDO
      

      MT = PID(1)

      DO I=1,NEL
        
         CAQ(I)=FOURTH*OFF(I)*GEO(13,MT)
         if (Write_flag == 1) THEN
         write(*,*) 'GEO(13,MT)',GEO(13,MT)
         write(*,*) 'CAQ',CAQ(I)
         endif
        !CAQ(I) = 0.25
        !FHOUR(I,1,1) = 0. 
        !FHOUR(I,1,2) = 0. 
        !FHOUR(I,1,3) = 0.
        !FHOUR(I,1,4) = 0.  
        !FHOUR(I,2,1) = 0. 
        !FHOUR(I,2,2) = 0. 
        !FHOUR(I,2,3) = 0.
        !FHOUR(I,2,4) = 0. 
        !FHOUR2(I,3,1) = 0. 
        !FHOUR2(I,3,1) = FHOUR(I,1)
        !FHOUR2(I,3,2) = 0. 
        !FHOUR2(I,3,2) = FHOUR(I,2)
        !FHOUR2(I,3,3) = 0.
        !FHOUR2(I,3,4) = 0. 

      ENDDO

   !   DO I=1,NEL
   !     G_3DT(I)=THIRD*OFF(I)*GG(I)*DT1
   !     E0(I)=TWO*(ONE+NU)*GG(I)
    !  ENDDO

      DO I=1,NEL
        G_3DT(I)=THIRD*OFF(I)*GG(I)*DT1
        E0(I)=TWO*(ONE+NU(I))*GG(I)
        if (Write_flag == 1) THEN
        write(*,*) 'G_3DT',G_3DT(I)
        write(*,*) 'E0',E0(I)
        endif
      ENDDO      

C//TODO:A VERIFIER
      DO I=1,NEL
       ! write(*,*) 'NUUUUUUUUUUUUU'
        NU1(I) =TWO/(ONE-NUU)
        NU2(I) =NUU*NU1(I)
        NU3(I) =TWO_THIRD*(ONE + NUU)
        NU4(I) =NUU
        if (Write_flag == 1) THEN
        write(*,*) 'NU1=',NU1(I)
        write(*,*) 'NU2=',NU2(I)
        write(*,*) 'NU3=',NU3(I)
        write(*,*) 'NU4=',NU4(I)
        endif
      ENDDO   
      !!pause

      DO I=1,NEL
        FCL(I)=CAQ(I)*RHO(I)*VOL(I)**THIRD
        FCL(I)=ZEP00666666667*FCL(I)*CXX(I)
        if (Write_flag == 1) THEN
        write(*,*) 'FCL=',FCL(I)
        endif
      ENDDO


    
C
      DO I=1,NEL
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//szderi3        
        X17(I)=X7(I)-X1(I)
        X28(I)=X8(I)-X2(I)
        X35(I)=X5(I)-X3(I)
        X46(I)=X6(I)-X4(I)

        Y17(I)=Y7(I)-Y1(I)
        Y28(I)=Y8(I)-Y2(I)
        Y35(I)=Y5(I)-Y3(I)
        Y46(I)=Y6(I)-Y4(I)

        Z17(I)=Z7(I)-Z1(I)
        Z28(I)=Z8(I)-Z2(I)
        Z35(I)=Z5(I)-Z3(I)
        Z46(I)=Z6(I)-Z4(I)
      END DO

C JACOBIAN MATRIX
      DO I=1,NEL
C//
        JAC4(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JAC5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JAC6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)

        X_17_46=X17(I)+X46(I)
        X_28_35=X28(I)+X35(I)
        Y_17_46=Y17(I)+Y46(I)
        Y_28_35=Y28(I)+Y35(I)
        Z_17_46=Z17(I)+Z46(I)
        Z_28_35=Z28(I)+Z35(I)

        JAC7(I)=X_17_46+X_28_35
        JAC8(I)=Y_17_46+Y_28_35
        JAC9(I)=Z_17_46+Z_28_35
        JAC1(I)=X_17_46-X_28_35
        JAC2(I)=Y_17_46-Y_28_35
        JAC3(I)=Z_17_46-Z_28_35
        
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_19_37(I)=JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)


        DET(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
        if (Write_flag == 1) THEN
        write(*,*) 'JAC1 = ', JAC1(I)
        write(*,*) 'JAC2 = ', JAC2(I)
        write(*,*) 'JAC3 = ', JAC3(I)
        write(*,*) 'JAC4 = ', JAC4(I)
        write(*,*) 'JAC5 = ', JAC5(I)
        write(*,*) 'JAC6 = ', JAC6(I)
        write(*,*) 'JAC7 = ', JAC7(I)
        write(*,*) 'JAC8 = ', JAC8(I)
        write(*,*) 'JAC9 = ', JAC9(I)
        write(*,*) 'DET = ', DET(I)
        !pause
        endif 

        THK =FOURTH*JAC5(I) !//TODO: A VERIFIER
        THK_1(I) =ONE/THK !//TODO: A VERIFIER
      ENDDO


  
C JACOBIAN MATRIX INVERSE 
      DO I=1,NEL
C//same in the szderi3    
        DETT=ONE_OVER_64/DET(I)
        JACI1=DETT*JAC_59_68(I)
        JACI4=DETT*JAC_67_49(I)
        JACI7=DETT*JAC_48_57(I)
        JACI2=DETT*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JACI5=DETT*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
        JACI8=DETT*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JACI3=DETT*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JACI6=DETT*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JACI9=DETT*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
C
        JACI12=JACI1-JACI2
        JACI45=JACI4-JACI5
        JACI78=JACI7-JACI8
        PX2(I)= JACI12-JACI3
        PY2(I)= JACI45-JACI6
        PZ2(I)= JACI78-JACI9
        PX4(I)=-JACI12-JACI3
        PY4(I)=-JACI45-JACI6
        PZ4(I)=-JACI78-JACI9

        JACI12=JACI1+JACI2
        JACI45=JACI4+JACI5
        JACI78=JACI7+JACI8
        PX1(I)=-JACI12-JACI3
        PY1(I)=-JACI45-JACI6
        PZ1(I)=-JACI78-JACI9
        PX3(I)=JACI12-JACI3
        PY3(I)=JACI45-JACI6
        PZ3(I)=JACI78-JACI9
        if (Write_flag == 1) THEN
        write(*,*) 'JACI1 = ', JACI1
        write(*,*) 'JACI2 = ', JACI2
        write(*,*) 'JACI3 = ', JACI3
        write(*,*) 'JACI4 = ', JACI4
        write(*,*) 'JACI5 = ', JACI5
        write(*,*) 'JACI6 = ', JACI6
        write(*,*) 'JACI7 = ', JACI7
        write(*,*) 'JACI8 = ', JACI8
        write(*,*) 'JACI9 = ', JACI9

        
            write(*,*) 'PX1(I) = ', PX1(I)
            write(*,*) 'PX2(I) = ', PX2(I)
            write(*,*) 'PX3(I) = ', PX3(I)
            write(*,*) 'PX4(I) = ', PX4(I)
            write(*,*) 'PY1(I) = ', PY1(I)
            write(*,*) 'PY2(I) = ', PY2(I)
            write(*,*) 'PY3(I) = ', PY3(I)
            write(*,*) 'PY4(I) = ', PY4(I)
            write(*,*) 'PZ1(I) = ', PZ1(I)
            write(*,*) 'PZ2(I) = ', PZ2(I)
            write(*,*) 'PZ3(I) = ', PZ3(I)
            write(*,*) 'PZ4(I) = ', PZ4(I)      
            write(*,*) 'I = ', I
        
      !pause
        endif



      ENDDO

C we do it in the same order of SZDERI3
C H3
C 1 -1 1 -1 1 -1 1 -1
       DO I=1,NEL
         H3X=X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I)
         H3Y=Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I)
         H3Z=Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I)   
         HX=ONE_OVER_8*H3X
         HY=ONE_OVER_8*H3Y
         HZ=ONE_OVER_8*H3Z
         PX1H3(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H3(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H3(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H3(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
         if (Write_flag == 1) THEN
         write(*,*) 'PX1H3(I) = ', PX1H3(I)
         write(*,*) 'PX2H3(I) = ', PX2H3(I)
         write(*,*) 'PX3H3(I) = ', PX3H3(I)
         write(*,*) 'PX4H3(I) = ', PX4H3(I)
         write(*,*) 'I = ', I
         pause
         endif

       END DO   
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
C   H1
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         H1X=X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I)
         H1Y=Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I)
         H1Z=Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I)
         HX=ONE_OVER_8*H1X
         HY=ONE_OVER_8*H1Y
         HZ=ONE_OVER_8*H1Z
         PX1H1(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H1(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H1(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H1(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
         if (Write_flag == 1) THEN
         write(*,*) 'PX1H1(I) = ', PX1H1(I)
         write(*,*) 'PX2H1(I) = ', PX2H1(I)
         write(*,*) 'PX3H1(I) = ', PX3H1(I)
         write(*,*) 'PX4H1(I) = ', PX4H1(I)
         write(*,*) 'I = ', I
         pause 
         endif
       END DO
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
C   H2
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         H2X=X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I)
         H2Y=Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I)
         H2Z=Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I)
         HX=ONE_OVER_8*H2X
         HY=ONE_OVER_8*H2Y
         HZ=ONE_OVER_8*H2Z   
         PX1H2(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H2(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H2(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H2(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
         if (Write_flag == 1) THEN
         write(*,*) 'PX1H2(I) = ', PX1H2(I)
         write(*,*) 'PX2H2(I) = ', PX2H2(I)
         write(*,*) 'PX3H2(I) = ', PX3H2(I)
         write(*,*) 'PX4H2(I) = ', PX4H2(I)
         write(*,*) 'I = ', I
         !pause
         endif
       END DO
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
C   H4       
C -1 1 -1 1 1 -1 1 -1
       DO I=1,NEL
         H4X=-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I)
         H4Y=-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I)
         H4Z=-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I)   
         HX=ONE_OVER_8*H4X
         HY=ONE_OVER_8*H4Y
         HZ=ONE_OVER_8*H4Z   
         PX1H4(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H4(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H4(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H4(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ

         if (Write_flag == 1) THEN
         write(*,*) 'PX1H4(I) = ', PX1H4(I)
         write(*,*) 'PX2H4(I) = ', PX2H4(I)
         write(*,*) 'PX3H4(I) = ', PX3H4(I)
         write(*,*) 'PX4H4(I) = ', PX4H4(I)
         write(*,*) 'I = ', I
         !pause  
         endif
       END DO


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

C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//szderi3 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCccccccccccc
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
!!To Calculate q/dt
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!//szhour3       
      DO I=1,NEL
       VX3478=VX3(I)-VX4(I)-VX7(I)+VX8(I)
       VX2358=VX2(I)-VX3(I)-VX5(I)+VX8(I)
       VX1467=VX1(I)-VX4(I)-VX6(I)+VX7(I)
       VX1256=VX1(I)-VX2(I)-VX5(I)+VX6(I)
C
       VY3478=VY3(I)-VY4(I)-VY7(I)+VY8(I)
       VY2358=VY2(I)-VY3(I)-VY5(I)+VY8(I)
       VY1467=VY1(I)-VY4(I)-VY6(I)+VY7(I)
       VY1256=VY1(I)-VY2(I)-VY5(I)+VY6(I)
C
       VZ3478=VZ3(I)-VZ4(I)-VZ7(I)+VZ8(I)
       VZ2358=VZ2(I)-VZ3(I)-VZ5(I)+VZ8(I)
       VZ1467=VZ1(I)-VZ4(I)-VZ6(I)+VZ7(I)
       VZ1256=VZ1(I)-VZ2(I)-VZ5(I)+VZ6(I)
       if (Write_flag == 1) THEN
       write(*,*) 'VX1467',VX1467
       write(*,*) 'VX2358',VX2358
       write(*,*) 'HGX3',HGX3(I) 
       endif

       HGX3(I)=(VX1467-VX2358)*ONE_OVER_8


       HGX1(I)=(VX1467+VX2358)*ONE_OVER_8
       HGX2(I)=(VX1256-VX3478)*ONE_OVER_8
       HGX4(I)=-(VX1256+VX3478)*ONE_OVER_8       
C
       HGY3(I)=(VY1467-VY2358)*ONE_OVER_8
       HGY1(I)=(VY1467+VY2358)*ONE_OVER_8
       HGY2(I)=(VY1256-VY3478)*ONE_OVER_8
       HGY4(I)=-(VY1256+VY3478)*ONE_OVER_8       
C
       HGZ3(I)=(VZ1467-VZ2358)*ONE_OVER_8
       HGZ1(I)=(VZ1467+VZ2358)*ONE_OVER_8
       HGZ2(I)=(VZ1256-VZ3478)*ONE_OVER_8
       HGZ4(I)=-(VZ1256+VZ3478)*ONE_OVER_8      
!A VERIFIER    
       if (Write_flag == 1) THEN    

         write(*,*) 'Atention please !!!!! HGX3'
         write(*,*) 'HGX1',HGX1(I)
         write(*,*) 'HGX2',HGX2(I)
         write(*,*) 'HGX3',HGX3(I)
         write(*,*) 'HGX4',HGX4(I)

         write(*,*) 'HGY1',HGY1(I)
         write(*,*) 'HGY2',HGY2(I)
         write(*,*) 'HGY3',HGY3(I)
         write(*,*) 'HGY4',HGY4(I)

         write(*,*) 'HGZ1',HGZ1(I)
         write(*,*) 'HGZ2',HGZ2(I)
         write(*,*) 'HGZ3',HGZ3(I)
         write(*,*) 'HGZ4',HGZ4(I)
         write(*,*) 'I = ', I
       endif  
      ENDDO

      DO I=1,NEL
        VX17=VX1(I)-VX7(I)
        VX28=VX2(I)-VX8(I)
        VX35=VX3(I)-VX5(I)
        VX46=VX4(I)-VX6(I)
        VY17=VY1(I)-VY7(I)
        VY28=VY2(I)-VY8(I)
        VY35=VY3(I)-VY5(I)
        VY46=VY4(I)-VY6(I)
        VZ17=VZ1(I)-VZ7(I)
        VZ28=VZ2(I)-VZ8(I)
        VZ35=VZ3(I)-VZ5(I)
        VZ46=VZ4(I)-VZ6(I)
      



C   alpha =1 ->eta zeta   
C 1 1 -1 -1 -1 -1 1 1
!VY1467=VY1(I)-VY4(I)-VY6(I)+VY7(I)
!VZ2358=VZ2(I)-VZ3(I)-VZ5(I)+VZ8(I)
!HGX1(I)=(VX1467+VX2358)*ONE_OVER_8
     
        HGX1(I)= HGX1(I)
     &          -(PX1H1(I)*VX17+PX2H1(I)*VX28
     &            +PX3H1(I)*VX35+PX4H1(I)*VX46)
        HGY1(I)= HGY1(I)
     &          -(PX1H1(I)*VY17+PX2H1(I)*VY28
     &            +PX3H1(I)*VY35+PX4H1(I)*VY46)
        HGZ1(I)= HGZ1(I)
     &          -(PX1H1(I)*VZ17+PX2H1(I)*VZ28
     &            +PX3H1(I)*VZ35+PX4H1(I)*VZ46)
! write(*,*) 'HGX1',HGX1(I)
! write(*,*) 'HGY1',HGY1(I)
! write(*,*) 'HGZ1',HGZ1(I)
C   alpha =2 ->zeta ksi   
C 1 -1 -1 1 -1 1 1 -1
        HGX2(I)= HGX2(I)
     &          -(PX1H2(I)*VX17+PX2H2(I)*VX28
     &            +PX3H2(I)*VX35+PX4H2(I)*VX46)
        HGY2(I)= HGY2(I)
     &          -(PX1H2(I)*VY17+PX2H2(I)*VY28
     &            +PX3H2(I)*VY35+PX4H2(I)*VY46)
        HGZ2(I)= HGZ2(I)
     &          -(PX1H2(I)*VZ17+PX2H2(I)*VZ28
     &            +PX3H2(I)*VZ35+PX4H2(I)*VZ46)
! write(*,*) 'HGX2',HGX2(I)
! write(*,*) 'HGY2',HGY2(I)
! write(*,*) 'HGZ2',HGZ2(I)
C   alpha =3 ->ksi eta    
C 1 -1 1 -1 1 -1 1 -1
        HGX3(I)= HGX3(I)
     &          -(PX1H3(I)*VX17+PX2H3(I)*VX28
     &            +PX3H3(I)*VX35+PX4H3(I)*VX46)
        HGY3(I)= HGY3(I)
     &          -(PX1H3(I)*VY17+PX2H3(I)*VY28
     &            +PX3H3(I)*VY35+PX4H3(I)*VY46)
        HGZ3(I)= HGZ3(I)
     &          -(PX1H3(I)*VZ17+PX2H3(I)*VZ28
     &            +PX3H3(I)*VZ35+PX4H3(I)*VZ46)
!  write(*,*) 'HGX3',HGX3(I)
! write(*,*) 'HGY3',HGY3(I)
!  write(*,*) 'HGZ3',HGZ3(I)
C
C   alpha =4 ->ksi eta zeta
C -1 1 -1 1 1 -1 1 -1
        HGX4(I)= HGX4(I)
     &          -(PX1H4(I)*VX17+PX2H4(I)*VX28
     &            +PX3H4(I)*VX35+PX4H4(I)*VX46)
        HGY4(I)= HGY4(I)
     &          -(PX1H4(I)*VY17+PX2H4(I)*VY28
     &            +PX3H4(I)*VY35+PX4H4(I)*VY46)
        HGZ4(I)= HGZ4(I)
     &          -(PX1H4(I)*VZ17+PX2H4(I)*VZ28
     &            +PX3H4(I)*VZ35+PX4H4(I)*VZ46)
! write(*,*) 'HGX4',HGX4(I)
! write(*,*) 'HGY4',HGY4(I)
! write(*,*) 'HGZ4',HGZ4(I)
      if (Write_flag == 1) THEN
      write(*,*) 'HGX1',HGX1(I)
      write(*,*) 'HGX2',HGX2(I)
      write(*,*) 'HGX3',HGX3(I)
      write(*,*) 'HGX4',HGX4(I)

      write(*,*) 'HGY1',HGY1(I)
      write(*,*) 'HGY2',HGY2(I)
      write(*,*) 'HGY3',HGY3(I)
      write(*,*) 'HGY4',HGY4(I)

      write(*,*) 'HGZ1',HGZ1(I)
      write(*,*) 'HGZ2',HGZ2(I)
      write(*,*) 'HGZ3',HGZ3(I)
      write(*,*) 'HGZ4',HGZ4(I)

      write(*,*) 'I = ', I
      !pause
      endif 
      ENDDO
      !q!!!!!!! above
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DO I=1,NEL 
! write(*,*) 'a'
        JR0(I) = JAC1(I)
        JS0(I) = JAC5(I)
        JT0(I) = JAC9(I)
!  JAC1 R,         JAC5 S,         JAC9 T
      JR_1(I) = ONE/MAX(EM20,JR0(I))
      JS_1(I) = ONE/MAX(EM20,JS0(I))
      JT_1(I) = ONE/MAX(EM20,JT0(I))
      H11(I) = JS0(I)*JT0(I)*JR_1(I)
      H22(I) = JR0(I)*JT0(I)*JS_1(I)
      H33(I) = JR0(I)*JS0(I)*JT_1(I)
      H12(I) = JT0(I)
      H13(I) = JS0(I)
      H23(I) = JR0(I)
      if (Write_flag == 1) THEN
      write(*,*) 'JR0',JR0(I)
      write(*,*) 'JS0',JS0(I)
      write(*,*) 'JT0',JT0(I)

      write(*,*) 'JR_1',JR_1(I)
      write(*,*) 'JS_1',JS_1(I)
      write(*,*) 'JT_1',JT_1(I)
     

      write(*,*) 'H11',H11(I)
      write(*,*) 'H22',H22(I)
      write(*,*) 'H33',H33(I)
      write(*,*) 'H12',H12(I)
      write(*,*) 'H13',H13(I)
      write(*,*) 'H23',H23(I)

      write(*,*) 'I = ', I
      write(*,*) 'OFF',OFF(I)
      pause
      endif

!      !Hii
      ENDDO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc      
    !   DO I=1,NEL
    !    write(*,*) 'b'
    !    FHOUR(I,1,1) = FHOUR(I,1,1)*OFF(I)
    !    FHOUR(I,1,2) = FHOUR(I,1,2)*OFF(I)
    !    FHOUR(I,1,3) = FHOUR(I,1,3)*OFF(I)
    !    FHOUR(I,1,4) = FHOUR(I,1,4)*OFF(I)
    !    FHOUR(I,2,1) = FHOUR(I,2,1)*OFF(I)
    !    FHOUR(I,2,2) = FHOUR(I,2,2)*OFF(I)
    !    FHOUR(I,2,3) = FHOUR(I,2,3)*OFF(I)
    !    FHOUR(I,2,4) = FHOUR(I,2,4)*OFF(I)
    !    FHOUR(I,3,1) = FHOUR(I,3,1)*OFF(I)
    !    FHOUR(I,3,2) = FHOUR(I,3,2)*OFF(I)
    !    FHOUR(I,3,3) = FHOUR(I,3,3)*OFF(I)
    !    FHOUR(I,3,4) = FHOUR(I,3,4)*OFF(I)
    !   ENDDO
      DO I=1,NEL
!        !//TODO: A VERIFIER G_3DT         
        E_R =G_3DT(I)*JR_1(I)
        E_S =G_3DT(I)*JS_1(I)
        E_T =G_3DT(I)*JT_1(I)

        
        if (Write_flag == 1) THEN
        write(*,*) 'I = ', I
        write(*,*) 'E_R = ', E_R
        write(*,*) 'E_S = ', E_S
        write(*,*) 'E_T = ', E_T
        endif

        DFHOUR(I,1,1) = E_R*HGX1(I)
        DFHOUR(I,1,2) = E_R*HGX2(I)
        DFHOUR(I,1,3) = E_R*HGX3(I)
        DFHOUR(I,1,4) = E_R*HGX4(I)

        DFHOUR(I,2,1) = E_S*HGY1(I)
        DFHOUR(I,2,2) = E_S*HGY2(I)
        DFHOUR(I,2,3) = E_S*HGY3(I)
        DFHOUR(I,2,4) = E_S*HGY4(I)
      
        DFHOUR(I,3,1) = E_T*HGZ1(I)
        DFHOUR(I,3,2) = E_T*HGZ2(I)
        DFHOUR(I,3,3) = E_T*HGZ3(I)
        DFHOUR(I,3,4) = E_T*HGZ4(I)  

        if (Write_flag == 1) THEN   
        write(*,*) 'FHOUR(I,1,1) = ',FHOUR(I,1,1)
        write(*,*) 'FHOUR(I,1,2) = ',FHOUR(I,1,2)
        write(*,*) 'FHOUR(I,1,3) = ',FHOUR(I,1,3)
        write(*,*) 'FHOUR(I,1,4) = ',FHOUR(I,1,4)
        write(*,*) 'FHOUR(I,2,1) = ',FHOUR(I,2,1)
        write(*,*) 'FHOUR(I,2,2) = ',FHOUR(I,2,2)
        write(*,*) 'FHOUR(I,2,3) = ',FHOUR(I,2,3)
        write(*,*) 'FHOUR(I,2,4) = ',FHOUR(I,2,4)
        write(*,*) 'FHOUR(I,3,1) = ',FHOUR(I,3,1)
        write(*,*) 'FHOUR(I,3,2) = ',FHOUR(I,3,2)
        write(*,*) 'FHOUR(I,3,3) = ',FHOUR(I,3,3)
        write(*,*) 'FHOUR(I,3,4) = ',FHOUR(I,3,4)

        write(*,*) 'DFHOUR(I,1,1) = ',DFHOUR(I,1,1)
        write(*,*) 'DFHOUR(I,1,2) = ',DFHOUR(I,1,2)
        write(*,*) 'DFHOUR(I,1,3) = ',DFHOUR(I,1,3)
        write(*,*) 'DFHOUR(I,1,4) = ',DFHOUR(I,1,4)
        write(*,*) 'DFHOUR(I,2,1) = ',DFHOUR(I,2,1)
        write(*,*) 'DFHOUR(I,2,2) = ',DFHOUR(I,2,2)
        write(*,*) 'DFHOUR(I,2,3) = ',DFHOUR(I,2,3)
        write(*,*) 'DFHOUR(I,2,4) = ',DFHOUR(I,2,4)
        write(*,*) 'DFHOUR(I,3,1) = ',DFHOUR(I,3,1)
        write(*,*) 'DFHOUR(I,3,2) = ',DFHOUR(I,3,2)
        write(*,*) 'DFHOUR(I,3,3) = ',DFHOUR(I,3,3)
        write(*,*) 'DFHOUR(I,3,4) = ',DFHOUR(I,3,4)
        endif
C
        FHOUR(I,1,1) = FHOUR(I,1,1) + DFHOUR(I,1,1)
        FHOUR(I,1,2) = FHOUR(I,1,2) + DFHOUR(I,1,2)
        FHOUR(I,1,3) = FHOUR(I,1,3) + DFHOUR(I,1,3)
        FHOUR(I,1,4) = FHOUR(I,1,4) + DFHOUR(I,1,4)
        FHOUR(I,2,1) = FHOUR(I,2,1) + DFHOUR(I,2,1)
        FHOUR(I,2,2) = FHOUR(I,2,2) + DFHOUR(I,2,2)
        FHOUR(I,2,3) = FHOUR(I,2,3) + DFHOUR(I,2,3)
        FHOUR(I,2,4) = FHOUR(I,2,4) + DFHOUR(I,2,4)
        FHOUR(I,3,1) = FHOUR(I,3,1) + DFHOUR(I,3,1)
        FHOUR(I,3,2) = FHOUR(I,3,2) + DFHOUR(I,3,2)
        FHOUR(I,3,3) = FHOUR(I,3,3) + DFHOUR(I,3,3)
        FHOUR(I,3,4) = FHOUR(I,3,4) + DFHOUR(I,3,4)
        
        if (Write_flag == 1) THEN   
        write(*,*) 'FHOUR(I,1,1) = ',FHOUR(I,1,1)
        write(*,*) 'FHOUR(I,1,2) = ',FHOUR(I,1,2)
        write(*,*) 'FHOUR(I,1,3) = ',FHOUR(I,1,3)
        write(*,*) 'FHOUR(I,1,4) = ',FHOUR(I,1,4)
 
        write(*,*) 'FHOUR(I,2,1) = ',FHOUR(I,2,1)
        write(*,*) 'FHOUR(I,2,2) = ',FHOUR(I,2,2)
        write(*,*) 'FHOUR(I,2,3) = ',FHOUR(I,2,3)
        write(*,*) 'FHOUR(I,2,4) = ',FHOUR(I,2,4)
 
        write(*,*) 'FHOUR(I,3,1) = ',FHOUR(I,3,1)
        write(*,*) 'FHOUR(I,3,2) = ',FHOUR(I,3,2)
        write(*,*) 'FHOUR(I,3,3) = ',FHOUR(I,3,3)
        write(*,*) 'FHOUR(I,3,4) = ',FHOUR(I,3,4)
        
        endif 
        !pause
      ENDDO
      !!pause
      DO I=1,NEL

!       write(*,*) 'FHOUR(I,1,1)*JR0(I)', FHOUR(I,1,1)*JR0(I)  
!       write(*,*) 'FCL(I)*HGX1(I)', FCL(I)*HGX1(I)  
!!       write(*,*) 'FHOUR(I,2,2)*JR0(I)', FHOUR(I,2,2)*JR0(I)  
!       write(*,*) 'FCL(I)*HGY2(I)', FCL(I)*HGY2(I) 
!       write(*,*) 'FHOUR(I,3,3)*JR0(I)', FHOUR(I,3,3)*JR0(I)  
!       write(*,*) 'FCL(I)*HGZ3(I)', FCL(I)*HGZ3(I)
!       !//TODO:FCL
        FHOURT(1,1) = FHOUR(I,1,1)*JR0(I)+FCL(I)*HGX1(I)
        FHOURT(1,2) = FHOUR(I,1,2)*JR0(I)+FCL(I)*HGX2(I)
        FHOURT(1,3) = FHOUR(I,1,3)*JR0(I)+FCL(I)*HGX3(I)
        FHOURT(1,4) = FHOUR(I,1,4)*JR0(I)+FCL(I)*HGX4(I)
        FHOURT(2,1) = FHOUR(I,2,1)*JS0(I)+FCL(I)*HGY1(I)
        FHOURT(2,2) = FHOUR(I,2,2)*JS0(I)+FCL(I)*HGY2(I)
        FHOURT(2,3) = FHOUR(I,2,3)*JS0(I)+FCL(I)*HGY3(I)
        FHOURT(2,4) = FHOUR(I,2,4)*JS0(I)+FCL(I)*HGY4(I)
        FHOURT(3,1) = FHOUR(I,3,1)*JT0(I)+FCL(I)*HGZ1(I)
        FHOURT(3,2) = FHOUR(I,3,2)*JT0(I)+FCL(I)*HGZ2(I)
        FHOURT(3,3) = FHOUR(I,3,3)*JT0(I)+FCL(I)*HGZ3(I)
        FHOURT(3,4) = FHOUR(I,3,4)*JT0(I)+FCL(I)*HGZ4(I)
        if (Write_flag == 1) THEN
        write(*,*) 'FHOURT(1,1) = ',FHOURT(1,1)
        write(*,*) 'FHOURT(1,2) = ',FHOURT(1,2)
        write(*,*) 'FHOURT(1,3) = ',FHOURT(1,3)
        write(*,*) 'FHOURT(1,4) = ',FHOURT(1,4)
 
        write(*,*) 'FHOURT(2,1) = ',FHOURT(2,1)
        write(*,*) 'FHOURT(2,2) = ',FHOURT(2,2)
        write(*,*) 'FHOURT(2,3) = ',FHOURT(2,3)
        write(*,*) 'FHOURT(2,4) = ',FHOURT(2,4)
 
        write(*,*) 'FHOURT(3,1) = ',FHOURT(3,1)
        write(*,*) 'FHOURT(3,2) = ',FHOURT(3,2)
        write(*,*) 'FHOURT(3,3) = ',FHOURT(3,3)
        write(*,*) 'FHOURT(3,4) = ',FHOURT(3,4)
        endif
C NFHX1 NFHOUR(I,3(XYZ),4(1,2,3,4))
!//TODO:NU1 NNU2 NU3
!      !NFHOUR(I,1,1) NFHX1(I)
        NFHOUR(I,1,1) = (H22(I)+H33(I))*FHOURT(1,1)
     .              +H12(I)*FHOURT(2,2)+H13(I)*FHOURT(3,3)
       !NFHOUR(I,2,2) NFHY2(I)
        NFHOUR(I,2,2) = (H11(I)+H33(I))*FHOURT(2,2)
     .              +H23(I)*FHOURT(3,3)+H12(I)*FHOURT(1,1)
       !NFHOUR(I,3,3) NFHZ3(I)
        NFHOUR(I,3,3) = (H11(I)+H22(I))*FHOURT(3,3)
     .              +H13(I)*FHOURT(1,1)+H23(I)*FHOURT(2,2)
       !NFHOUR(I,1,2) NFHX2(I)
        NFHOUR(I,1,2) = NU1(I)*H11(I)*FHOURT(1,2)
     .             +NU2(I)*H12(I)*FHOURT(2,1)
       !NFHOUR(I,1,3) NFHX3(I)
        NFHOUR(I,1,3) = NU1(I)*H11(I)*FHOURT(1,3)
     .             +NU2(I)*H13(I)*FHOURT(3,1)
       !NFHOUR(I,2,1) NFHY1(I)
        NFHOUR(I,2,1) = NU1(I)*H22(I)*FHOURT(2,1)
     .             +NU2(I)*H12(I)*FHOURT(1,2)
       !NFHOUR(I,3,1)  NFHZ1(I) NFHZ1(I) = CC*FHOUR(I,1) + FCL(I)*HGZ1(I)
        NFHOUR(I,3,1) = NU1(I)*H33(I)*FHOURT(3,1)
     .             +NU2(I)*H13(I)*FHOURT(1,3)
       !NFHOUR(I,2,3) NFHY3(I)
        NFHOUR(I,2,3) = NU1(I)*H22(I)*FHOURT(2,3)
     .             +NU2(I)*H23(I)*FHOURT(3,2)
       !NFHOUR(I,3,2)  NFHZ2(I) NFHZ2(I) = CC*FHOUR(I,2) + FCL(I)*HGZ2(I)
        NFHOUR(I,3,2)= NU1(I)*H33(I)*FHOURT(3,2)
     .             +NU2(I)*H23(I)*FHOURT(2,3)
       !NFHOUR(I,1,4) NFHX4(I)
        NFHOUR(I,1,4) = NU3(I)*H11(I)*FHOURT(1,4)
       !NFHOUR(I,2,4) NFHY4(I)
        NFHOUR(I,2,4) = NU3(I)*H22(I)*FHOURT(2,4)
       !NFHOUR(I,3,4)  NFHZ4(I)
        NFHOUR(I,3,4) = NU3(I)*H33(I)*FHOURT(3,4)
        
        if (Write_flag == 1) THEN
      write(*,*) 'NFHOUR(I,1,1) = ',NFHOUR(I,1,1)
      write(*,*) 'NFHOUR(I,1,2) = ',NFHOUR(I,1,2)
      write(*,*) 'NFHOUR(I,1,3) = ',NFHOUR(I,1,3)
      write(*,*) 'NFHOUR(I,1,4) = ',NFHOUR(I,1,4)
 
      write(*,*) 'NFHOUR(I,2,1) = ',NFHOUR(I,2,1)
      write(*,*) 'NFHOUR(I,2,2) = ',NFHOUR(I,2,2)
      write(*,*) 'NFHOUR(I,2,3) = ',NFHOUR(I,2,3)
      write(*,*) 'NFHOUR(I,2,4) = ',NFHOUR(I,2,4)
 
      write(*,*) 'NFHOUR(I,3,1) = ',NFHOUR(I,3,1)
      write(*,*) 'NFHOUR(I,3,2) = ',NFHOUR(I,3,2)
      write(*,*) 'NFHOUR(I,3,3) = ',NFHOUR(I,3,3)
      write(*,*) 'NFHOUR(I,3,4) = ',NFHOUR(I,3,4)
      
      write(*,*) 'I = ', I
        endif
      ENDDO
      !!pause
      
      DO I=1,NEL
        HQ13P = (NFHOUR(I,1,1)+NFHOUR(I,1,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,1,1)-NFHOUR(I,1,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,1,2)+NFHOUR(I,1,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,1,2)-NFHOUR(I,1,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,1,1)-PX1H2(I)*NFHOUR(I,1,2)
     .      -PX1H3(I)*NFHOUR(I,1,3)-PX1H4(I)*NFHOUR(I,1,4)
        F11_hgl(I) =-(HQ13P+HQ24N+FF)
        F17_hgl(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,1,1)-PX2H2(I)*NFHOUR(I,1,2)
     .      -PX2H3(I)*NFHOUR(I,1,3)-PX2H4(I)*NFHOUR(I,1,4)
        F12_hgl(I) =-(HQ13N-HQ24N+FF)
        F18_hgl(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,1,1)-PX3H2(I)*NFHOUR(I,1,2)
     .      -PX3H3(I)*NFHOUR(I,1,3)-PX3H4(I)*NFHOUR(I,1,4)
        F13_hgl(I) =-(-HQ13N-HQ24P+FF)
        F15_hgl(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,1,1)-PX4H2(I)*NFHOUR(I,1,2)
     .      -PX4H3(I)*NFHOUR(I,1,3)-PX4H4(I)*NFHOUR(I,1,4)
        F14_hgl(I) =-(-HQ13P+HQ24P+FF)
        F16_hgl(I) =-(-HQ13P+HQ24N-FF)
       ENDDO
       DO I=1,NEL
        HQ13P = (NFHOUR(I,2,1)+NFHOUR(I,2,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,2,1)-NFHOUR(I,2,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,2,2)+NFHOUR(I,2,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,2,2)-NFHOUR(I,2,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,2,1)-PX1H2(I)*NFHOUR(I,2,2)
     .      -PX1H3(I)*NFHOUR(I,2,3)-PX1H4(I)*NFHOUR(I,2,4)
        F21_hgl(I) =-(HQ13P+HQ24N+FF)
        F27_hgl(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,2,1)-PX2H2(I)*NFHOUR(I,2,2)
     .      -PX2H3(I)*NFHOUR(I,2,3)-PX2H4(I)*NFHOUR(I,2,4)
        F22_hgl(I) =-(HQ13N-HQ24N+FF)
        F28_hgl(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,2,1)-PX3H2(I)*NFHOUR(I,2,2)
     .      -PX3H3(I)*NFHOUR(I,2,3)-PX3H4(I)*NFHOUR(I,2,4)
        F23_hgl(I) =-(-HQ13N-HQ24P+FF)
        F25_hgl(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,2,1)-PX4H2(I)*NFHOUR(I,2,2)
     .      -PX4H3(I)*NFHOUR(I,2,3)-PX4H4(I)*NFHOUR(I,2,4)
        F24_hgl(I) =-(-HQ13P+HQ24P+FF)
        F26_hgl(I) =-(-HQ13P+HQ24N-FF)
       ENDDO
       DO I=1,NEL
        HQ13P = (NFHOUR(I,3,1)+NFHOUR(I,3,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,3,1)-NFHOUR(I,3,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,3,2)+NFHOUR(I,3,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,3,2)-NFHOUR(I,3,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,3,1)-PX1H2(I)*NFHOUR(I,3,2)
     .      -PX1H3(I)*NFHOUR(I,3,3)-PX1H4(I)*NFHOUR(I,3,4)
        F31_hgl(I) =-(HQ13P+HQ24N+FF)
        F37_hgl(I) =-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,3,1)-PX2H2(I)*NFHOUR(I,3,2)
     .      -PX2H3(I)*NFHOUR(I,3,3)-PX2H4(I)*NFHOUR(I,3,4)
        F32_hgl(I) =-(HQ13N-HQ24N+FF)
        F38_hgl(I) =-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,3,1)-PX3H2(I)*NFHOUR(I,3,2)
     .      -PX3H3(I)*NFHOUR(I,3,3)-PX3H4(I)*NFHOUR(I,3,4)
        F33_hgl(I) =-(-HQ13N-HQ24P+FF)
        F35_hgl(I) =-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,3,1)-PX4H2(I)*NFHOUR(I,3,2)
     .      -PX4H3(I)*NFHOUR(I,3,3)-PX4H4(I)*NFHOUR(I,3,4)
        F34_hgl(I) =-(-HQ13P+HQ24P+FF)
        F36_hgl(I) =-(-HQ13P+HQ24N-FF)
       ENDDO
C------------------------------------------------
C

      DO I=1,NEL
         if (Write_flag == 1) then
          write(*,*) 'I = ', I
          write(*,*) 'F11_hgl(I) = ', F11_hgl(I)
          write(*,*) 'F21_hgl(I) = ', F21_hgl(I)
          write(*,*) 'F31_hgl(I) = ', F31_hgl(I) 
          write(*,*) 'F12_hgl(I) = ', F12_hgl(I)
          write(*,*) 'F22_hgl(I) = ', F22_hgl(I)
          write(*,*) 'F32_hgl(I) = ', F32_hgl(I) 
          write(*,*) 'F13_hgl(I) = ', F13_hgl(I)   
          write(*,*) 'F23_hgl(I) = ', F23_hgl(I)        
          write(*,*) 'F33_hgl(I) = ', F33_hgl(I) 
          write(*,*) 'F14_hgl(I) = ', F14_hgl(I)
          write(*,*) 'F24_hgl(I) = ', F24_hgl(I)
          write(*,*) 'F34_hgl(I) = ', F34_hgl(I) 
          write(*,*) 'F15_hgl(I) = ', F15_hgl(I)
          write(*,*) 'F25_hgl(I) = ', F25_hgl(I)
          write(*,*) 'F35_hgl(I) = ', F35_hgl(I) 
          write(*,*) 'F16_hgl(I) = ', F16_hgl(I)  
          write(*,*) 'F26_hgl(I) = ', F26_hgl(I)
          write(*,*) 'F36_hgl(I) = ', F36_hgl(I) 
          write(*,*) 'F17_hgl(I) = ', F17_hgl(I)
          write(*,*) 'F27_hgl(I) = ', F27_hgl(I)
          write(*,*) 'F37_hgl(I) = ', F37_hgl(I)          
          write(*,*) 'F18_hgl(I) = ', F18_hgl(I)
          write(*,*) 'F28_hgl(I) = ', F28_hgl(I)
          write(*,*) 'F38_hgl(I) = ', F38_hgl(I)
          endif 
       
      enddo

      DO I=1,NEL
           
          if (hourglass == 0) THEN
          F11_hgl(I)=0.
          F21_hgl(I)=0.
          F31_hgl(I)=0. 
          F12_hgl(I)=0.
          F22_hgl(I)=0.
          F32_hgl(I)=0. 
          F13_hgl(I)=0.   
          F23_hgl(I)=0.        
          F33_hgl(I)=0. 
          F14_hgl(I)=0.
          F24_hgl(I)=0.
          F34_hgl(I)=0. 
          F15_hgl(I)=0.
          F25_hgl(I)=0.
          F35_hgl(I)=0. 
          F16_hgl(I)=0.  
          F26_hgl(I)=0.
          F36_hgl(I)=0. 
          F17_hgl(I)=0.
          F27_hgl(I)=0.
          F37_hgl(I)=0.       
          F18_hgl(I)=0.
          F28_hgl(I)=0.
          F38_hgl(I)=0. 

!          write(*,*) 'Ncycle =', NC_DEBUG 
!          PAUSE
          endif

        F11(I) =F11(I)+F11_hgl(I)    
        F12(I) =F12(I)+F12_hgl(I)
        F13(I) =F13(I)+F13_hgl(I) + F14_hgl(I) ! F14                
        F15(I) =F15(I)+F15_hgl(I) 
        F16(I) =F16(I)+F16_hgl(I)
        F17(I) =F17(I)+F17_hgl(I) + F18_hgl(I) ! F18             
        F21(I) =F21(I)+F21_hgl(I)
        F22(I) =F22(I)+F22_hgl(I)
        F23(I) =F23(I)+F23_hgl(I) + F24_hgl(I) ! F24               
        F25(I) =F25(I)+F25_hgl(I) 
        F26(I) =F26(I)+F26_hgl(I)
        F27(I) =F27(I)+F27_hgl(I) + F28_hgl(I) ! F28
        F31(I) =F31(I)+F31_hgl(I)
        F32(I) =F32(I)+F32_hgl(I)
        F33(I) =F33(I)+F33_hgl(I) + F34_hgl(I) ! F34          
        F35(I) =F35(I)+F35_hgl(I)
        F36(I) =F36(I)+F36_hgl(I)
        F37(I) =F37(I)+F37_hgl(I) + F38_hgl(I) ! F38

      !F11(I) =F11(I) ! F14     
      !F12(I) =F12(I)
      !F13(I) =F13(I)                 
      !F15(I) =F15(I) ! F18
      !F16(I) =F16(I)
      !F17(I) =F17(I)
      !F21(I) =F21(I) ! F24
      !F22(I) =F22(I)
      !F23(I) =F23(I)             
      !F25(I) =F25(I) ! F28
      !F26(I) =F26(I)
      !F27(I) =F27(I)
      !F31(I) =F31(I) ! F34
      !F32(I) =F32(I)
      !F33(I) =F33(I)               
      !F35(I) =F35(I) ! F38
      !F36(I) =F36(I)
      !F37(I) =F37(I)       
                  
 
      ENDDO

      
C------------------------------------------------
C
!      DO I=1,NEL
!        F11(I) =F11(I)-G11(I)*NFHX1(I)-G12(I)*NFHX2(I)-G13(I)*NFHX3(I)-G14(I)*NFHX4(I)
!        F12(I) =F12(I)-G21(I)*NFHX1(I)-G22(I)*NFHX2(I)-G23(I)*NFHX3(I)-G24(I)*NFHX4(I)
!        F13(I) =F13(I)-G31(I)*NFHX1(I)-G32(I)*NFHX2(I)-G33(I)*NFHX3(I)-G34(I)*NFHX4(I)
!     .                -G41(I)*NFHX1(I)-G42(I)*NFHX2(I)-G43(I)*NFHX3(I)-G44(I)*NFHX4(I) ! F14        
!        F15(I) =F15(I)-G51(I)*NFHX1(I)-G52(I)*NFHX2(I)-G53(I)*NFHX3(I)-G54(I)*NFHX4(I)
!        F16(I) =F16(I)-G61(I)*NFHX1(I)-G62(I)*NFHX2(I)-G63(I)*NFHX3(I)-G64(I)*NFHX4(I)
!        F17(I) =F17(I)-G71(I)*NFHX1(I)-G72(I)*NFHX2(I)-G73(I)*NFHX3(I)-G74(I)*NFHX4(I)
!     .                -G81(I)*NFHX1(I)-G82(I)*NFHX2(I)-G83(I)*NFHX3(I)-G84(I)*NFHX4(I) ! F18



!        F21(I) =F21(I)-G11(I)*NFHY1(I)-G12(I)*NFHY2(I)-G13(I)*NFHY3(I)-G14(I)*NFHY4(I)
!        F22(I) =F22(I)-G21(I)*NFHY1(I)-G22(I)*NFHY2(I)-G23(I)*NFHY3(I)-G24(I)*NFHY4(I)
!        F23(I) =F23(I)-G31(I)*NFHY1(I)-G32(I)*NFHY2(I)-G33(I)*NFHY3(I)-G34(I)*NFHY4(I)
!     .                -G41(I)*NFHY1(I)-G42(I)*NFHY2(I)-G43(I)*NFHY3(I)-G44(I)*NFHY4(I) ! F24
!        F25(I) =F25(I)-G51(I)*NFHY1(I)-G52(I)*NFHY2(I)-G53(I)*NFHY3(I)-G54(I)*NFHY4(I)
!        F26(I) =F26(I)-G61(I)*NFHY1(I)-G62(I)*NFHY2(I)-G63(I)*NFHY3(I)-G64(I)*NFHY4(I)
!        F27(I) =F27(I)-G71(I)*NFHY1(I)-G72(I)*NFHY2(I)-G73(I)*NFHY3(I)-G74(I)*NFHY4(I)
!     .                -G81(I)*NFHY1(I)-G82(I)*NFHY2(I)-G83(I)*NFHY3(I)-G84(I)*NFHY4(I) ! F28!



!        F31(I) =F31(I)-G11(I)*NFHZ1(I)-G12(I)*NFHZ2(I)-G13(I)*NFHZ3(I)-G14(I)*NFHZ4(I)
!        F32(I) =F32(I)-G21(I)*NFHZ1(I)-G22(I)*NFHZ2(I)-G23(I)*NFHZ3(I)-G24(I)*NFHZ4(I)
!        F33(I) =F33(I)-G31(I)*NFHZ1(I)-G32(I)*NFHZ2(I)-G33(I)*NFHZ3(I)-G34(I)*NFHZ4(I)
!     .                -G41(I)*NFHZ1(I)-G42(I)*NFHZ2(I)-G43(I)*NFHZ3(I)-G44(I)*NFHZ4(I) ! F34
!        F35(I) =F35(I)-G51(I)*NFHZ1(I)-G52(I)*NFHZ2(I)-G53(I)*NFHZ3(I)-G54(I)*NFHZ4(I)
!        F36(I) =F36(I)-G61(I)*NFHZ1(I)-G62(I)*NFHZ2(I)-G63(I)*NFHZ3(I)-G64(I)*NFHZ4(I)
!        F37(I) =F37(I)-G71(I)*NFHZ1(I)-G72(I)*NFHZ2(I)-G73(I)*NFHZ3(I)-G74(I)*NFHZ4(I)
!     .                -G81(I)*NFHZ1(I)-G82(I)*NFHZ2(I)-G83(I)*NFHZ3(I)-G84(I)*NFHZ4(I) ! F38
 
!      ENDDO
      DO I=1,NEL
        EINT(I)= EINT(I)+DT1*(
     .     NFHZ1(I)*HGZ1(I) + NFHZ2(I)*HGZ2(I) ) 
     .   /MAX(EM20,VOL0(I)) 
      ENDDO
C-----------
      RETURN
C-----------
      END SUBROUTINE S6CHOUR3_1
