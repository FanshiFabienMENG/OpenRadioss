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
      !||    s6rcoor3    ../engine/source/elements/thickshell/solide6c/s6rcoor3.F
      !||--- called by ------------------------------------------------------
      !||    s6cforc3    ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||--- calls      -----------------------------------------------------
      !||    s6cortho3   ../engine/source/elements/thickshell/solide6c/s6cortho3.F
      !||    s6proj3     ../engine/source/elements/thickshell/solide6c/s6proj3.F
      !||    vrrota3     ../engine/source/elements/thickshell/solide6c/vrrota3.F
      !||====================================================================
      SUBROUTINE S6RCOOR3_1(
     .   X  ,IXS , V   ,GAMA0,GAMA,
     .   X1 , X2 , X3 , X4 , X5 ,  X6   ,
     .   Y1 , Y2 , Y3 , Y4 , Y5 ,  Y6   ,
     .   Z1 , Z2 , Z3 , Z4 , Z5 ,  Z6   ,
     .   VX1, VX2, VX3, VX4, VX5, VX6   , 
     .   VY1, VY2, VY3, VY4, VY5, VY6   ,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6   ,
     .   VD2,VIS,OFFG,OFF,SAV,RHO,RHOO  ,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NGL,MXT,NGEO,
     .   IOUTPRT, VGAX, VGAY, VGAZ, VGA2,DI,
     .   NEL, XGAX, XGAY, XGAZ, XGXA2, XGYA2, XGZA2,
     .   XGXYA, XGYZA, XGZXA, IPARG,GAMA_R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*),V(3,*), VIS(*),GAMA(MVSIZ,6), GAMA0(NEL,6),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), 
     .  VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), 
     .  VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), 
     .  VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), 
     .  VD2(*), OFFG(*), OFF(*),  RHO(*), RHOO(*),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) ,
     .   VGAX(*), VGAY(*), VGAZ(*), VGA2(*),DI(MVSIZ,6),
     .   XGAX(*), XGAY(*), XGAZ(*),
     .   XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ),
     .   XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ),GAMA_R(NEL,6)
      DOUBLE PRECISION
     .  SAV(NEL,15) !//?
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*),
     .        NC5(*), NC6(*),  MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*),IOUTPRT,IPARG(*)
CMasParINCLUDE 'scoor3.intmap.inc'
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,Write_flag
C     REAL
      my_real
     .   DT05
      my_real
     .   G11(MVSIZ),G12(MVSIZ),G13(MVSIZ),
     .   G21(MVSIZ),G22(MVSIZ),G23(MVSIZ),
     .   G31(MVSIZ),G32(MVSIZ),G33(MVSIZ),
     .   T11(MVSIZ),T12(MVSIZ),T13(MVSIZ),
     .   T21(MVSIZ),T22(MVSIZ),T23(MVSIZ),
     .   T31(MVSIZ),T32(MVSIZ),T33(MVSIZ)
      my_real
     .   XL,YL,ZL
      my_real
     .   XX,YY,ZZ,XY,XZ,YZ,RTR(6),ABC,XXYZ2,ZZXY2,YYXZ2,DETA   
      my_real
     .   OFF_L

!SAV(NEL,21), !//?
      DOUBLE PRECISION 
     .   XDP(3,MVSIZ),X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8),
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ), YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ), 
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ), ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),      
     .   VXD1(MVSIZ), VXD2(MVSIZ), VXD3(MVSIZ), VXD4(MVSIZ),
     .   VXD5(MVSIZ), VXD6(MVSIZ), VXD7(MVSIZ), VXD8(MVSIZ), 
     .   VYD1(MVSIZ), VYD2(MVSIZ), VYD3(MVSIZ), VYD4(MVSIZ), 
     .   VYD5(MVSIZ), VYD6(MVSIZ), VYD7(MVSIZ), VYD8(MVSIZ), 
     .   VZD1(MVSIZ), VZD2(MVSIZ), VZD3(MVSIZ), VZD4(MVSIZ), 
     .   VZD5(MVSIZ), VZD6(MVSIZ), VZD7(MVSIZ), VZD8(MVSIZ)       
      DOUBLE PRECISION
     .   XDL, YDL, ZDL
C-----------------------------------------------
      OFF_L  = ZERO 
      Write_flag = 0
      DO I=LFT,LLT
      VIS(I)=ZERO
      NGEO(I)=IXS(10,I)
      NGL(I)=IXS(11,I)
      MXT(I)=IXS(1,I)
      NC1(I)=IXS(2,I)
      NC2(I)=IXS(3,I)
      NC3(I)=IXS(4,I)
      NC4(I)=IXS(6,I)
      NC5(I)=IXS(7,I)
      NC6(I)=IXS(8,I)
      RHOO(I)=RHO(I)
      ENDDO
!OK
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO I=LFT,LLT
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
        X5(I)=X(1,NC5(I))
        Y5(I)=X(2,NC5(I))
        Z5(I)=X(3,NC5(I))
        X6(I)=X(1,NC6(I))
        Y6(I)=X(2,NC6(I))
        Z6(I)=X(3,NC6(I))
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))

        !//TODO:Conversion from PENTA to Degenerated Solid 24
        XD1(I)=X1(I)
        YD1(I)=Y1(I)
        ZD1(I)=Z1(I)

        XD2(I)=X2(I)
        YD2(I)=Y2(I)
        ZD2(I)=Z2(I)

        XD3(I)=X3(I)
        YD3(I)=Y3(I)
        ZD3(I)=Z3(I)

        XD4(I)=X3(I)  
        YD4(I)=Y3(I)  
        ZD4(I)=Z3(I)  

        XD5(I)=X4(I)  
        YD5(I)=Y4(I)  
        ZD5(I)=Z4(I)  

        XD6(I)=X5(I)  
        YD6(I)=Y5(I)  
        ZD6(I)=Z5(I)  

        XD7(I)=X6(I)
        YD7(I)=Y6(I)
        ZD7(I)=Z6(I)

        XD8(I)=X6(I)  
        YD8(I)=Y6(I)  
        ZD8(I)=Z6(I)
        
      if (Write_flag == 1) then
        write(*,*) 'I = ', I
        write(*,*) 'XD1(I) = ', XD1(I)
        write(*,*) 'XD2(I) = ', XD2(I)
        write(*,*) 'XD3(I) = ', XD3(I)
        write(*,*) 'XD4(I) = ', XD4(I)
        write(*,*) 'XD5(I) = ', XD5(I)
        write(*,*) 'XD6(I) = ', XD6(I)
        write(*,*) 'XD7(I) = ', XD7(I)
        write(*,*) 'XD8(I) = ', XD8(I)
        write(*,*) 'YD1(I) = ', YD1(I)
        write(*,*) 'YD2(I) = ', YD2(I)
        write(*,*) 'YD3(I) = ', YD3(I)
        write(*,*) 'YD4(I) = ', YD4(I)
        write(*,*) 'YD5(I) = ', YD5(I)
        write(*,*) 'YD6(I) = ', YD6(I)
        write(*,*) 'YD7(I) = ', YD7(I)
        write(*,*) 'YD8(I) = ', YD8(I)
        write(*,*) 'ZD1(I) = ', ZD1(I)
        write(*,*) 'ZD2(I) = ', ZD2(I)
        write(*,*) 'ZD3(I) = ', ZD3(I)
        write(*,*) 'ZD4(I) = ', ZD4(I)
        write(*,*) 'ZD5(I) = ', ZD5(I)
        write(*,*) 'ZD6(I) = ', ZD6(I)
        write(*,*) 'ZD7(I) = ', ZD7(I)
        write(*,*) 'ZD8(I) = ', ZD8(I)
        endif
      ENDDO
C-----------
      DO I=LFT,LLT
       VX1(I)=V(1,NC1(I))
       VY1(I)=V(2,NC1(I))
       VZ1(I)=V(3,NC1(I))
       VX2(I)=V(1,NC2(I))
       VY2(I)=V(2,NC2(I))
       VZ2(I)=V(3,NC2(I))
       VX3(I)=V(1,NC3(I))
       VY3(I)=V(2,NC3(I))
       VZ3(I)=V(3,NC3(I))
       VX4(I)=V(1,NC4(I))
       VY4(I)=V(2,NC4(I))
       VZ4(I)=V(3,NC4(I))
       VX5(I)=V(1,NC5(I))
       VY5(I)=V(2,NC5(I))
       VZ5(I)=V(3,NC5(I))
       VX6(I)=V(1,NC6(I))
       VY6(I)=V(2,NC6(I))
       VZ6(I)=V(3,NC6(I))

       VXD1(I)=VX1(I)
       VYD1(I)=VY1(I)
       VZD1(I)=VZ1(I)

       VXD2(I)=VX2(I)
       VYD2(I)=VY2(I)
       VZD2(I)=VZ2(I)

       VXD3(I)=VX3(I)
       VYD3(I)=VY3(I)
       VZD3(I)=VZ3(I)

       VXD4(I)=VX3(I)  
       VYD4(I)=VY3(I)  
       VZD4(I)=VZ3(I)  

       VXD5(I)=VX4(I)  
       VYD5(I)=VY4(I)  
       VZD5(I)=VZ4(I)  

       VXD6(I)=VX5(I)  
       VYD6(I)=VY5(I)  
       VZD6(I)=VZ5(I)  

       VXD7(I)=VX6(I)
       VYD7(I)=VY6(I)
       VZD7(I)=VZ6(I)

       VXD8(I)=VX6(I)  
       VYD8(I)=VY6(I)  
       VZD8(I)=VZ6(I) 

       if (Write_flag == 1) then              
       write(*,*) 'VXD1(I) = ', VXD1(I)
       write(*,*) 'VXD2(I) = ', VXD2(I)
       write(*,*) 'VXD3(I) = ', VXD3(I)
       write(*,*) 'VXD4(I) = ', VXD4(I)
       write(*,*) 'VXD5(I) = ', VXD5(I)
       write(*,*) 'VXD6(I) = ', VXD6(I)
       write(*,*) 'VXD7(I) = ', VXD7(I)
       write(*,*) 'VXD8(I) = ', VXD8(I)
       write(*,*) 'VYD1(I) = ', VYD1(I)
       write(*,*) 'VYD2(I) = ', VYD2(I)
       write(*,*) 'VYD3(I) = ', VYD3(I)
       write(*,*) 'VYD4(I) = ', VYD4(I)
       write(*,*) 'VYD5(I) = ', VYD5(I)
       write(*,*) 'VYD6(I) = ', VYD6(I)
       write(*,*) 'VYD7(I) = ', VYD7(I)
       write(*,*) 'VYD8(I) = ', VYD8(I)   
       write(*,*) 'VZD1(I) = ', VZD1(I)
       write(*,*) 'VZD2(I) = ', VZD2(I)
       write(*,*) 'VZD3(I) = ', VZD3(I)
       write(*,*) 'VZD4(I) = ', VZD4(I)
       write(*,*) 'VZD5(I) = ', VZD5(I)
       write(*,*) 'VZD6(I) = ', VZD6(I)
       write(*,*) 'VZD7(I) = ', VZD7(I)
       write(*,*) 'VZD8(I) = ', VZD8(I)  
       endif     

      ENDDO
      IF(OFF_L<ZERO)THEN
        DO I=LFT,LLT
          IF(OFFG(I)<ZERO)THEN
            VX1(I)=ZERO
            VY1(I)=ZERO
            VZ1(I)=ZERO
            VX2(I)=ZERO
            VY2(I)=ZERO
            VZ2(I)=ZERO
            VX3(I)=ZERO
            VY3(I)=ZERO
            VZ3(I)=ZERO
            VX4(I)=ZERO
            VY4(I)=ZERO
            VZ4(I)=ZERO
            VX5(I)=ZERO
            VY5(I)=ZERO
            VZ5(I)=ZERO
            VX6(I)=ZERO
            VY6(I)=ZERO
            VZ6(I)=ZERO
          ENDIF
        ENDDO
      ENDIF
C-----------
C     Prepare les sorties par part.
C-----------
      IF(IOUTPRT/=0)THEN
       DO I=LFT,LLT
        VGAX(I)=VX1(I)+VX2(I)+VX3(I)+VX4(I)+VX5(I)+VX6(I)
        VGAY(I)=VY1(I)+VY2(I)+VY3(I)+VY4(I)+VY5(I)+VY6(I)
        VGAZ(I)=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)+VZ5(I)+VZ6(I)
        VGA2(I)=VX1(I)*VX1(I)+VX2(I)*VX2(I)+VX3(I)*VX3(I)+VX4(I)*VX4(I)
     1         +VX5(I)*VX5(I)+VX6(I)*VX6(I)
     2         +VY1(I)*VY1(I)+VY2(I)*VY2(I)+VY3(I)*VY3(I)+VY4(I)*VY4(I)
     3         +VY5(I)*VY5(I)+VY6(I)*VY6(I)
     4         +VZ1(I)*VZ1(I)+VZ2(I)*VZ2(I)+VZ3(I)*VZ3(I)+VZ4(I)*VZ4(I)
     5         +VZ5(I)*VZ5(I)+VZ6(I)*VZ6(I)
       ENDDO
       IF(IPARG(80)==1) THEN
         DO I=LFT,LLT
          XGAX(I)=X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)
          XGAY(I)=Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)
          XGAZ(I)=Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)
          XGXA2(I)=X1(I)**2+X2(I)**2+X3(I)**2+X4(I)**2
     1            +X5(I)**2+X6(I)**2
          XGYA2(I)=Y1(I)**2+Y2(I)**2+Y3(I)**2+Y4(I)**2
     1            +Y5(I)**2+Y6(I)**2
          XGZA2(I)=Z1(I)**2+Z2(I)**2+Z3(I)**2+Z4(I)**2
     1            +Z5(I)**2+Z6(I)**2
          XGXYA(I)=X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)+X4(I)*Y4(I)
     1            +X5(I)*Y5(I)+X6(I)*Y6(I)
          XGYZA(I)=Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)+Y4(I)*Z4(I)
     1            +Y5(I)*Z5(I)+Y6(I)*Z6(I)
          XGZXA(I)=Z1(I)*X1(I)+Z2(I)*X2(I)+Z3(I)*X3(I)+Z4(I)*X4(I)
     1            +Z5(I)*X5(I)+Z6(I)*X6(I)
         ENDDO
       ENDIF
      ENDIF

C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      CALL SREPISO3(
     1   XD1,     XD2,     XD3,     XD4,
     2   XD5,     XD6,     XD7,     XD8,
     3   YD1,     YD2,     YD3,     YD4,
     4   YD5,     YD6,     YD7,     YD8,
     5   ZD1,     ZD2,     ZD3,     ZD4,
     6   ZD5,     ZD6,     ZD7,     ZD8,
     7   RX,      RY,      RZ,      SX,
     8   SY,      SZ,      TX,      TY,
     9   TZ,      NEL)

! DO I=LFT,LLT
! write(*,*) 'RX(1)', RX(I)
! write(*,*) 'RY(I)', RY(I)  
! write(*,*) 'RZ(I)', RZ(I)
! write(*,*) 'SX(I)', SX(I)
! write(*,*) 'SY(I)', SY(I)
! write(*,*) 'SZ(I)', SZ(I)
! write(*,*) 'TX(I)', TX(I)
! write(*,*) 'TY(I)', TY(I)
! write(*,*) 'TZ(I)', TZ(I)
! ENDDO    
C---

        CALL SORTHO3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R12,     R13,     R11,
     4   R22,     R23,     R21,     R32,
     5   R33,     R31,     NEL)

C------stocker [R] in %GAMA_R--------------     
       GAMA_R(LFT:LLT,1) = R11(LFT:LLT) ! Dir1_x                            
       GAMA_R(LFT:LLT,2) = R21(LFT:LLT) ! Dir1_y                            
       GAMA_R(LFT:LLT,3) = R31(LFT:LLT) ! Dir1_z                            
       GAMA_R(LFT:LLT,4) = R12(LFT:LLT) ! Dir2_x                            
       GAMA_R(LFT:LLT,5) = R22(LFT:LLT) ! Dir2_y                           
       GAMA_R(LFT:LLT,6) = R32(LFT:LLT) ! Dir2_z     

!  DO I=LFT,LLT
!    write(*,*) 'GAMA_R(I,1)', GAMA_R(I,1)
!    write(*,*) 'GAMA_R(I,2)', GAMA_R(I,2)  
!    write(*,*) 'GAMA_R(I,3)', GAMA_R(I,3)
!       write(*,*) 'GAMA_R(I,4)', GAMA_R(I,4)
!       write(*,*) 'GAMA_R(I,5)', GAMA_R(I,5)
!       write(*,*) 'GAMA_R(I,6)', GAMA_R(I,6)
!  ENDDO
     
!      write(*,*) 'ISORTH = ', ISORTH
       IF (ISORTH == 0) THEN
!            write(*,*) 'In the SRCOOR3 subroutine, the ISORTH variable is 0.'
            DO I=LFT,LLT                                          
              GAMA(I,1) = ONE                             
              GAMA(I,2) = ZERO                              
              GAMA(I,3) = ZERO
              GAMA(I,4) = ZERO                              
              GAMA(I,5) = ONE                              
              GAMA(I,6) = ZERO
            ENDDO      
          ELSE    
            CALL SORTHDIR3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      R11,     R12,     R13,
     4   R21,     R22,     R23,     R31,
     5   R32,     R33,     GAMA0,   GAMA,
     6   NEL,     IREP)
      ENDIF



C  -----------
    !  DO I=LFT,LLT
    !    XL=ONE_OVER_6*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I))
    !    YL=ONE_OVER_6*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I))
    !    ZL=ONE_OVER_6*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I))
    !    X1(I)=X1(I)-XL
    !    Y1(I)=Y1(I)-YL
    !    Z1(I)=Z1(I)-ZL
    !    X2(I)=X2(I)-XL
    !    Y2(I)=Y2(I)-YL
    !    Z2(I)=Z2(I)-ZL
    !    X3(I)=X3(I)-XL
    !    Y3(I)=Y3(I)-YL
    !    Z3(I)=Z3(I)-ZL
    !    X4(I)=X4(I)-XL
    !    Y4(I)=Y4(I)-YL
    !    Z4(I)=Z4(I)-ZL
    !    X5(I)=X5(I)-XL
    !    Y5(I)=Y5(I)-YL
    !    Z5(I)=Z5(I)-ZL
    !    X6(I)=X6(I)-XL
    !    Y6(I)=Y6(I)-YL
    !    Z6(I)=Z6(I)-ZL
    !  ENDDO
C-----------
C     REPERE CONVECTE 
C-----------
   !    CALL S6CORTHO3(
   !  1   X1,      X2,      X3,      X4,
   !  2   X5,      X6,      Y1,      Y2,
   !  3   Y3,      Y4,      Y5,      Y6,
   !  4   Z1,      Z2,      Z3,      Z4,
   !  5   Z5,      Z6,      R11,     R12,
   !  6   R13,     R21,     R22,     R23,
   !  7   R31,     R32,     R33,     RX,
   !  8   RY,      RZ,      SX,      SY,
   !  9   SZ,      TX,      TY,      TZ,
   !  A   NEL)
C------stocker [R] in %GAMA_R--------------     
    !   GAMA_R(LFT:LLT,1) = R11(LFT:LLT)                             
    !   GAMA_R(LFT:LLT,2) = R21(LFT:LLT)                             
    !   GAMA_R(LFT:LLT,3) = R31(LFT:LLT)                             
    !   GAMA_R(LFT:LLT,4) = R12(LFT:LLT)                             
    !   GAMA_R(LFT:LLT,5) = R22(LFT:LLT)                             
    !   GAMA_R(LFT:LLT,6) = R32(LFT:LLT)                             
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
C   X=RX' <=> X'=t(R)X chgt de base.
      
      IF(ISMSTR<=4.AND.JLAG>0) THEN
       DO I=LFT,LLT
       IF(OFFG(I)>ONE)THEN
       !//TODO:not touched
        X1(I)=SAV(I,1)
        Y1(I)=SAV(I,2)
        Z1(I)=SAV(I,3)
        X2(I)=SAV(I,4)
        Y2(I)=SAV(I,5)
        Z2(I)=SAV(I,6)
        X3(I)=SAV(I,7)
        Y3(I)=SAV(I,8)
        Z3(I)=SAV(I,9)
        X4(I)=SAV(I,10)
        Y4(I)=SAV(I,11)
        Z4(I)=SAV(I,12)
        X5(I)=SAV(I,13)
        Y5(I)=SAV(I,14)
        Z5(I)=SAV(I,15)
        X6(I)=ZERO
        Y6(I)=ZERO
        Z6(I)=ZERO
        OFF(I) = OFFG(I) -ONE
        XL=ONE_OVER_6*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I))
        YL=ONE_OVER_6*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I))
        ZL=ONE_OVER_6*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I))
        X1(I)=X1(I)-XL
        Y1(I)=Y1(I)-YL
        Z1(I)=Z1(I)-ZL
        X2(I)=X2(I)-XL
        Y2(I)=Y2(I)-YL
        Z2(I)=Z2(I)-ZL
        X3(I)=X3(I)-XL
        Y3(I)=Y3(I)-YL
        Z3(I)=Z3(I)-ZL
        X4(I)=X4(I)-XL
        Y4(I)=Y4(I)-YL
        Z4(I)=Z4(I)-ZL
        X5(I)=X5(I)-XL
        Y5(I)=Y5(I)-YL
        Z5(I)=Z5(I)-ZL
        X6(I)=X6(I)-XL
        Y6(I)=Y6(I)-YL
        Z6(I)=Z6(I)-ZL
       ELSE

          !  write(*,*) 'In the S6RCOOR3_NEW subroutine, the ISMSTR variable is 4.'
          !  write(*,*) 'R11(I)', R11(I)
          !  write(*,*) 'R12(I)', R12(I)
          !  write(*,*) 'R13(I)', R13(I)
          !  write(*,*) 'R21(I)', R21(I)
          !  write(*,*) 'R22(I)', R22(I)
          !  write(*,*) 'R23(I)', R23(I)
          !  write(*,*) 'R31(I)', R31(I)
          !  write(*,*) 'R32(I)', R32(I)
          !  write(*,*) 'R33(I)', R33(I)
            XDL=R11(I)*XD1(I)+R21(I)*YD1(I)+R31(I)*ZD1(I)
            YDL=R12(I)*XD1(I)+R22(I)*YD1(I)+R32(I)*ZD1(I)
            ZDL=R13(I)*XD1(I)+R23(I)*YD1(I)+R33(I)*ZD1(I)
            XD1(I)=XDL
            YD1(I)=YDL
            ZD1(I)=ZDL
            XDL=R11(I)*XD2(I)+R21(I)*YD2(I)+R31(I)*ZD2(I)
            YDL=R12(I)*XD2(I)+R22(I)*YD2(I)+R32(I)*ZD2(I)
            ZDL=R13(I)*XD2(I)+R23(I)*YD2(I)+R33(I)*ZD2(I)
            XD2(I)=XDL
            YD2(I)=YDL
            ZD2(I)=ZDL
            XDL=R11(I)*XD3(I)+R21(I)*YD3(I)+R31(I)*ZD3(I)
            YDL=R12(I)*XD3(I)+R22(I)*YD3(I)+R32(I)*ZD3(I)
            ZDL=R13(I)*XD3(I)+R23(I)*YD3(I)+R33(I)*ZD3(I)
            XD3(I)=XDL
            YD3(I)=YDL
            ZD3(I)=ZDL
            XDL=R11(I)*XD4(I)+R21(I)*YD4(I)+R31(I)*ZD4(I)
            YDL=R12(I)*XD4(I)+R22(I)*YD4(I)+R32(I)*ZD4(I)
            ZDL=R13(I)*XD4(I)+R23(I)*YD4(I)+R33(I)*ZD4(I)
            XD4(I)=XDL
            YD4(I)=YDL
            ZD4(I)=ZDL
            XDL=R11(I)*XD5(I)+R21(I)*YD5(I)+R31(I)*ZD5(I)
            YDL=R12(I)*XD5(I)+R22(I)*YD5(I)+R32(I)*ZD5(I)
            ZDL=R13(I)*XD5(I)+R23(I)*YD5(I)+R33(I)*ZD5(I)
            XD5(I)=XDL
            YD5(I)=YDL
            ZD5(I)=ZDL
            XDL=R11(I)*XD6(I)+R21(I)*YD6(I)+R31(I)*ZD6(I)
            YDL=R12(I)*XD6(I)+R22(I)*YD6(I)+R32(I)*ZD6(I)
            ZDL=R13(I)*XD6(I)+R23(I)*YD6(I)+R33(I)*ZD6(I)
            XD6(I)=XDL
            YD6(I)=YDL
            ZD6(I)=ZDL
            XDL=R11(I)*XD7(I)+R21(I)*YD7(I)+R31(I)*ZD7(I)
            YDL=R12(I)*XD7(I)+R22(I)*YD7(I)+R32(I)*ZD7(I)
            ZDL=R13(I)*XD7(I)+R23(I)*YD7(I)+R33(I)*ZD7(I)
            XD7(I)=XDL
            YD7(I)=YDL
            ZD7(I)=ZDL
            XDL=R11(I)*XD8(I)+R21(I)*YD8(I)+R31(I)*ZD8(I)
            YDL=R12(I)*XD8(I)+R22(I)*YD8(I)+R32(I)*ZD8(I)
            ZDL=R13(I)*XD8(I)+R23(I)*YD8(I)+R33(I)*ZD8(I)
            XD8(I)=XDL
            YD8(I)=YDL
            ZD8(I)=ZDL              
            OFF(I) = OFFG(I)
            !//TODO: A VERIFY OFF_L  = MIN(OFF_L,OFFG(I))
       ENDIF
       ENDDO
C
      ELSE
!       write(*,*) 'In the S6RCOOR3_NEW subroutine, the ISMSTR variable is greater than 4.'
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     X1,      Y1,      Z1,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     X2,      Y2,      Z2,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     X3,      Y3,      Z3,
     4   NEL)
       DO I=LFT,LLT
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
C          ZL=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
        X4(I)=XL
        Y4(I)=YL
        Z4(I)=-Z1(I)
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
C          ZL=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
        X5(I)=XL
        Y5(I)=YL
        Z5(I)=-Z2(I)
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
C          ZL=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
        X6(I)=XL
        Y6(I)=YL
        Z6(I)=-Z3(I)
        OFF(I) = MIN(ONE,OFFG(I))
       ENDDO
C
       ENDIF


  
C-----------
C     PASSAGE DES VITESSES AU REPERE CONVECTE (OU ORTHOTROPE).
C-----------
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     VX1,     VY1,     VZ1,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     VX2,     VY2,     VZ2,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     VX3,     VY3,     VZ3,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     VX4,     VY4,     VZ4,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     VX5,     VY5,     VZ5,
     4   NEL)
      CALL VRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,     VX6,     VY6,     VZ6,
     4   NEL)
C-----------
      DO I=LFT,LLT
        VD2(I) = ZERO
      ENDDO
C-----PROJECTION----
      DO I=LFT,LLT
        XX = X1(I)*X1(I)+X2(I)*X2(I)+X3(I)*X3(I)
     1      +X4(I)*X4(I)+X5(I)*X5(I)+X6(I)*X6(I)
        YY = Y1(I)*Y1(I)+Y2(I)*Y2(I)+Y3(I)*Y3(I)
     1      +Y4(I)*Y4(I)+Y5(I)*Y5(I)+Y6(I)*Y6(I)
        XY = X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)
     1      +X4(I)*Y4(I)+X5(I)*Y5(I)+X6(I)*Y6(I)
        XZ = X1(I)*Z1(I)+X2(I)*Z2(I)+X3(I)*Z3(I)
     1      +X4(I)*Z4(I)+X5(I)*Z5(I)+X6(I)*Z6(I)
        YZ = Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)
     1      +Y4(I)*Z4(I)+Y5(I)*Z5(I)+Y6(I)*Z6(I)
        ZZ = Z1(I)*Z1(I)+Z2(I)*Z2(I)+Z3(I)*Z3(I)
     1      +Z4(I)*Z4(I)+Z5(I)*Z5(I)+Z6(I)*Z6(I)
        RTR(1)= YY+ZZ
        RTR(2)= XX+ZZ
        RTR(3)= XX+YY
        RTR(4)= -XY
        RTR(5)= -XZ
        RTR(6)= -YZ
C       
        ABC = RTR(1)*RTR(2)*RTR(3)
        XXYZ2 = RTR(1)*RTR(6)*RTR(6)
        YYXZ2 = RTR(2)*RTR(5)*RTR(5)
        ZZXY2 = RTR(3)*RTR(4)*RTR(4)
        DETA = ABC + TWO*RTR(4)*RTR(5)*RTR(6)-XXYZ2-YYXZ2-ZZXY2
        IF (DETA<EM20) THEN
          DETA=ONE
        ELSE
         DETA=ONE/DETA
        ENDIF
        DI(I,1) = (ABC-XXYZ2)*DETA/RTR(1)
        DI(I,2) = (ABC-YYXZ2)*DETA/RTR(2)
        DI(I,3) = (ABC-ZZXY2)*DETA/RTR(3)
        DI(I,4) = (RTR(5)*RTR(6)-RTR(4)*RTR(3))*DETA
        DI(I,5) = (RTR(6)*RTR(4)-RTR(5)*RTR(2))*DETA
        DI(I,6) = (RTR(4)*RTR(5)-RTR(6)*RTR(1))*DETA
      ENDDO
      
      CALL S6PROJ3(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      Y1,      Y2,
     3   Y3,      Y4,      Y5,      Y6,
     4   Z1,      Z2,      Z3,      Z4,
     5   Z5,      Z6,      VX1,     VX2,
     6   VX3,     VX4,     VX5,     VX6,
     7   VY1,     VY2,     VY3,     VY4,
     8   VY5,     VY6,     VZ1,     VZ2,
     9   VZ3,     VZ4,     VZ5,     VZ6,
     A   DI,      NEL)


           CALL SRROTA3(
     1   R11,     R12,     R13,     R21,
     2   R22,     R23,     R31,     R32,
     3   R33,      VXD1,     VXD2,     VXD3,
     4   VXD4,     VXD5,     VXD6,     VXD7,
     5   VXD8,     VYD1,     VYD2,     VYD3,
     6   VYD4,     VYD5,     VYD6,     VYD7,
     7   VYD8,     VZD1,     VZD2,     VZD3,
     8   VZD4,     VZD5,     VZD6,     VZD7,
     9   VZD8,     NEL)


      DO I=LFT,LLT
        X1(I)= XD1(I) 
        Y1(I)= YD1(I) 
        Z1(I)= ZD1(I)

        X2(I)= XD2(I)  
        Y2(I)= YD2(I)  
        Z2(I)= ZD2(I) 

        X3(I)= XD3(I) 
        Y3(I)= YD3(I) 
        Z3(I)= ZD3(I) 

        X4(I)= XD5(I) 
        Y4(I)= YD5(I) 
        Z4(I)= ZD5(I) 

        X5(I)= XD6(I) 
        Y5(I)= YD6(I)
        Z5(I)= ZD6(I) 

        X6(I)= XD7(I) 
        Y6(I)= YD7(I) 
        Z6(I)= ZD7(I) 
 
      !  write(*,*) 'I = ', I
      !  write(*,*) 'X1(I) = ', X1(I)
      !  write(*,*) 'X2(I) = ', X2(I)
      !  write(*,*) 'X3(I) = ', X3(I)
      !  write(*,*) 'X4(I) = ', X4(I)
      !  write(*,*) 'X5(I) = ', X5(I)
      !  write(*,*) 'X6(I) = ', X6(I)

      !  write(*,*) 'Y1(I) = ', Y1(I)
      !  write(*,*) 'Y2(I) = ', Y2(I)
      !  write(*,*) 'Y3(I) = ', Y3(I)
      !  write(*,*) 'Y4(I) = ', Y4(I)
      !  write(*,*) 'Y5(I) = ', Y5(I)
      !  write(*,*) 'Y6(I) = ', Y6(I)

      !  write(*,*) 'Z1(I) = ', Z1(I)
      !  write(*,*) 'Z2(I) = ', Z2(I)
      !  write(*,*) 'Z3(I) = ', Z3(I)
      !  write(*,*) 'Z4(I) = ', Z4(I)
      !  write(*,*) 'Z5(I) = ', Z5(I)
      !  write(*,*) 'Z6(I) = ', Z6(I)


        VX1(I)= VXD1(I) 
        VY1(I)= VYD1(I) 
        VZ1(I)= VZD1(I)

        VX2(I)= VXD2(I)  
        VY2(I)= VYD2(I)  
        VZ2(I)= VZD2(I) 

        VX3(I)= VXD3(I) 
        VY3(I)= VYD3(I) 
        VZ3(I)= VZD3(I) 

        VX4(I)= VXD5(I) 
        VY4(I)= VYD5(I) 
        VZ4(I)= VZD5(I) 

        VX5(I)= VXD6(I) 
        VY5(I)= VYD6(I)
        VZ5(I)= VZD6(I) 

        VX6(I)= VXD7(I) 
        VY6(I)= VYD7(I) 
        VZ6(I)= VZD7(I) 

      ENDDO  
!   CALL SRROTA3(
!  1   R11,     R12,     R13,     R21,
!  2   R22,     R23,     R31,     R32,
!  3   R33,     VX1,     VX2,     VX3,
!  4   VX1,     VX4,     VX5,     VX6,
!  5   VX6,     VY1,     VY2,     VY3,
!  6   VY1,     VY4,     VY5,     VY6,
!  7   VY6,     VZ1,     VZ2,     VZ3,
!  8   VZ1,     VZ4,     VZ5,     VZ6,
!  9   VZ6,     NEL)
 
!  DO I=LFT,LLT
!        X1(I) =   -6.0150095500754581     
!        X2(I) =   -1.1487646027368061     
!        X3(I) =   -0.0000000000000000     
!      
!        X4(I) =   -6.0150095500754581     
!        X5(I) =   -1.1487646027368061     
!       X6(I) =   -0.0000000000000000     
!     

! Y1(I) =   -3.7174803446018458     
! Y2(I) =   -4.8662449473386520     
! Y3(I) =    0.0000000000000000        

!           Y4(I) =   -3.7174803446018458     
!         Y5(I) =   -4.8662449473386520     
!        Y6(I) =    0.0000000000000000     

!       Z1(I) =   -5.0000000000000009     
!       Z2(I) =   -5.0000000000000009     
!       Z3(I) =   -5.0000000000000009     
             
!      Z4(I) =    0.0000000000000000     
!       Z5(I) =    0.0000000000000000     
!     Z6(I) =    0.0000000000000000     
     

!enddo


      RETURN
      END
