/* Copyright (C) 1991-2016 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.
   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 8.0.0.  Version 8.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2014, plus Amendment 1 (published
   2015-05-15).  */
/* We do not support C11 <threads.h>.  */
        subroutine GETV(
     &           V
     &           ,phi
     &           ,phi0
     &           ,alp
     &           ,s
     &           ,eps
     &           )
      implicit none
      REAL*8 V
      REAL*8 phi
      REAL*8 phi0
      REAL*8 alp
      REAL*8 s
      REAL*8 eps
        V = 0.5*s*s*phi*phi
        return
        end
        subroutine GETVPHI(
     &           Vphi
     &           ,phi
     &           ,phi0
     &           ,alp
     &           ,s
     &           ,eps
     &           )
      implicit none
      REAL*8 Vphi
      REAL*8 phi
      REAL*8 phi0
      REAL*8 alp
      REAL*8 s
      REAL*8 eps
       Vphi = s*s*phi
        return
        end
      subroutine GETBSSNCRHSF(
     &           dchidt
     &           ,idchidtlo0,idchidtlo1,idchidtlo2
     &           ,idchidthi0,idchidthi1,idchidthi2
     &           ,dgammadt
     &           ,idgammadtlo0,idgammadtlo1,idgammadtlo2
     &           ,idgammadthi0,idgammadthi1,idgammadthi2
     &           ,ndgammadtcomp
     &           ,dKdt
     &           ,idKdtlo0,idKdtlo1,idKdtlo2
     &           ,idKdthi0,idKdthi1,idKdthi2
     &           ,dAdt
     &           ,idAdtlo0,idAdtlo1,idAdtlo2
     &           ,idAdthi0,idAdthi1,idAdthi2
     &           ,ndAdtcomp
     &           ,dThetadt
     &           ,idThetadtlo0,idThetadtlo1,idThetadtlo2
     &           ,idThetadthi0,idThetadthi1,idThetadthi2
     &           ,dGamma1dt
     &           ,idGamma1dtlo0,idGamma1dtlo1,idGamma1dtlo2
     &           ,idGamma1dthi0,idGamma1dthi1,idGamma1dthi2
     &           ,ndGamma1dtcomp
     &           ,dalphadt
     &           ,idalphadtlo0,idalphadtlo1,idalphadtlo2
     &           ,idalphadthi0,idalphadthi1,idalphadthi2
     &           ,dbetadt
     &           ,idbetadtlo0,idbetadtlo1,idbetadtlo2
     &           ,idbetadthi0,idbetadthi1,idbetadthi2
     &           ,ndbetadtcomp
     &           ,dBdt
     &           ,idBdtlo0,idBdtlo1,idBdtlo2
     &           ,idBdthi0,idBdthi1,idBdthi2
     &           ,ndBdtcomp
     &           ,dphidt
     &           ,idphidtlo0,idphidtlo1,idphidtlo2
     &           ,idphidthi0,idphidthi1,idphidthi2
     &           ,dphiMdt
     &           ,idphiMdtlo0,idphiMdtlo1,idphiMdtlo2
     &           ,idphiMdthi0,idphiMdthi1,idphiMdthi2
     &           ,chi
     &           ,ichilo0,ichilo1,ichilo2
     &           ,ichihi0,ichihi1,ichihi2
     &           ,gamma
     &           ,igammalo0,igammalo1,igammalo2
     &           ,igammahi0,igammahi1,igammahi2
     &           ,ngammacomp
     &           ,K
     &           ,iKlo0,iKlo1,iKlo2
     &           ,iKhi0,iKhi1,iKhi2
     &           ,A
     &           ,iAlo0,iAlo1,iAlo2
     &           ,iAhi0,iAhi1,iAhi2
     &           ,nAcomp
     &           ,Theta
     &           ,iThetalo0,iThetalo1,iThetalo2
     &           ,iThetahi0,iThetahi1,iThetahi2
     &           ,Gamma1
     &           ,iGamma1lo0,iGamma1lo1,iGamma1lo2
     &           ,iGamma1hi0,iGamma1hi1,iGamma1hi2
     &           ,nGamma1comp
     &           ,alpha
     &           ,ialphalo0,ialphalo1,ialphalo2
     &           ,ialphahi0,ialphahi1,ialphahi2
     &           ,beta
     &           ,ibetalo0,ibetalo1,ibetalo2
     &           ,ibetahi0,ibetahi1,ibetahi2
     &           ,nbetacomp
     &           ,B
     &           ,iBlo0,iBlo1,iBlo2
     &           ,iBhi0,iBhi1,iBhi2
     &           ,nBcomp
     &           ,phi
     &           ,iphilo0,iphilo1,iphilo2
     &           ,iphihi0,iphihi1,iphihi2
     &           ,phiM
     &           ,iphiMlo0,iphiMlo1,iphiMlo2
     &           ,iphiMhi0,iphiMhi1,iphiMhi2
     &           ,dx
     &           ,kappa1
     &           ,kappa2
     &           ,kappa3
     &           ,eta
     &           ,mubeta1
     &           ,sigma
     &           ,s
     &           ,iboxlo0,iboxlo1,iboxlo2
     &           ,iboxhi0,iboxhi1,iboxhi2
     &           )
      implicit none
      integer CHF_ID(0:5,0:5)
      data CHF_ID/ 1,0,0,0,0,0 ,0,1,0,0,0,0 ,0,0,1,0,0,0 ,0,0,0,1,0,0 ,0
     &,0,0,0,1,0 ,0,0,0,0,0,1 /
      integer idchidtlo0,idchidtlo1,idchidtlo2
      integer idchidthi0,idchidthi1,idchidthi2
      REAL*8 dchidt(
     &           idchidtlo0:idchidthi0,
     &           idchidtlo1:idchidthi1,
     &           idchidtlo2:idchidthi2)
      integer ndgammadtcomp
      integer idgammadtlo0,idgammadtlo1,idgammadtlo2
      integer idgammadthi0,idgammadthi1,idgammadthi2
      REAL*8 dgammadt(
     &           idgammadtlo0:idgammadthi0,
     &           idgammadtlo1:idgammadthi1,
     &           idgammadtlo2:idgammadthi2,
     &           0:ndgammadtcomp-1)
      integer idKdtlo0,idKdtlo1,idKdtlo2
      integer idKdthi0,idKdthi1,idKdthi2
      REAL*8 dKdt(
     &           idKdtlo0:idKdthi0,
     &           idKdtlo1:idKdthi1,
     &           idKdtlo2:idKdthi2)
      integer ndAdtcomp
      integer idAdtlo0,idAdtlo1,idAdtlo2
      integer idAdthi0,idAdthi1,idAdthi2
      REAL*8 dAdt(
     &           idAdtlo0:idAdthi0,
     &           idAdtlo1:idAdthi1,
     &           idAdtlo2:idAdthi2,
     &           0:ndAdtcomp-1)
      integer idThetadtlo0,idThetadtlo1,idThetadtlo2
      integer idThetadthi0,idThetadthi1,idThetadthi2
      REAL*8 dThetadt(
     &           idThetadtlo0:idThetadthi0,
     &           idThetadtlo1:idThetadthi1,
     &           idThetadtlo2:idThetadthi2)
      integer ndGamma1dtcomp
      integer idGamma1dtlo0,idGamma1dtlo1,idGamma1dtlo2
      integer idGamma1dthi0,idGamma1dthi1,idGamma1dthi2
      REAL*8 dGamma1dt(
     &           idGamma1dtlo0:idGamma1dthi0,
     &           idGamma1dtlo1:idGamma1dthi1,
     &           idGamma1dtlo2:idGamma1dthi2,
     &           0:ndGamma1dtcomp-1)
      integer idalphadtlo0,idalphadtlo1,idalphadtlo2
      integer idalphadthi0,idalphadthi1,idalphadthi2
      REAL*8 dalphadt(
     &           idalphadtlo0:idalphadthi0,
     &           idalphadtlo1:idalphadthi1,
     &           idalphadtlo2:idalphadthi2)
      integer ndbetadtcomp
      integer idbetadtlo0,idbetadtlo1,idbetadtlo2
      integer idbetadthi0,idbetadthi1,idbetadthi2
      REAL*8 dbetadt(
     &           idbetadtlo0:idbetadthi0,
     &           idbetadtlo1:idbetadthi1,
     &           idbetadtlo2:idbetadthi2,
     &           0:ndbetadtcomp-1)
      integer ndBdtcomp
      integer idBdtlo0,idBdtlo1,idBdtlo2
      integer idBdthi0,idBdthi1,idBdthi2
      REAL*8 dBdt(
     &           idBdtlo0:idBdthi0,
     &           idBdtlo1:idBdthi1,
     &           idBdtlo2:idBdthi2,
     &           0:ndBdtcomp-1)
      integer idphidtlo0,idphidtlo1,idphidtlo2
      integer idphidthi0,idphidthi1,idphidthi2
      REAL*8 dphidt(
     &           idphidtlo0:idphidthi0,
     &           idphidtlo1:idphidthi1,
     &           idphidtlo2:idphidthi2)
      integer idphiMdtlo0,idphiMdtlo1,idphiMdtlo2
      integer idphiMdthi0,idphiMdthi1,idphiMdthi2
      REAL*8 dphiMdt(
     &           idphiMdtlo0:idphiMdthi0,
     &           idphiMdtlo1:idphiMdthi1,
     &           idphiMdtlo2:idphiMdthi2)
      integer ichilo0,ichilo1,ichilo2
      integer ichihi0,ichihi1,ichihi2
      REAL*8 chi(
     &           ichilo0:ichihi0,
     &           ichilo1:ichihi1,
     &           ichilo2:ichihi2)
      integer ngammacomp
      integer igammalo0,igammalo1,igammalo2
      integer igammahi0,igammahi1,igammahi2
      REAL*8 gamma(
     &           igammalo0:igammahi0,
     &           igammalo1:igammahi1,
     &           igammalo2:igammahi2,
     &           0:ngammacomp-1)
      integer iKlo0,iKlo1,iKlo2
      integer iKhi0,iKhi1,iKhi2
      REAL*8 K(
     &           iKlo0:iKhi0,
     &           iKlo1:iKhi1,
     &           iKlo2:iKhi2)
      integer nAcomp
      integer iAlo0,iAlo1,iAlo2
      integer iAhi0,iAhi1,iAhi2
      REAL*8 A(
     &           iAlo0:iAhi0,
     &           iAlo1:iAhi1,
     &           iAlo2:iAhi2,
     &           0:nAcomp-1)
      integer iThetalo0,iThetalo1,iThetalo2
      integer iThetahi0,iThetahi1,iThetahi2
      REAL*8 Theta(
     &           iThetalo0:iThetahi0,
     &           iThetalo1:iThetahi1,
     &           iThetalo2:iThetahi2)
      integer nGamma1comp
      integer iGamma1lo0,iGamma1lo1,iGamma1lo2
      integer iGamma1hi0,iGamma1hi1,iGamma1hi2
      REAL*8 Gamma1(
     &           iGamma1lo0:iGamma1hi0,
     &           iGamma1lo1:iGamma1hi1,
     &           iGamma1lo2:iGamma1hi2,
     &           0:nGamma1comp-1)
      integer ialphalo0,ialphalo1,ialphalo2
      integer ialphahi0,ialphahi1,ialphahi2
      REAL*8 alpha(
     &           ialphalo0:ialphahi0,
     &           ialphalo1:ialphahi1,
     &           ialphalo2:ialphahi2)
      integer nbetacomp
      integer ibetalo0,ibetalo1,ibetalo2
      integer ibetahi0,ibetahi1,ibetahi2
      REAL*8 beta(
     &           ibetalo0:ibetahi0,
     &           ibetalo1:ibetahi1,
     &           ibetalo2:ibetahi2,
     &           0:nbetacomp-1)
      integer nBcomp
      integer iBlo0,iBlo1,iBlo2
      integer iBhi0,iBhi1,iBhi2
      REAL*8 B(
     &           iBlo0:iBhi0,
     &           iBlo1:iBhi1,
     &           iBlo2:iBhi2,
     &           0:nBcomp-1)
      integer iphilo0,iphilo1,iphilo2
      integer iphihi0,iphihi1,iphihi2
      REAL*8 phi(
     &           iphilo0:iphihi0,
     &           iphilo1:iphihi1,
     &           iphilo2:iphihi2)
      integer iphiMlo0,iphiMlo1,iphiMlo2
      integer iphiMhi0,iphiMhi1,iphiMhi2
      REAL*8 phiM(
     &           iphiMlo0:iphiMhi0,
     &           iphiMlo1:iphiMhi1,
     &           iphiMlo2:iphiMhi2)
      REAL*8 dx
      REAL*8 kappa1
      REAL*8 kappa2
      REAL*8 kappa3
      REAL*8 eta
      REAL*8 mubeta1
      REAL*8 sigma
      REAL*8 s
      integer iboxlo0,iboxlo1,iboxlo2
      integer iboxhi0,iboxhi1,iboxhi2
      integer i0,i1,i2, ii0,ii1,ii2, jj0,jj1,jj2
      REAL*8 dxinv, dxinv2
      REAL*8 g(0:2,0:2), gu(0:2,0:2), detg
      REAL*8 dgammadx(0:2,0:2,0:3-1)
      REAL*8 d2gammadxdy(0:2,0:2,0:3-1,0:3-1)
      REAL*8 chris(0:2,0:2,0:2), chriss(0:2,0:2,0:2), chris1st(0:2,0:2,0
     &:2)
      REAL*8 dchidx(0:3-1)
      REAL*8 d2chidxdy(0:3-1,0:3-1)
      REAL*8 dbetadx(0:2,0:3-1), trdbetadx
      REAL*8 d2betadxdy(0:2,0:3-1,0:3-1)
      REAL*8 dalphadx(0:3-1)
      REAL*8 dBdx(0:3-1,0:3-1)
      REAL*8 d2alphadxdy(0:3-1,0:3-1)
      REAL*8 Gamma1d(0:2), R(0:2,0:2), Rchi(0:2,0:2), Rscalar
      REAL*8 dGamma1dx(0:2,0:3-1)
      REAL*8 dAdx(0:2,0:2,0:3-1)
      REAL*8 dKdx(0:3-1)
      REAL*8 dphidx(0:3-1), dphiMdx(0:3-1)
      REAL*8 d2phidxdy(0:3-1,0:3-1)
      REAL*8 Sten(0:2,0:2), trSten, Svec(0:2), rho, Vphi, Vphiphi
      REAL*8 dphidt2, beta2, Vt, betaidiphi, chi2, chi1, lapse
      REAL*8 n(0:3), nu(0:3), T(0:3,0:3)
      REAL*8 pstn(0:3-1), pstnr, f_0, dfdx
      REAL*8 phi0, alp, eps, centerx, centery, centerz
      REAL*8 DEBUG
      integer d0, d1, d2, d3, d4, bound(0:3-1)
      integer IDX(0:2, 0:2), boundaries
      data IDX / 0, 1, 2, 1, 3, 4, 2, 4, 5  /
      boundaries = 0
      eps = 0; phi0 = 0; alp = 0;
      centerx = 0; centery = 0; centerz = 0;
      dxinv  = (1.0d0)/dx
      dxinv2 = (1.0d0)/(dx*dx)
      DEBUG = 0.0
      do i2 = iboxlo2,iboxhi2
      do i1 = iboxlo1,iboxhi1
      do i0 = iboxlo0,iboxhi0
         lapse = alpha(i0,i1,i2)
         chi1 = chi(i0,i1,i2)         
         chi2 = chi1**2
         do d0 = 0,2; do d1 = 0,2
           g(d0,d1) = gamma(i0,i1,i2, IDX(d0,d1))
         enddo; enddo
         detg = g(0,0)*(g(1,1)*g(2,2)-g(1,2)*g(2,1))-
     &          g(0,1)*(g(2,2)*g(1,0)-g(1,2)*g(2,0))+
     &          g(0,2)*(g(1,0)*g(2,1)-g(1,1)*g(2,0))
         gu(0,0) = (g(1,1)*g(2,2)-g(1,2)*g(2,1))/detg
         gu(0,1) = (g(2,0)*g(1,2)-g(1,0)*g(2,2))/detg
         gu(0,2) = (g(1,0)*g(2,1)-g(2,0)*g(1,1))/detg
         gu(1,0) = (g(2,1)*g(0,2)-g(0,1)*g(2,2))/detg
         gu(1,1) = (g(0,0)*g(2,2)-g(0,2)*g(2,0))/detg
         gu(1,2) = (g(2,0)*g(0,1)-g(0,0)*g(2,1))/detg
         gu(2,0) = (g(0,1)*g(1,2)-g(1,1)*g(0,2))/detg
         gu(2,1) = (g(0,2)*g(1,0)-g(0,0)*g(1,2))/detg
         gu(2,2) = (g(0,0)*g(1,1)-g(0,1)*g(1,0))/detg
         do d0 = 0,2; do d1 = 0,3-1
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
           dbetadx(d0,d1) = dxinv/12 * (
     &         beta(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &     - 8*beta(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &     + 8*beta(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &     -   beta(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &       )
         enddo; enddo
         trdbetadx = 0
         do d0 = 0,3-1
           trdbetadx = trdbetadx + dbetadx(d0,d0)
         enddo
         do d0 = 0,2; do d1 = 0,3-1; do d2 = 0,3-1
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
             jj0 = CHF_ID(d2,0)
             jj1 = CHF_ID(d2,1)
             jj2 = CHF_ID(d2,2)
           if (d1 .eq. d2) cycle
           d2betadxdy(d0,d1,d2) = dxinv2/144 * (
     &         beta(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2,d0)
     &     - 8*beta(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      ,d0)
     &     + 8*beta(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      ,d0)
     &     -   beta(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2,d0)
     &     - 8*beta(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      ,d0)
     &     +64*beta(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            ,d0)
     &     -64*beta(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            ,d0)
     &     + 8*beta(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      ,d0)
     &     + 8*beta(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      ,d0)
     &     -64*beta(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            ,d0)
     &     +64*beta(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            ,d0)
     &     - 8*beta(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      ,d0)
     &     -   beta(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2,d0)
     &     + 8*beta(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      ,d0)
     &     - 8*beta(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      ,d0)
     &     +   beta(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2,d0)
     &       )
         enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,3-1
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
           d2betadxdy(d0,d1,d1) = dxinv2/12 * (
     &         -beta(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &     + 16*beta(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &     - 30*beta(i0,i1,i2        ,d0)
     &     + 16*beta(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &     -    beta(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &       )
         enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,3-1
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           dgammadx(d0,d1,d2) = dxinv/12 * (
     &         gamma(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &     - 8*gamma(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &     + 8*gamma(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &     -   gamma(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &       )
         enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,3-1; do d3 = 0,3-1
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
             jj0 = CHF_ID(d3,0)
             jj1 = CHF_ID(d3,1)
             jj2 = CHF_ID(d3,2)
           if (d2 .eq. d3) cycle
           d2gammadxdy(d0,d1,d2,d3) = dxinv2/144 * (
     &         gamma(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2,IDX(d0
     &,d1))
     &     - 8*gamma(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      ,IDX(d0
     &,d1))
     &     + 8*gamma(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      ,IDX(d0
     &,d1))
     &     -   gamma(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2,IDX(d0
     &,d1))
     &     - 8*gamma(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      ,IDX(d0
     &,d1))
     &     +64*gamma(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            ,IDX(d0
     &,d1))
     &     -64*gamma(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            ,IDX(d0
     &,d1))
     &     + 8*gamma(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      ,IDX(d0
     &,d1))
     &     + 8*gamma(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      ,IDX(d0
     &,d1))
     &     -64*gamma(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            ,IDX(d0
     &,d1))
     &     +64*gamma(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            ,IDX(d0
     &,d1))
     &     - 8*gamma(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      ,IDX(d0
     &,d1))
     &     -   gamma(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2,IDX(d0
     &,d1))
     &     + 8*gamma(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      ,IDX(d0
     &,d1))
     &     - 8*gamma(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      ,IDX(d0
     &,d1))
     &     +   gamma(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2,IDX(d0
     &,d1))
     &       )
         enddo; enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,3-1
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           d2gammadxdy(d0,d1,d2,d2) = dxinv2/12 * (
     &         -gamma(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &     + 16*gamma(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &     - 30*gamma(i0,i1,i2        ,IDX(d0,d1))
     &     + 16*gamma(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &     -    gamma(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &       )
         enddo; enddo; enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           dalphadx(d0) = dxinv/12 * (
     &         alpha(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*alpha(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*alpha(i0+ii0,i1+ii1,i2+ii2  )
     &     -   alpha(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         do d0 = 0,3-1; do d1 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
             jj0 = CHF_ID(d1,0)
             jj1 = CHF_ID(d1,1)
             jj2 = CHF_ID(d1,2)
           if (d0 .eq. d1) cycle
           d2alphadxdy(d0,d1) = dxinv2/144 * (
     &         alpha(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2)
     &     - 8*alpha(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      )
     &     + 8*alpha(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      )
     &     -   alpha(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2)
     &     - 8*alpha(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      )
     &     +64*alpha(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            )
     &     -64*alpha(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            )
     &     + 8*alpha(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      )
     &     + 8*alpha(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      )
     &     -64*alpha(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            )
     &     +64*alpha(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            )
     &     - 8*alpha(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      )
     &     -   alpha(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2)
     &     + 8*alpha(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      )
     &     - 8*alpha(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      )
     &     +   alpha(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2)
     &       )
         enddo; enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           d2alphadxdy(d0,d0) = dxinv2/12 * (
     &         -alpha(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     + 16*alpha(i0-ii0,i1-ii1,i2-ii2  )
     &     - 30*alpha(i0,i1,i2        )
     &     + 16*alpha(i0+ii0,i1+ii1,i2+ii2  )
     &     -    alpha(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         do d0 = 0,2; do d1 = 0,3-1
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
           dGamma1dx(d0,d1) = dxinv/12 * (
     &         Gamma1(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &     - 8*Gamma1(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &     + 8*Gamma1(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &     -   Gamma1(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &       )
         enddo; enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           dchidx(d0) = dxinv/12 * (
     &         chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)**2.0*0.5/chi1
     &     - 8*chi(i0-ii0,i1-ii1,i2-ii2  )**2.0*0.5/chi1
     &     + 8*chi(i0+ii0,i1+ii1,i2+ii2  )**2.0*0.5/chi1
     &     -   chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)**2.0*0.5/chi1
     &       )
         enddo
         do d0 = 0,3-1; do d1 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
             jj0 = CHF_ID(d1,0)
             jj1 = CHF_ID(d1,1)
             jj2 = CHF_ID(d1,2)
           if (d0 .eq. d1) cycle
           d2chidxdy(d0,d1) = dxinv2/144 * (
     &         chi(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2)**2.0
     &     - 8*chi(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      )**2.0
     &     + 8*chi(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      )**2.0
     &     -   chi(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2)**2.0
     &     - 8*chi(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      )**2.0
     &     +64*chi(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            )**2.0
     &     -64*chi(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            )**2.0
     &     + 8*chi(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      )**2.0
     &     + 8*chi(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      )**2.0
     &     -64*chi(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            )**2.0
     &     +64*chi(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            )**2.0
     &     - 8*chi(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      )**2.0
     &     -   chi(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2)**2.0
     &     + 8*chi(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      )**2.0
     &     - 8*chi(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      )**2.0
     &     +   chi(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2)**2.0
     &       )
         d2chidxdy(d0,d1) = 0.5*d2chidxdy(d0,d1)/chi1 - dchidx(d0)*dchid
     &x(d1)/chi1
         enddo; enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           d2chidxdy(d0,d0) = dxinv2/12 * (
     &         -chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)**2.0
     &     + 16*chi(i0-ii0,i1-ii1,i2-ii2  )**2.0
     &     - 30*chi(i0,i1,i2        )**2.0
     &     + 16*chi(i0+ii0,i1+ii1,i2+ii2  )**2.0
     &     -    chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)**2.0
     &       )
          d2chidxdy(d0,d0) = 0.5*d2chidxdy(d0,d0)/chi1 - dchidx(d0)*dchi
     &dx(d0)/chi1
         enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,3-1
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           dAdx(d0,d1,d2) = dxinv/12 * (
     &         A(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &     - 8*A(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &     + 8*A(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &     -   A(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &       )
         enddo; enddo; enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           dKdx(d0) = dxinv/12 * (
     &         K(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*K(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*K(i0+ii0,i1+ii1,i2+ii2  )
     &     -   K(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           dphidx(d0) = dxinv/12 * (
     &         phi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*phi(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*phi(i0+ii0,i1+ii1,i2+ii2  )
     &     -   phi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         do d0 = 0,3-1; do d1 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
             jj0 = CHF_ID(d1,0)
             jj1 = CHF_ID(d1,1)
             jj2 = CHF_ID(d1,2)
           if (d0 .eq. d1) cycle
           d2phidxdy(d0,d1) = dxinv2/144 * (
     &         phi(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2)
     &     - 8*phi(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      )
     &     + 8*phi(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      )
     &     -   phi(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2)
     &     - 8*phi(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      )
     &     +64*phi(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            )
     &     -64*phi(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            )
     &     + 8*phi(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      )
     &     + 8*phi(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      )
     &     -64*phi(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            )
     &     +64*phi(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            )
     &     - 8*phi(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      )
     &     -   phi(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2)
     &     + 8*phi(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      )
     &     - 8*phi(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      )
     &     +   phi(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2)
     &       )
         enddo; enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           d2phidxdy(d0,d0) = dxinv2/12 * (
     &         -phi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     + 16*phi(i0-ii0,i1-ii1,i2-ii2  )
     &     - 30*phi(i0,i1,i2        )
     &     + 16*phi(i0+ii0,i1+ii1,i2+ii2  )
     &     -    phi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           dphiMdx(d0) = dxinv/12 * (
     &         phiM(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*phiM(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*phiM(i0+ii0,i1+ii1,i2+ii2  )
     &     -   phiM(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         do d0 = 0,2; do d1 = 0,3-1
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
           dBdx(d0,d1) = dxinv/12 * (
     &         B(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &     - 8*B(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &     + 8*B(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &     -   B(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &       )
         enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,2
           chris(d0,d1,d2) = 0; chriss(d0,d1,d2) = 0
           do d3 = 0,2
             if (d2 .lt. 3) then
               chris(d0,d1,d2) = chris(d0,d1,d2) +
     &           (0.500d0)*gu(d0,d3)*dgammadx(d3,d1,d2)
               chriss(d0,d1,d2) = chriss(d0,d1,d2) +
     &           gu(d0,d3)*((0.500d0)*dgammadx(d3,d1,d2)*chi1
     &              - g(d3,d1)*dchidx(d2))/chi1
             endif
             if (d1 .lt. 3) then
               chris(d0,d1,d2) = chris(d0,d1,d2) +
     &           (0.500d0)*gu(d0,d3)*dgammadx(d3,d2,d1)
               chriss(d0,d1,d2) = chriss(d0,d1,d2) +
     &           gu(d0,d3)*((0.500d0)*dgammadx(d3,d2,d1)*chi1
     &              - g(d3,d2)*dchidx(d1))/chi1
             endif
             if (d3 .lt. 3) then
               chris(d0,d1,d2) = chris(d0,d1,d2) -
     &           (0.500d0)*gu(d0,d3)*dgammadx(d1,d2,d3)
               chriss(d0,d1,d2) = chriss(d0,d1,d2) -
     &           gu(d0,d3)*((0.500d0)*dgammadx(d1,d2,d3)*chi1
     &              - g(d1,d2)*dchidx(d3))/chi1
             endif
           enddo
         enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,2
           chris1st(d0,d1,d2) = 0
           do d3 = 0,2
             chris1st(d0,d1,d2) = chris1st(d0,d1,d2) + g(d0,d3)*chris(d3
     &,d1,d2)
           enddo
         enddo; enddo; enddo
         do d0 = 0,2
           Gamma1d(d0) = 0
           do d1 = 0,2; do d2 = 0,2
             Gamma1d(d0) = Gamma1d(d0) + chris(d0,d1,d2)*gu(d1,d2)
           enddo; enddo
         enddo
         call GETV(Vphi, phi(i0,i1,i2), phi0, alp, s, eps)
         call GETVPHI(Vphiphi, phi(i0,i1,i2), phi0, alp, s, eps)
         Vt = -phiM(i0,i1,i2)**2 + 2*Vphi
         do d0 = 0,3-1; do d1 = 0,3-1
           Vt = Vt + gu(d0,d1)*chi2*dphidx(d0)*dphidx(d1)
         enddo; enddo
         betaidiphi = 0
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           if (beta(i0,i1,i2,d0) .lt. 0) then
            betaidiphi =
     &            betaidiphi
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                -phi(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*phi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*phi(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*phi(i0,i1,i2        )
     &            +  3*phi(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           else
            betaidiphi =
     &            betaidiphi
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                 phi(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*phi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*phi(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*phi(i0,i1,i2        )
     &            -  3*phi(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           endif
         enddo
         dphidt2 = lapse*phiM(i0,i1,i2) + betaidiphi
         dphidt2 = dphidt2**2
         beta2 = 0
         do d0 = 0,2; do d1 = 0,2
           beta2 = beta2 + g(d0,d1)/chi2*
     &       beta(i0,i1,i2,d0)*beta(i0,i1,i2,d1)
         enddo; enddo
         n(0) = 0; n(1) = 0; n(2) = 0; n(3) = -lapse
         do d0 = 0,2
           nu(d0) = -beta(i0,i1,i2,d0)/lapse
         enddo
         nu(3) = 1.0/lapse
         T(3,3) = dphidt2 + (0.500d0)*(lapse**2 - beta2)*Vt
         do d0 = 0,2; do d1 = 0,2
           T(d0, d1) = -(0.500d0)*g(d0,d1)*Vt/chi2
           if (d0 .lt. 3 .and. d1 .lt. 3) then
             T(d0, d1) = T(d0, d1) + dphidx(d0)*dphidx(d1)
           endif
         enddo; enddo
         do d0 = 0,2
           T(d0,3) = dphidx(d0)*lapse*phiM(i0,i1,i2) + dphidx(d0)*betaid
     &iphi
             do d1 = 0,3-1
               T(d0,3) = T(d0,3) - (0.500d0)*g(d0,d1)*beta(i0,i1,i2,d1)*
     &Vt/chi2
             enddo
           T(3,d0) = T(d0,3)
         enddo
         rho = 0;
         do d0 = 0,3; do d1 = 0,3
           rho = rho + nu(d0)*nu(d1)*T(d0,d1)
         enddo; enddo
         do d0 = 0,2
           Svec(d0) = 0
           do d1 = 0,3
             Svec(d0) = Svec(d0) - nu(d1)*T(d1,d0)
           enddo
         enddo
         do d0 = 0,2; do d1 = 0,2
          Sten(d0,d1) = T(d0,d1)
         enddo; enddo
         trSten = 0
         do d0 = 0,2; do d1 = 0,2
           trSten = trSten + gu(d0,d1)*chi2*T(d0,d1)
         enddo; enddo
         dchidt(i0,i1,i2) = (1.000d0 / 3.000d0)*chi1*(lapse*K(i0,i1,i2) 
     &- trdbetadx)
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           if (beta(i0,i1,i2,d0) .lt. 0) then
            dchidt(i0,i1,i2) =
     &            dchidt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                -chi(i0-3*ii0,i1-3*ii1,i2-3*ii2)**2.0*0.5/chi1
     &            +  6*chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)**2.0*0.5/chi1
     &            - 18*chi(i0-ii0,i1-ii1,i2-ii2  )**2.0*0.5/chi1
     &            + 10*chi(i0,i1,i2        )**2.0*0.5/chi1
     &            +  3*chi(i0+ii0,i1+ii1,i2+ii2  )**2.0*0.5/chi1
     &            )
           else
            dchidt(i0,i1,i2) =
     &            dchidt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                 chi(i0+3*ii0,i1+3*ii1,i2+3*ii2)**2.0*0.5/chi1
     &            -  6*chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)**2.0*0.5/chi1
     &            + 18*chi(i0+ii0,i1+ii1,i2+ii2  )**2.0*0.5/chi1
     &            - 10*chi(i0,i1,i2        )**2.0*0.5/chi1
     &            -  3*chi(i0-ii0,i1-ii1,i2-ii2  )**2.0*0.5/chi1
     &            )
           endif
         enddo
         do d0 = 0,2; do d1 = 0,2
           dgammadt(i0,i1,i2,IDX(d0,d1)) =
     &       - 2*lapse*A(i0,i1,i2,IDX(d0,d1)) 
     &       - 2./3.*g(d0,d1)*trdbetadx
           do d2 = 0,2
             dgammadt(i0,i1,i2,IDX(d0,d1)) =
     &           dgammadt(i0,i1,i2,IDX(d0,d1))
     &         + g(d2,d0)*dbetadx(d2,d1) + g(d2,d1)*dbetadx(d2,d0)
           enddo
         do d2 = 0,3-1
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           if (beta(i0,i1,i2,d2) .lt. 0) then
            dgammadt(i0,i1,i2,IDX(d0,d1)) =
     &            dgammadt(i0,i1,i2,IDX(d0,d1))
     &          + dxinv/12*beta(i0,i1,i2,d2)*(
     &                -gamma(i0-3*ii0,i1-3*ii1,i2-3*ii2,IDX(d0,d1))
     &            +  6*gamma(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &            - 18*gamma(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &            + 10*gamma(i0,i1,i2        ,IDX(d0,d1))
     &            +  3*gamma(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &            )
           else
            dgammadt(i0,i1,i2,IDX(d0,d1)) =
     &             dgammadt(i0,i1,i2,IDX(d0,d1)) 
     &          + dxinv/12*beta(i0,i1,i2,d2)*(
     &                 gamma(i0+3*ii0,i1+3*ii1,i2+3*ii2,IDX(d0,d1))
     &            -  6*gamma(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &            + 18*gamma(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &            - 10*gamma(i0,i1,i2        ,IDX(d0,d1))
     &            -  3*gamma(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &            )
           endif
         enddo
         enddo; enddo
         dKdt(i0,i1,i2) = (1.000d0 / 3.000d0)*K(i0,i1,i2)**2
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,2; do d3 = 0,2
           dKdt(i0,i1,i2) = dKdt(i0,i1,i2)
     &     + A(i0,i1,i2,IDX(d0,d1))*gu(d0,d2)*gu(d1,d3)*A(i0,i1,i2,IDX(d
     &2,d3))
         enddo; enddo; enddo; enddo
         dKdt(i0,i1,i2) = lapse*dKdt(i0,i1,i2)
         do d0 = 0,3-1; do d1 = 0,3-1
           dKdt(i0,i1,i2) = dKdt(i0,i1,i2)
     &     - (
     &         chi2*gu(d0,d1)*d2alphadxdy(d0,d1)
     &       )
         enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,3-1
           dKdt(i0,i1,i2) = dKdt(i0,i1,i2)
     &     + (
     &         chi2*gu(d0,d1)*chriss(d2,d0,d1)*dalphadx(d2)
     &       )
         enddo; enddo; enddo
         dKdt(i0,i1,i2) = dKdt(i0,i1,i2)
     &     + 4*(3.14159265358979323846264338327950288d0)*lapse*(trSten +
     & rho)
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           if (beta(i0,i1,i2,d0) .lt. 0) then
            dKdt(i0,i1,i2) =
     &            dKdt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                -K(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*K(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*K(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*K(i0,i1,i2        )
     &            +  3*K(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           else
            dKdt(i0,i1,i2) =
     &            dKdt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                 K(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*K(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*K(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*K(i0,i1,i2        )
     &            -  3*K(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           endif
         enddo
         do d0 = 0,2; do d1 = 0,2
           R(d0,d1) = 0
           do d2 = 0,3-1; do d3 = 0,3-1
             R(d0,d1) = R(d0,d1) - (0.500d0)*gu(d2,d3)*d2gammadxdy(d0,d1
     &,d2,d3)
           enddo; enddo
           do d2 = 0,2
             if (d1 .lt. 3) then
               R(d0,d1) = R(d0,d1) + (0.500d0)*g(d2,d0)*dGamma1dx(d2,d1)
             endif
             if (d0 .lt. 3) then
               R(d0,d1) = R(d0,d1) + (0.500d0)*g(d2,d1)*dGamma1dx(d2,d0)
             endif
           enddo
           do d2 = 0,2
             R(d0,d1) = R(d0,d1)
     &       + (0.500d0)*Gamma1(i0,i1,i2,d2)*chris1st(d0,d1,d2)
     &       + (0.500d0)*Gamma1(i0,i1,i2,d2)*chris1st(d1,d0,d2)
             do d3 = 0,2; do d4 = 0,2
               R(d0,d1) = R(d0,d1)
     &         + gu(d3,d4)*chris(d2,d3,d0)*chris1st(d1,d2,d4)
     &         + gu(d3,d4)*chris(d2,d3,d1)*chris1st(d0,d2,d4)
     &         + gu(d3,d4)*chris(d2,d0,d4)*chris1st(d2,d3,d1)
             enddo; enddo
           enddo
         enddo; enddo
         do d0 = 0,2; do d1 = 0,2
           Rchi(d0,d1) = 0
           do d2 = 0,2; do d3 = 0,2
             Rchi(d0,d1) = Rchi(d0,d1) - g(d0,d1)*gu(d2,d3)*dchidx(d2)*d
     &chidx(d3)
           enddo; enddo
           Rchi(d0,d1) = 2*Rchi(d0,d1)/chi1
           do d2 = 0,3-1; do d3 = 0,3-1
             Rchi(d0,d1) = Rchi(d0,d1)
     &       + (
     &           g(d0,d1)*gu(d2,d3)*d2chidxdy(d2,d3)
     &         )
           enddo; enddo
           do d2 = 0,2; do d3 = 0,2; do d4 = 0,3-1
             Rchi(d0,d1) = Rchi(d0,d1)
     &       - (
     &           g(d0,d1)*gu(d2,d3)*chris(d4,d2,d3)*dchidx(d4)
     &         )
           enddo; enddo; enddo
           Rchi(d0,d1) = Rchi(d0,d1) + d2chidxdy(d0,d1)
           do d2 = 0,2;
             Rchi(d0,d1) = Rchi(d0,d1) - chris(d2,d0,d1)*dchidx(d2)
           enddo 
           Rchi(d0,d1) = Rchi(d0,d1)/chi1
         enddo; enddo
         Rscalar = 0
         do d0 = 0,2; do d1 = 0,2; 
           Rscalar = Rscalar
     &       + (R(d0,d1)+Rchi(d0,d1))*gu(d0,d1)*chi2
         enddo; enddo;
         do d0 = 0,2; do d1 = 0,2
           dAdt(i0,i1,i2,IDX(d0,d1)) =
     &        K(i0,i1,i2)*A(i0,i1,i2,IDX(d0,d1))
           do d2 = 0,2; do d3 = 0,2
             dAdt(i0,i1,i2,IDX(d0,d1)) =
     &         dAdt(i0,i1,i2,IDX(d0,d1))
     &       - 2*A(i0,i1,i2,IDX(d0,d2))*gu(d2,d3)*A(i0,i1,i2,IDX(d3,d1))
           enddo; enddo
           dAdt(i0,i1,i2,IDX(d0,d1)) =
     &       dAdt(i0,i1,i2,IDX(d0,d1))*lapse
           do d2 = 0,2
             dAdt(i0,i1,i2,IDX(d0,d1)) =
     &         dAdt(i0,i1,i2,IDX(d0,d1))
     &       + A(i0,i1,i2,IDX(d2,d0))*dbetadx(d2,d1)
     &       + A(i0,i1,i2,IDX(d2,d1))*dbetadx(d2,d0)
           enddo
           dAdt(i0,i1,i2,IDX(d0,d1)) =
     &      + dAdt(i0,i1,i2,IDX(d0,d1))
     &      - (2.000d0 / 3.000d0)*A(i0,i1,i2,IDX(d0,d1))*trdbetadx
           dAdt(i0,i1,i2,IDX(d0,d1)) =
     &        dAdt(i0,i1,i2,IDX(d0,d1))
     &      + lapse*
     &     (
     &        (chi2*R(d0,d1)+chi2*Rchi(d0,d1) - (1.000d0 / 3.000d0)*g(d0
     &,d1)*Rscalar)
     &        - 8*(3.14159265358979323846264338327950288d0)*(chi2*Sten(d
     &0,d1) - (1.000d0 / 3.000d0)*g(d0,d1)*trSten)
     &     )
           do d2 = 0,2
             dAdt(i0,i1,i2,IDX(d0,d1)) =
     &         dAdt(i0,i1,i2,IDX(d0,d1))
     &       + chi2*chriss(d2,d0,d1)*dalphadx(d2)
           enddo
           dAdt(i0,i1,i2,IDX(d0,d1)) =
     &         dAdt(i0,i1,i2,IDX(d0,d1))
     &       - chi2*(d2alphadxdy(d0,d1))
           do d2 = 0,2; do d3 = 0,2 
             dAdt(i0,i1,i2,IDX(d0,d1)) =
     &          dAdt(i0,i1,i2,IDX(d0,d1))
     &        + chi2*(1.000d0 / 3.000d0)*g(d0,d1)*gu(d2,d3)*d2alphadxdy(
     &d2,d3) 
             do d4 = 0,2
               dAdt(i0,i1,i2,IDX(d0,d1)) =
     &            dAdt(i0,i1,i2,IDX(d0,d1)) 
     &          - chi2*(1.000d0 / 3.000d0)*g(d0,d1)*gu(d2,d3)*chriss(d4,
     &d2,d3)*dalphadx(d4)
             enddo
           enddo; enddo
         do d2 = 0,3-1
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           if (beta(i0,i1,i2,d2) .lt. 0) then
            dAdt(i0,i1,i2,IDX(d0,d1)) =
     &            dAdt(i0,i1,i2,IDX(d0,d1))
     &          + dxinv/12*beta(i0,i1,i2,d2)*(
     &                -A(i0-3*ii0,i1-3*ii1,i2-3*ii2,IDX(d0,d1))
     &            +  6*A(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &            - 18*A(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &            + 10*A(i0,i1,i2        ,IDX(d0,d1))
     &            +  3*A(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &            )
           else
            dAdt(i0,i1,i2,IDX(d0,d1)) =
     &            dAdt(i0,i1,i2,IDX(d0,d1))
     &          + dxinv/12*beta(i0,i1,i2,d2)*(
     &                 A(i0+3*ii0,i1+3*ii1,i2+3*ii2,IDX(d0,d1))
     &            -  6*A(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &            + 18*A(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &            - 10*A(i0,i1,i2        ,IDX(d0,d1))
     &            -  3*A(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &            )
           endif
         enddo
        enddo; enddo
         dThetadt(i0,i1,i2) = 0
        do d0 = 0,2
          dGamma1dt(i0,i1,i2,d0) = 0
          do d1 = 0,2
            dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &      - 8*(3.14159265358979323846264338327950288d0)*gu(d0,d1)*Svec
     &(d1)
          enddo
          do d1 = 0,3-1
            dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &        - (2.000d0 / 3.000d0)*gu(d0,d1)*dKdx(d1)
            do d2 = 0,2; do d3 = 0,2
              dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &          - 3*gu(d0,d2)*gu(d1,d3)*A(i0,i1,i2,IDX(d2,d3))
     &              *dchidx(d1)/chi1
            enddo; enddo 
          enddo
          do d1 = 0,2; do d2 = 0,2; do d3 = 0,2; do d4 = 0,2
            dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &        + chris(d0,d1,d2)*gu(d1,d3)*gu(d2,d4)*A(i0,i1,i2,IDX(d3,d4
     &))
          enddo; enddo; enddo; enddo
          dGamma1dt(i0,i1,i2,d0) =
     &      dGamma1dt(i0,i1,i2,d0)*2*lapse
          dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &      + (2.000d0 / 3.000d0)*Gamma1d(d0)*trdbetadx
          do d1 = 0,3-1
            dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &        - Gamma1d(d1)*dbetadx(d0,d1)
            do d2 = 0,2; do d3 = 0,2
              dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &          - 2*gu(d0,d2)*gu(d1,d3)*A(i0,i1,i2,IDX(d2,d3))*dalphadx(
     &d1)
            enddo; enddo
          enddo
          do d1 = 0,3-1; do d2 = 0,3-1
            dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &        + (1.000d0 / 3.000d0)*gu(d0,d1)*d2betadxdy(d2,d1,d2)
     &        + gu(d1,d2)*d2betadxdy(d0,d1,d2)
          enddo; enddo
          do d1 = 0,3-1
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
           if (beta(i0,i1,i2,d1) .lt. 0) then
            dGamma1dt(i0,i1,i2,d0) =
     &            dGamma1dt(i0,i1,i2,d0)
     &          + dxinv/12*beta(i0,i1,i2,d1)*(
     &                -Gamma1(i0-3*ii0,i1-3*ii1,i2-3*ii2,d0)
     &            +  6*Gamma1(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &            - 18*Gamma1(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &            + 10*Gamma1(i0,i1,i2        ,d0)
     &            +  3*Gamma1(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &            )
           else
            dGamma1dt(i0,i1,i2,d0) =
     &            dGamma1dt(i0,i1,i2,d0)
     &          + dxinv/12*beta(i0,i1,i2,d1)*(
     &                 Gamma1(i0+3*ii0,i1+3*ii1,i2+3*ii2,d0)
     &            -  6*Gamma1(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &            + 18*Gamma1(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &            - 10*Gamma1(i0,i1,i2        ,d0)
     &            -  3*Gamma1(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &            )
           endif
         enddo
        enddo
        dalphadt(i0,i1,i2) = -2.0*lapse*K(i0,i1,i2)
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           if (beta(i0,i1,i2,d0) .lt. 0) then
            dalphadt(i0,i1,i2) =
     &            dalphadt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                -alpha(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*alpha(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*alpha(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*alpha(i0,i1,i2        )
     &            +  3*alpha(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           else
            dalphadt(i0,i1,i2) =
     &            dalphadt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                 alpha(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*alpha(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*alpha(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*alpha(i0,i1,i2        )
     &            -  3*alpha(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           endif
         enddo
        do d0 = 0,2
          dbetadt(i0,i1,i2,d0) = mubeta1*B(i0,i1,i2,d0)
        enddo
        do d0 = 0,2
          dBdt(i0,i1,i2,d0) =
     &          dGamma1dt(i0,i1,i2,d0)
     &        - eta*B(i0,i1,i2,d0)
        enddo
        dphidt(i0,i1,i2) = lapse*phiM(i0,i1,i2)
        do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           if (beta(i0,i1,i2,d0) .lt. 0) then
           dphidt(i0,i1,i2) =
     &            dphidt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                -phi(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*phi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*phi(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*phi(i0,i1,i2        )
     &            +  3*phi(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           else
           dphidt(i0,i1,i2) =
     &            dphidt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                 phi(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*phi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*phi(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*phi(i0,i1,i2        )
     &            -  3*phi(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           endif
        enddo
        dphiMdt(i0,i1,i2) = lapse*(K(i0,i1,i2)*phiM(i0,i1,i2)
     &    - Vphiphi)
        do d0 = 0,3-1; do d1 = 0,3-1
          do d2 = 0,2
            dphiMdt(i0,i1,i2) = dphiMdt(i0,i1,i2) 
     &      - chi2*gu(d0,d1)*chriss(d2,d0,d1)*dphidx(d2)*lapse
          enddo 
          dphiMdt(i0,i1,i2) = dphiMdt(i0,i1,i2) 
     &      + chi2*gu(d0,d1)*d2phidxdy(d0,d1)*lapse
     &      + chi2*gu(d0,d1)*dalphadx(d0)*dphidx(d1)
        enddo; enddo
        do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           if (beta(i0,i1,i2,d0) .lt. 0) then
           dphiMdt(i0,i1,i2) =
     &            dphiMdt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                -phiM(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*phiM(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*phiM(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*phiM(i0,i1,i2        )
     &            +  3*phiM(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           else
           dphiMdt(i0,i1,i2) =
     &            dphiMdt(i0,i1,i2)
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                 phiM(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*phiM(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*phiM(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*phiM(i0,i1,i2        )
     &            -  3*phiM(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           endif
        enddo
        do d0 = 0,3-1
            ii0 = CHF_ID(d0,0)
            ii1 = CHF_ID(d0,1)
            ii2 = CHF_ID(d0,2)
          dchidt(i0,i1,i2) = dchidt(i0,i1,i2)
     &    + sigma/(64*dx) * (
     &         chi(i0-3*ii0,i1-3*ii1,i2-3*ii2)**2.0*0.5/chi1
     &    -  6*chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)**2.0*0.5/chi1
     &    + 15*chi(i0-ii0,i1-ii1,i2-ii2  )**2.0*0.5/chi1
     &    - 20*chi(i0,i1,i2        )**2.0*0.5/chi1
     &    + 15*chi(i0+ii0,i1+ii1,i2+ii2  )**2.0*0.5/chi1
     &    -  6*chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)**2.0*0.5/chi1
     &    +    chi(i0+3*ii0,i1+3*ii1,i2+3*ii2)**2.0*0.5/chi1
     &    )
        enddo
        do d0 = 0,2; do d1 = 0,2; do d2 = 0,3-1
          if (d0 .gt. d1) cycle
            ii0 = CHF_ID(d2,0)
            ii1 = CHF_ID(d2,1)
            ii2 = CHF_ID(d2,2)
          dgammadt(i0,i1,i2,IDX(d0,d1)) = dgammadt(i0,i1,i2,IDX(d0,d1))
     &    + sigma/(64*dx) * (
     &         gamma(i0-3*ii0,i1-3*ii1,i2-3*ii2,IDX(d0,d1))
     &    -  6*gamma(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &    + 15*gamma(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &    - 20*gamma(i0,i1,i2        ,IDX(d0,d1))
     &    + 15*gamma(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &    -  6*gamma(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &    +    gamma(i0+3*ii0,i1+3*ii1,i2+3*ii2,IDX(d0,d1))
     &    )
        enddo; enddo; enddo
        do d0 = 0,3-1
            ii0 = CHF_ID(d0,0)
            ii1 = CHF_ID(d0,1)
            ii2 = CHF_ID(d0,2)
          dKdt(i0,i1,i2) = dKdt(i0,i1,i2)
     &    + sigma/(64*dx) * (
     &         K(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &    -  6*K(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &    + 15*K(i0-ii0,i1-ii1,i2-ii2  )
     &    - 20*K(i0,i1,i2        )
     &    + 15*K(i0+ii0,i1+ii1,i2+ii2  )
     &    -  6*K(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &    +    K(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &    )
        enddo
        do d0 = 0,2; do d1 = 0,2; do d2 = 0,3-1
          if (d0 .gt. d1) cycle
            ii0 = CHF_ID(d2,0)
            ii1 = CHF_ID(d2,1)
            ii2 = CHF_ID(d2,2)
          dAdt(i0,i1,i2,IDX(d0,d1)) = dAdt(i0,i1,i2,IDX(d0,d1))
     &    + sigma/(64*dx) * (
     &         A(i0-3*ii0,i1-3*ii1,i2-3*ii2,IDX(d0,d1))
     &    -  6*A(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &    + 15*A(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &    - 20*A(i0,i1,i2        ,IDX(d0,d1))
     &    + 15*A(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &    -  6*A(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &    +    A(i0+3*ii0,i1+3*ii1,i2+3*ii2,IDX(d0,d1))
     &    )
        enddo; enddo; enddo
        do d0 = 0,2; do d1 = 0,3-1
            ii0 = CHF_ID(d1,0)
            ii1 = CHF_ID(d1,1)
            ii2 = CHF_ID(d1,2)
          dGamma1dt(i0,i1,i2,d0) = dGamma1dt(i0,i1,i2,d0)
     &    + sigma/(64*dx) * (
     &         Gamma1(i0-3*ii0,i1-3*ii1,i2-3*ii2,d0)
     &    -  6*Gamma1(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &    + 15*Gamma1(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &    - 20*Gamma1(i0,i1,i2        ,d0)
     &    + 15*Gamma1(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &    -  6*Gamma1(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &    +    Gamma1(i0+3*ii0,i1+3*ii1,i2+3*ii2,d0)
     &    )
        enddo; enddo
        do d0 = 0,3-1
            ii0 = CHF_ID(d0,0)
            ii1 = CHF_ID(d0,1)
            ii2 = CHF_ID(d0,2)
          dalphadt(i0,i1,i2) = dalphadt(i0,i1,i2)
     &    + sigma/(64*dx) * (
     &         alpha(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &    -  6*alpha(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &    + 15*alpha(i0-ii0,i1-ii1,i2-ii2  )
     &    - 20*alpha(i0,i1,i2        )
     &    + 15*alpha(i0+ii0,i1+ii1,i2+ii2  )
     &    -  6*alpha(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &    +    alpha(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &    )
        enddo
        do d0 = 0,2; do d1 = 0,3-1
            ii0 = CHF_ID(d1,0)
            ii1 = CHF_ID(d1,1)
            ii2 = CHF_ID(d1,2)
          dbetadt(i0,i1,i2,d0) = dbetadt(i0,i1,i2,d0)
     &    + sigma/(64*dx) * (
     &         beta(i0-3*ii0,i1-3*ii1,i2-3*ii2,d0)
     &    -  6*beta(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &    + 15*beta(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &    - 20*beta(i0,i1,i2        ,d0)
     &    + 15*beta(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &    -  6*beta(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &    +    beta(i0+3*ii0,i1+3*ii1,i2+3*ii2,d0)
     &    )
        enddo; enddo
        do d0 = 0,2; do d1 = 0,3-1
            ii0 = CHF_ID(d1,0)
            ii1 = CHF_ID(d1,1)
            ii2 = CHF_ID(d1,2)
          dBdt(i0,i1,i2,d0) = dBdt(i0,i1,i2,d0)
     &    + sigma/(64*dx) * (
     &         B(i0-3*ii0,i1-3*ii1,i2-3*ii2,d0)
     &    -  6*B(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &    + 15*B(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &    - 20*B(i0,i1,i2        ,d0)
     &    + 15*B(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &    -  6*B(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &    +    B(i0+3*ii0,i1+3*ii1,i2+3*ii2,d0)
     &    )
        enddo; enddo
        do d0 = 0,3-1
            ii0 = CHF_ID(d0,0)
            ii1 = CHF_ID(d0,1)
            ii2 = CHF_ID(d0,2)
          dphidt(i0,i1,i2) = dphidt(i0,i1,i2)
     &    + sigma/(64*dx) * (
     &         phi(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &    -  6*phi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &    + 15*phi(i0-ii0,i1-ii1,i2-ii2  )
     &    - 20*phi(i0,i1,i2        )
     &    + 15*phi(i0+ii0,i1+ii1,i2+ii2  )
     &    -  6*phi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &    +    phi(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &    )
        enddo
        do d0 = 0,3-1
            ii0 = CHF_ID(d0,0) 
            ii1 = CHF_ID(d0,1)
            ii2 = CHF_ID(d0,2)
          dphiMdt(i0,i1,i2) = dphiMdt(i0,i1,i2)
     &    + sigma/(64*dx) * (
     &         phiM(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &    -  6*phiM(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &    + 15*phiM(i0-ii0,i1-ii1,i2-ii2  )
     &    - 20*phiM(i0,i1,i2        )
     &    + 15*phiM(i0+ii0,i1+ii1,i2+ii2  )
     &    -  6*phiM(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &    +    phiM(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &    )
        enddo
        dchidt(i0,i1,i2) = dchidt(i0,i1,i2)*2.0*chi(i0,i1,i2)
      enddo
      enddo
      enddo
      return
      end
      subroutine GETBSSNCONSTRF(
     &           H
     &           ,iHlo0,iHlo1,iHlo2
     &           ,iHhi0,iHhi1,iHhi2
     &           ,M
     &           ,iMlo0,iMlo1,iMlo2
     &           ,iMhi0,iMhi1,iMhi2
     &           ,nMcomp
     &           ,chi
     &           ,ichilo0,ichilo1,ichilo2
     &           ,ichihi0,ichihi1,ichihi2
     &           ,gamma
     &           ,igammalo0,igammalo1,igammalo2
     &           ,igammahi0,igammahi1,igammahi2
     &           ,ngammacomp
     &           ,K
     &           ,iKlo0,iKlo1,iKlo2
     &           ,iKhi0,iKhi1,iKhi2
     &           ,A
     &           ,iAlo0,iAlo1,iAlo2
     &           ,iAhi0,iAhi1,iAhi2
     &           ,nAcomp
     &           ,Gamma1
     &           ,iGamma1lo0,iGamma1lo1,iGamma1lo2
     &           ,iGamma1hi0,iGamma1hi1,iGamma1hi2
     &           ,nGamma1comp
     &           ,alpha
     &           ,ialphalo0,ialphalo1,ialphalo2
     &           ,ialphahi0,ialphahi1,ialphahi2
     &           ,beta
     &           ,ibetalo0,ibetalo1,ibetalo2
     &           ,ibetahi0,ibetahi1,ibetahi2
     &           ,nbetacomp
     &           ,phi
     &           ,iphilo0,iphilo1,iphilo2
     &           ,iphihi0,iphihi1,iphihi2
     &           ,phiM
     &           ,iphiMlo0,iphiMlo1,iphiMlo2
     &           ,iphiMhi0,iphiMhi1,iphiMhi2
     &           ,dx
     &           ,s
     &           ,iboxlo0,iboxlo1,iboxlo2
     &           ,iboxhi0,iboxhi1,iboxhi2
     &           )
      implicit none
      integer CHF_ID(0:5,0:5)
      data CHF_ID/ 1,0,0,0,0,0 ,0,1,0,0,0,0 ,0,0,1,0,0,0 ,0,0,0,1,0,0 ,0
     &,0,0,0,1,0 ,0,0,0,0,0,1 /
      integer iHlo0,iHlo1,iHlo2
      integer iHhi0,iHhi1,iHhi2
      REAL*8 H(
     &           iHlo0:iHhi0,
     &           iHlo1:iHhi1,
     &           iHlo2:iHhi2)
      integer nMcomp
      integer iMlo0,iMlo1,iMlo2
      integer iMhi0,iMhi1,iMhi2
      REAL*8 M(
     &           iMlo0:iMhi0,
     &           iMlo1:iMhi1,
     &           iMlo2:iMhi2,
     &           0:nMcomp-1)
      integer ichilo0,ichilo1,ichilo2
      integer ichihi0,ichihi1,ichihi2
      REAL*8 chi(
     &           ichilo0:ichihi0,
     &           ichilo1:ichihi1,
     &           ichilo2:ichihi2)
      integer ngammacomp
      integer igammalo0,igammalo1,igammalo2
      integer igammahi0,igammahi1,igammahi2
      REAL*8 gamma(
     &           igammalo0:igammahi0,
     &           igammalo1:igammahi1,
     &           igammalo2:igammahi2,
     &           0:ngammacomp-1)
      integer iKlo0,iKlo1,iKlo2
      integer iKhi0,iKhi1,iKhi2
      REAL*8 K(
     &           iKlo0:iKhi0,
     &           iKlo1:iKhi1,
     &           iKlo2:iKhi2)
      integer nAcomp
      integer iAlo0,iAlo1,iAlo2
      integer iAhi0,iAhi1,iAhi2
      REAL*8 A(
     &           iAlo0:iAhi0,
     &           iAlo1:iAhi1,
     &           iAlo2:iAhi2,
     &           0:nAcomp-1)
      integer nGamma1comp
      integer iGamma1lo0,iGamma1lo1,iGamma1lo2
      integer iGamma1hi0,iGamma1hi1,iGamma1hi2
      REAL*8 Gamma1(
     &           iGamma1lo0:iGamma1hi0,
     &           iGamma1lo1:iGamma1hi1,
     &           iGamma1lo2:iGamma1hi2,
     &           0:nGamma1comp-1)
      integer ialphalo0,ialphalo1,ialphalo2
      integer ialphahi0,ialphahi1,ialphahi2
      REAL*8 alpha(
     &           ialphalo0:ialphahi0,
     &           ialphalo1:ialphahi1,
     &           ialphalo2:ialphahi2)
      integer nbetacomp
      integer ibetalo0,ibetalo1,ibetalo2
      integer ibetahi0,ibetahi1,ibetahi2
      REAL*8 beta(
     &           ibetalo0:ibetahi0,
     &           ibetalo1:ibetahi1,
     &           ibetalo2:ibetahi2,
     &           0:nbetacomp-1)
      integer iphilo0,iphilo1,iphilo2
      integer iphihi0,iphihi1,iphihi2
      REAL*8 phi(
     &           iphilo0:iphihi0,
     &           iphilo1:iphihi1,
     &           iphilo2:iphihi2)
      integer iphiMlo0,iphiMlo1,iphiMlo2
      integer iphiMhi0,iphiMhi1,iphiMhi2
      REAL*8 phiM(
     &           iphiMlo0:iphiMhi0,
     &           iphiMlo1:iphiMhi1,
     &           iphiMlo2:iphiMhi2)
      REAL*8 dx
      REAL*8 s
      integer iboxlo0,iboxlo1,iboxlo2
      integer iboxhi0,iboxhi1,iboxhi2
      integer i0,i1,i2, ii0,ii1,ii2, jj0,jj1,jj2
      REAL*8 dxinv, dxinv2, detg
      REAL*8 g(0:2,0:2), gu(0:2,0:2)
      REAL*8 dgammadx(0:2,0:2,0:3-1)
      REAL*8 d2gammadxdy(0:2,0:2,0:3-1,0:3-1)
      REAL*8 dchidx(0:3-1)
      REAL*8 dphidx(0:3-1)
      REAL*8 Sten(0:2,0:2), trSten, Svec(0:2), Vphi, rho
      REAL*8 dphidt2, beta2, Vt, betaidiphi, chi2, chi1, lapse
      REAL*8 n(0:3), nu(0:3), T(0:3,0:3)
      REAL*8 alp, phi0, eps
      integer d0, d1, d2, d3, d4, d5
      integer IDX(0:2, 0:2)
      data IDX / 0, 1, 2, 1, 3, 4, 2, 4, 5  /
      dxinv  = (1.0d0)/dx
      dxinv2 = (1.0d0)/(dx*dx)
      do i2 = iboxlo2,iboxhi2
      do i1 = iboxlo1,iboxhi1
      do i0 = iboxlo0,iboxhi0
         lapse = alpha(i0,i1,i2)
         chi1 = chi(i0,i1,i2)         
         chi2 = chi1**2
         do d0 = 0,2; do d1 = 0,2
           g(d0,d1) = gamma(i0,i1,i2, IDX(d0,d1))
         enddo; enddo
         detg = g(0,0)*(g(1,1)*g(2,2)-g(1,2)*g(2,1))-
     &          g(0,1)*(g(2,2)*g(1,0)-g(1,2)*g(2,0))+
     &          g(0,2)*(g(1,0)*g(2,1)-g(1,1)*g(2,0))
         gu(0,0) = (g(1,1)*g(2,2)-g(1,2)*g(2,1))/detg
         gu(0,1) = (g(2,0)*g(1,2)-g(1,0)*g(2,2))/detg
         gu(0,2) = (g(1,0)*g(2,1)-g(2,0)*g(1,1))/detg
         gu(1,0) = (g(2,1)*g(0,2)-g(0,1)*g(2,2))/detg
         gu(1,1) = (g(0,0)*g(2,2)-g(0,2)*g(2,0))/detg
         gu(1,2) = (g(2,0)*g(0,1)-g(0,0)*g(2,1))/detg
         gu(2,0) = (g(0,1)*g(1,2)-g(1,1)*g(0,2))/detg
         gu(2,1) = (g(0,2)*g(1,0)-g(0,0)*g(1,2))/detg
         gu(2,2) = (g(0,0)*g(1,1)-g(0,1)*g(1,0))/detg
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,3-1
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           dgammadx(d0,d1,d2) = dxinv/12 * (
     &         gamma(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &     - 8*gamma(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &     + 8*gamma(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &     -   gamma(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &       )
         enddo; enddo; enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           dchidx(d0) = dxinv/12 * (
     &         chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)**2.0*0.5/chi1
     &     - 8*chi(i0-ii0,i1-ii1,i2-ii2  )**2.0*0.5/chi1
     &     + 8*chi(i0+ii0,i1+ii1,i2+ii2  )**2.0*0.5/chi1
     &     -   chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)**2.0*0.5/chi1
     &       )
         enddo
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           dphidx(d0) = dxinv/12 * (
     &         phi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*phi(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*phi(i0+ii0,i1+ii1,i2+ii2  )
     &     -   phi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         call GETV(Vphi, phi(i0,i1,i2), phi0, alp, s, eps)
         Vt = -phiM(i0,i1,i2)**2 + 2*Vphi
         do d0 = 0,3-1; do d1 = 0,3-1
           Vt = Vt + gu(d0,d1)*chi2*dphidx(d0)*dphidx(d1)
         enddo; enddo
         betaidiphi = 0
         do d0 = 0,3-1
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           if (beta(i0,i1,i2,d0) .lt. 0) then
            betaidiphi =
     &            betaidiphi
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                -phi(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*phi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*phi(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*phi(i0,i1,i2        )
     &            +  3*phi(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           else
            betaidiphi =
     &            betaidiphi
     &          + dxinv/12*beta(i0,i1,i2,d0)*(
     &                 phi(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*phi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*phi(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*phi(i0,i1,i2        )
     &            -  3*phi(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           endif
         enddo
         dphidt2 = lapse*phiM(i0,i1,i2) + betaidiphi
         dphidt2 = dphidt2**2
         beta2 = 0
         do d0 = 0,2; do d1 = 0,2
           beta2 = beta2 + g(d0,d1)/chi2*
     &       beta(i0,i1,i2,d0)*beta(i0,i1,i2,d1)
         enddo; enddo
         n(0) = 0; n(1) = 0; n(2) = 0; n(3) = -lapse
         do d0 = 0,2
           nu(d0) = -beta(i0,i1,i2,d0)/lapse
         enddo
         nu(3) = 1.0/lapse
         T(3,3) = dphidt2 + (0.500d0)*(lapse**2 - beta2)*Vt
         do d0 = 0,2; do d1 = 0,2
           T(d0, d1) = -(0.500d0)*g(d0,d1)*Vt/chi2
           if (d0 .lt. 3 .and. d1 .lt. 3) then
             T(d0, d1) = T(d0, d1) + dphidx(d0)*dphidx(d1)
           endif
         enddo; enddo
         do d0 = 0,2
           T(d0,3) = dphidx(d0)*lapse*phiM(i0,i1,i2) + dphidx(d0)*betaid
     &iphi
             do d1 = 0,3-1
               T(d0,3) = T(d0,3) - (0.500d0)*g(d0,d1)*beta(i0,i1,i2,d1)*
     &Vt/chi2
             enddo
           T(3,d0) = T(d0,3)
         enddo
         rho = 0;
         do d0 = 0,3; do d1 = 0,3
           rho = rho + nu(d0)*nu(d1)*T(d0,d1)
         enddo; enddo
         do d0 = 0,2
           Svec(d0) = 0
           do d1 = 0,3
             Svec(d0) = Svec(d0) - nu(d1)*T(d1,d0)
           enddo
         enddo
         do d0 = 0,2; do d1 = 0,2
          Sten(d0,d1) = T(d0,d1)
         enddo; enddo
         trSten = 0
         do d0 = 0,2; do d1 = 0,2
           trSten = trSten + gu(d0,d1)*chi2*T(d0,d1)
         enddo; enddo
         H(i0,i1,i2) = - 16*(3.14159265358979323846264338327950288d0)*rh
     &o
         do d0 = 0,2
           M(i0,i1,i2,d0) = - 8*(3.14159265358979323846264338327950288d0
     &)*Svec(d0)
         enddo
      enddo
      enddo
      enddo
      return
      end
