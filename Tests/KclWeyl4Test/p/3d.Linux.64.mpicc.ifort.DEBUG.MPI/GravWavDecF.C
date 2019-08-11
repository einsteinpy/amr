#include "REAL.H"
#include "SPACE.H"
#include "CONSTANTS.H"

#include "CONSTANTS.H"
#define _1  0
#define _2  1
#define _3  2
#define _11 0
#define _12 1
#define _13 2
#define _22 3
#define _23 4
#define _33 5
        subroutine GETV(
     &           V
     &           ,phi
     &           ,phi0
     &           ,alp
     &           ,s
     &           ,eps
     &           )

      implicit none
      REAL_T V
      REAL_T phi
      REAL_T phi0
      REAL_T alp
      REAL_T s
      REAL_T eps
        V = 0
        return
        end
      subroutine GRAVWAVDEC(
     &           weyl1
     &           ,iweyl1lo0,iweyl1lo1,iweyl1lo2
     &           ,iweyl1hi0,iweyl1hi1,iweyl1hi2
     &           ,weyl2
     &           ,iweyl2lo0,iweyl2lo1,iweyl2lo2
     &           ,iweyl2hi0,iweyl2hi1,iweyl2hi2
     &           ,gamma
     &           ,igammalo0,igammalo1,igammalo2
     &           ,igammahi0,igammahi1,igammahi2
     &           ,ngammacomp
     &           ,Gamma1
     &           ,iGamma1lo0,iGamma1lo1,iGamma1lo2
     &           ,iGamma1hi0,iGamma1hi1,iGamma1hi2
     &           ,nGamma1comp
     &           ,chi
     &           ,ichilo0,ichilo1,ichilo2
     &           ,ichihi0,ichihi1,ichihi2
     &           ,K
     &           ,iKlo0,iKlo1,iKlo2
     &           ,iKhi0,iKhi1,iKhi2
     &           ,A
     &           ,iAlo0,iAlo1,iAlo2
     &           ,iAhi0,iAhi1,iAhi2
     &           ,nAcomp
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
     &           ,alpha
     &           ,ialphalo0,ialphalo1,ialphalo2
     &           ,ialphahi0,ialphahi1,ialphahi2
     &           ,rho
     &           ,irholo0,irholo1,irholo2
     &           ,irhohi0,irhohi1,irhohi2
     &           ,phi0
     &           ,mass
     &           ,dx
     &           ,centerx
     &           ,centery
     &           ,centerz
     &           ,iboxlo0,iboxlo1,iboxlo2
     &           ,iboxhi0,iboxhi1,iboxhi2
     &           )

      implicit none
      integer CHF_ID(0:5,0:5)
      data CHF_ID/ 1,0,0,0,0,0 ,0,1,0,0,0,0 ,0,0,1,0,0,0 ,0,0,0,1,0,0 ,0,0,0,0,1,0 ,0,0,0,0,0,1 /


      integer iweyl1lo0,iweyl1lo1,iweyl1lo2
      integer iweyl1hi0,iweyl1hi1,iweyl1hi2
      REAL_T weyl1(
     &           iweyl1lo0:iweyl1hi0,
     &           iweyl1lo1:iweyl1hi1,
     &           iweyl1lo2:iweyl1hi2)
      integer iweyl2lo0,iweyl2lo1,iweyl2lo2
      integer iweyl2hi0,iweyl2hi1,iweyl2hi2
      REAL_T weyl2(
     &           iweyl2lo0:iweyl2hi0,
     &           iweyl2lo1:iweyl2hi1,
     &           iweyl2lo2:iweyl2hi2)
      integer ngammacomp
      integer igammalo0,igammalo1,igammalo2
      integer igammahi0,igammahi1,igammahi2
      REAL_T gamma(
     &           igammalo0:igammahi0,
     &           igammalo1:igammahi1,
     &           igammalo2:igammahi2,
     &           0:ngammacomp-1)
      integer nGamma1comp
      integer iGamma1lo0,iGamma1lo1,iGamma1lo2
      integer iGamma1hi0,iGamma1hi1,iGamma1hi2
      REAL_T Gamma1(
     &           iGamma1lo0:iGamma1hi0,
     &           iGamma1lo1:iGamma1hi1,
     &           iGamma1lo2:iGamma1hi2,
     &           0:nGamma1comp-1)
      integer ichilo0,ichilo1,ichilo2
      integer ichihi0,ichihi1,ichihi2
      REAL_T chi(
     &           ichilo0:ichihi0,
     &           ichilo1:ichihi1,
     &           ichilo2:ichihi2)
      integer iKlo0,iKlo1,iKlo2
      integer iKhi0,iKhi1,iKhi2
      REAL_T K(
     &           iKlo0:iKhi0,
     &           iKlo1:iKhi1,
     &           iKlo2:iKhi2)
      integer nAcomp
      integer iAlo0,iAlo1,iAlo2
      integer iAhi0,iAhi1,iAhi2
      REAL_T A(
     &           iAlo0:iAhi0,
     &           iAlo1:iAhi1,
     &           iAlo2:iAhi2,
     &           0:nAcomp-1)
      integer nbetacomp
      integer ibetalo0,ibetalo1,ibetalo2
      integer ibetahi0,ibetahi1,ibetahi2
      REAL_T beta(
     &           ibetalo0:ibetahi0,
     &           ibetalo1:ibetahi1,
     &           ibetalo2:ibetahi2,
     &           0:nbetacomp-1)
      integer iphilo0,iphilo1,iphilo2
      integer iphihi0,iphihi1,iphihi2
      REAL_T phi(
     &           iphilo0:iphihi0,
     &           iphilo1:iphihi1,
     &           iphilo2:iphihi2)
      integer iphiMlo0,iphiMlo1,iphiMlo2
      integer iphiMhi0,iphiMhi1,iphiMhi2
      REAL_T phiM(
     &           iphiMlo0:iphiMhi0,
     &           iphiMlo1:iphiMhi1,
     &           iphiMlo2:iphiMhi2)
      integer ialphalo0,ialphalo1,ialphalo2
      integer ialphahi0,ialphahi1,ialphahi2
      REAL_T alpha(
     &           ialphalo0:ialphahi0,
     &           ialphalo1:ialphahi1,
     &           ialphalo2:ialphahi2)
      integer irholo0,irholo1,irholo2
      integer irhohi0,irhohi1,irhohi2
      REAL_T rho(
     &           irholo0:irhohi0,
     &           irholo1:irhohi1,
     &           irholo2:irhohi2)
      REAL_T phi0
      REAL_T mass
      REAL_T dx
      REAL_T centerx
      REAL_T centery
      REAL_T centerz
      integer iboxlo0,iboxlo1,iboxlo2
      integer iboxhi0,iboxhi1,iboxhi2
      integer i0,i1,i2, ii0,ii1,ii2, jj0,jj1,jj2
      REAL_T xx, yy, zz
      REAL_T rhoc2, rhoc, r2, rr, costh, cosph, sinth, sinph, costh2, sinth2, cosph2, sinph2
      REAL_T dxinv, dxinv2
      REAL_T g(0:CH_SPACEDIM-1,0:CH_SPACEDIM-1), gu(0:CH_SPACEDIM-1,0:CH_SPACEDIM-1)
      REAL_T detg
      REAL_T d2gammadxdy(0:2,0:2,0:CH_SPACEDIM-1,0:CH_SPACEDIM-1)
      REAL_T dgammadx(0:2,0:2,0:CH_SPACEDIM-1)
      REAL_T chris(0:2,0:2,0:2), chriss(0:2,0:2,0:2), chris1st(0:2,0:2,0:2)
      REAL_T R(0:CH_SPACEDIM-1,0:CH_SPACEDIM-1)
      REAL_T Rchi(0:CH_SPACEDIM-1,0:CH_SPACEDIM-1)
      REAL_T Rfull(0:CH_SPACEDIM-1,0:CH_SPACEDIM-1)
      REAL_T Gamma1d(0:2), dGamma1dx(0:2,0:CH_SPACEDIM-1)
      REAL_T d2chidxdy(0:CH_SPACEDIM-1,0:CH_SPACEDIM-1)
      REAL_T dchidx(0:CH_SPACEDIM-1)
      REAL_T chi2, chi1
      REAL_T Kscalar, Ktsr(0:2,0:2)
      REAL_T dKdx(0:CH_SPACEDIM-1)
      REAL_T CovKtsr(0:2,0:2,0:CH_SPACEDIM-1)
      REAL_T dKtsrdx(0:2,0:2,0:CH_SPACEDIM-1)
      REAL_T dKscalardx(0:CH_SPACEDIM-1)
      REAL_T alp, s
      REAL_T Vphi, Vt
      REAL_T dphidx(0:2)
      REAL_T betaidiphi
      REAL_T dphidt2
      REAL_T lapse
      REAL_T beta2
      REAL_T n(0:3), nu(0:3)
      REAL_T T(0:3,0:3)
      REAL_T Sten(0:2,0:2), trSten, Svec(0:2)
      REAL_T dAdx(0:2,0:2,0:CH_SPACEDIM-1)
      REAL_T levi(0:2,0:2,0:2)
      REAL_T levi44d(0:3,0:3,0:3,0:3)
      REAL_T levi34d(0:3,0:3,0:3)
      REAL_T E(0:2,0:2)
      REAL_T B(0:2,0:2)
      REAL_T er(0:2),ephi(0:2),etheta(0:2)
      REAL_T normr, normphi, normtheta
      REAL_T projrphi, projrtheta, projphitheta
      REAL_T epsilon_tensor(0:2,0:2,0:2)
      REAL_T v1(0:2),v2(0:2),v3(0:2)
      REAL_T omega11,omega12,omega22,omega13,omega23,omega33
      REAL_T x,y,z
      REAL_T null
      integer d0, d1, d2, d3, d4, d5
      integer IDX(0:2, 0:2)
      data IDX / _11, _12, _13, _12, _22, _23, _13, _23, _33  /
      dxinv  = one/dx
      dxinv2 = one/(dx*dx)
      null = 0
      
      do i2 = iboxlo2,iboxhi2
      do i1 = iboxlo1,iboxhi1
      do i0 = iboxlo0,iboxhi0

         chi1 = chi(i0,i1,i2)
         chi2 = chi1**2
         lapse = alpha(i0,i1,i2)
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
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,CH_SPACEDIM-1
           
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
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,CH_SPACEDIM-1; do d3 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           
             jj0 = CHF_ID(d3,0)
             jj1 = CHF_ID(d3,1)
             jj2 = CHF_ID(d3,2)
           if (d2 .eq. d3) cycle
           d2gammadxdy(d0,d1,d2,d3) = dxinv2/144 * (
     &         gamma(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2,IDX(d0,d1))
     &     - 8*gamma(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      ,IDX(d0,d1))
     &     + 8*gamma(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      ,IDX(d0,d1))
     &     -   gamma(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2,IDX(d0,d1))
     &     - 8*gamma(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      ,IDX(d0,d1))
     &     +64*gamma(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            ,IDX(d0,d1))
     &     -64*gamma(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            ,IDX(d0,d1))
     &     + 8*gamma(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      ,IDX(d0,d1))
     &     + 8*gamma(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      ,IDX(d0,d1))
     &     -64*gamma(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            ,IDX(d0,d1))
     &     +64*gamma(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            ,IDX(d0,d1))
     &     - 8*gamma(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      ,IDX(d0,d1))
     &     -   gamma(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2,IDX(d0,d1))
     &     + 8*gamma(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      ,IDX(d0,d1))
     &     - 8*gamma(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      ,IDX(d0,d1))
     &     +   gamma(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2,IDX(d0,d1))
     &       )
         enddo; enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,CH_SPACEDIM-1
           
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
         do d0 = 0,2; do d1 = 0,CH_SPACEDIM-1
           
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
         do d0 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           dchidx(d0) = dxinv/12 * (
     &         chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*chi(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*chi(i0+ii0,i1+ii1,i2+ii2  )
     &     -   chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
           enddo
         do d0 = 0,CH_SPACEDIM-1; do d1 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           
             jj0 = CHF_ID(d1,0)
             jj1 = CHF_ID(d1,1)
             jj2 = CHF_ID(d1,2)
           if (d0 .eq. d1) cycle
           d2chidxdy(d0,d1) = dxinv2/144 * (
     &         chi(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2)
     &     - 8*chi(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      )
     &     + 8*chi(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      )
     &     -   chi(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2)
     &     - 8*chi(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      )
     &     +64*chi(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            )
     &     -64*chi(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            )
     &     + 8*chi(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      )
     &     + 8*chi(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      )
     &     -64*chi(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            )
     &     +64*chi(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            )
     &     - 8*chi(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      )
     &     -   chi(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2)
     &     + 8*chi(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      )
     &     - 8*chi(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      )
     &     +   chi(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2)
     &       )
         enddo; enddo
         do d0 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           d2chidxdy(d0,d0) = dxinv2/12 * (
     &         -chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     + 16*chi(i0-ii0,i1-ii1,i2-ii2  )
     &     - 30*chi(i0,i1,i2        )
     &     + 16*chi(i0+ii0,i1+ii1,i2+ii2  )
     &     -    chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         do d0 = 0,CH_SPACEDIM-1
           
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
         do d0 = 0,CH_SPACEDIM-1
           
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
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,CH_SPACEDIM-1
           
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
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,2
           chris(d0,d1,d2) = 0; chriss(d0,d1,d2) = 0
           do d3 = 0,2
             if (d2 .lt. CH_SPACEDIM) then
               chris(d0,d1,d2) = chris(d0,d1,d2) +
     &           half*gu(d0,d3)*dgammadx(d3,d1,d2)
               chriss(d0,d1,d2) = chriss(d0,d1,d2) +
     &           gu(d0,d3)*(half*dgammadx(d3,d1,d2)*chi1
     &              - g(d3,d1)*dchidx(d2))/chi1
             endif
             if (d1 .lt. CH_SPACEDIM) then
               chris(d0,d1,d2) = chris(d0,d1,d2) +
     &           half*gu(d0,d3)*dgammadx(d3,d2,d1)
               chriss(d0,d1,d2) = chriss(d0,d1,d2) +
     &           gu(d0,d3)*(half*dgammadx(d3,d2,d1)*chi1
     &              - g(d3,d2)*dchidx(d1))/chi1
             endif
             if (d3 .lt. CH_SPACEDIM) then
               chris(d0,d1,d2) = chris(d0,d1,d2) -
     &           half*gu(d0,d3)*dgammadx(d1,d2,d3)
               chriss(d0,d1,d2) = chriss(d0,d1,d2) -
     &           gu(d0,d3)*(half*dgammadx(d1,d2,d3)*chi1
     &              - g(d1,d2)*dchidx(d3))/chi1
             endif
           enddo
         enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,2
           chris1st(d0,d1,d2) = 0
           do d3 = 0,2
             chris1st(d0,d1,d2) = chris1st(d0,d1,d2) + g(d0,d3)*chris(d3,d1,d2)
           enddo
         enddo; enddo; enddo
         do d0 = 0,2
           Gamma1d(d0) = 0
           do d1 = 0,2; do d2 = 0,2
             Gamma1d(d0) = Gamma1d(d0) + chris(d0,d1,d2)*gu(d1,d2)
           enddo; enddo
         enddo
         do d0 = 0,2
           Gamma1d(d0) = Gamma1(i0,i1,i2,d0)
         enddo
         do d0 = 0,2; do d1 = 0,2
           R(d0,d1) = 0
           do d2 = 0,CH_SPACEDIM-1; do d3 = 0,CH_SPACEDIM-1
             R(d0,d1) = R(d0,d1) - half*gu(d2,d3)*d2gammadxdy(d0,d1,d2,d3)
           enddo; enddo
           do d2 = 0,2
             if (d1 .lt. CH_SPACEDIM) then
               R(d0,d1) = R(d0,d1) + half*g(d2,d0)*dGamma1dx(d2,d1)
             endif
             if (d0 .lt. CH_SPACEDIM) then
               R(d0,d1) = R(d0,d1) + half*g(d2,d1)*dGamma1dx(d2,d0)
             endif
           enddo
           do d2 = 0,2
             R(d0,d1) = R(d0,d1)
     &       + half*Gamma1d(d2)*chris1st(d0,d1,d2)
     &       + half*Gamma1d(d2)*chris1st(d1,d0,d2)
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
             Rchi(d0,d1) = Rchi(d0,d1) - g(d0,d1)*gu(d2,d3)*dchidx(d2)*dchidx(d3)
           enddo; enddo
           Rchi(d0,d1) = 2*Rchi(d0,d1)/chi1
           do d2 = 0,CH_SPACEDIM-1; do d3 = 0,CH_SPACEDIM-1
             Rchi(d0,d1) = Rchi(d0,d1)
     &       + (
     &           g(d0,d1)*gu(d2,d3)*d2chidxdy(d2,d3)
     &         )
           enddo; enddo
           do d2 = 0,2; do d3 = 0,2; do d4 = 0,CH_SPACEDIM-1
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
         do d0 = 0,2; do d1 = 0,2
                  Rfull(d0,d1) = R(d0,d1) + Rchi(d0,d1)
               enddo; enddo
         Kscalar = K(i0,i1,i2)
         do d0 = 0,CH_SPACEDIM-1
           dKscalardx(d0) = dKdx(d0)
         enddo
         do d0 = 0,2; do d1 = 0,2
           Ktsr(d0,d1) = A(i0,i1,i2,IDX(d0,d1))/chi2
     &       + third*g(d0,d1)*Kscalar/chi2
           do d2 = 0,CH_SPACEDIM-1
             dKtsrdx(d0,d1,d2) =
     &           dAdx(d0,d1,d2)
     &         - 2*dchidx(d2)*(A(i0,i1,i2,IDX(d0,d1))
     &         + third*g(d0,d1)*K(i0,i1,i2))/chi1
     &         + third*dgammadx(d0,d1,d2)*Kscalar
     &         + third*g(d0,d1)*dKscalardx(d2)
             dKtsrdx(d0,d1,d2) = dKtsrdx(d0,d1,d2)/chi2
           enddo
         enddo; enddo
         alp = 0
         s = 0
         call GETV(Vphi, phi(i0,i1,i2), phi0, alp, s, mass)
         Vt = -phiM(i0,i1,i2)**2 + 2*Vphi
         do d0 = 0,CH_SPACEDIM-1; do d1 = 0,CH_SPACEDIM-1
           Vt = Vt + gu(d0,d1)*chi2*dphidx(d0)*dphidx(d1)
         enddo; enddo
         betaidiphi = 0
         do d0 = 0,CH_SPACEDIM-1
           
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
         nu(3) = 1./lapse
         T(3,3) = dphidt2 + half*(lapse**2 - beta2)*Vt
         do d0 = 0,2; do d1 = 0,2
           T(d0, d1) = -half*g(d0,d1)*Vt/chi2
           if (d0 .lt. CH_SPACEDIM .and. d1 .lt. CH_SPACEDIM) then
             T(d0, d1) = T(d0, d1) + dphidx(d0)*dphidx(d1)
           endif
         enddo; enddo
         do d0 = 0,2
           T(d0,3) = dphidx(d0)*lapse*phiM(i0,i1,i2) + dphidx(d0)*betaidiphi
             do d1 = 0,CH_SPACEDIM-1
               T(d0,3) = T(d0,3) - half*g(d0,d1)*beta(i0,i1,i2,d1)*Vt/chi2
             enddo
           T(3,d0) = T(d0,3)
         enddo
         do d0 = 0,2
           Svec(d0) = 0
           do d1 = 0,3
             Svec(d0) = Svec(d0)
     &         - nu(d1)*T(d1,d0)
           enddo
         enddo
         do d0 = 0,2; do d1 = 0,2
             Sten(d0,d1) = T(d0,d1)
         enddo; enddo
         trSten = 0
         do d0 = 0,2; do d1 = 0,2
           trSten = trSten + gu(d0,d1)*chi2*T(d0,d1)
         enddo; enddo
         do d0=0,3; do d1=0,3; do d2=0,3; do d3=0,3;
          levi44d(d0,d1,d2,d3) = 0
         enddo; enddo; enddo; enddo;
         levi44d(0,1,2,3) = 1
         levi44d(0,1,3,2) = -1
         levi44d(0,3,1,2) = 1
         levi44d(0,3,2,1) = -1
         levi44d(0,2,1,3) = -1
         levi44d(0,2,3,1) = 1
         levi44d(1,0,2,3) = -1
         levi44d(1,2,0,3) = 1
         levi44d(1,2,3,0) = -1
         levi44d(1,3,2,0) = 1
         levi44d(1,3,0,2) = -1
         levi44d(1,0,3,2) = 1
         levi44d(2,0,1,3) = 1
         levi44d(2,0,3,1) = -1
         levi44d(2,3,0,1) = 1
         levi44d(2,3,1,0) = -1
         levi44d(2,1,3,0) = 1
         levi44d(2,1,0,3) = -1
         levi44d(3,0,1,2) = -1
         levi44d(3,1,0,2) = 1
         levi44d(3,1,2,0) = -1
         levi44d(3,2,1,0) = 1
         levi44d(3,2,0,1) = -1
         levi44d(3,0,2,1) = 1
         do d0=0,2; do d1=0,2; do d2=0,2
          levi34d(d0,d1,d2) = 0
         enddo; enddo; enddo
         do d0=0,2; do d1=0,2; do d2=0,2; do d3=0,3
          levi34d(d0,d1,d2) = levi34d(d0,d1,d2) + nu(d3)*levi44d(d0,d1,d2,d3)*lapse/chi2**1.5
         enddo; enddo; enddo; enddo
         do d0=0,2; do d1=0,2; do d2=0,2
          levi(d0,d1,d2) = 0
         enddo; enddo; enddo
         do d0=0,2; do d1=0,2; do d2=0,2; do d3=0,2; do d4=0,2
          levi(d0,d1,d2) = levi(d0,d1,d2) + levi34d(d0,d3,d4)
     &                *gu(d3,d1)*chi2*gu(d4,d2)*chi2
         enddo; enddo; enddo; enddo; enddo
        do d0=0,2; do d1 =0,2;do d2=0,2
          CovKtsr(d0,d1,d2) = dKtsrdx(d0,d1,d2)
        enddo;enddo;enddo
        do d0=0,2; do d1=0,2; do d2=0,2 ; do d3=0,2
          CovKtsr(d0,d1,d2) = CovKtsr(d0,d1,d2) - chriss(d3,d2,d0)*Ktsr(d3,d1)
     &          - chriss(d3,d2,d1)*Ktsr(d0,d3)
        enddo;enddo;enddo; enddo
        do d0=0,2; do d1 =0,2
          E(d0,d1) = 0
          B(d0,d1) = 0
        enddo;enddo
        do d0=0,2; do d1 =0,2; do d2 =0,2; do d3 = 0,2
          B(d0,d1) = B(d0,d1)+levi(d0,d2,d3)*(CovKtsr(d3,d1,d2)
     &                - 4.*Pi*g(d1,d2)/chi2*Svec(d3))
        enddo;enddo;enddo;enddo
        do d0=0,2; do d1 =0,2
          E(d0,d1) = E(d0,d1)+Rfull(d0,d1) + K(i0,i1,i2)
     &               *Ktsr(d0,d1) - 4.*Pi*(Sten(d0,d1)+1./3.*g(d0,d1)/chi2
     &                *(4.*rho(i0,i1,i2)-trSten))
        enddo;enddo
        do d0=0,2; do d1=0,2; do d2=0,2 ; do d3=0,2
          E(d0,d1) = E(d0,d1) - Ktsr(d0,d2)*Ktsr(d3,d1)
     &               *gu(d2,d3)*chi2
        enddo; enddo; enddo; enddo
        xx = dx*(i0+0.5) - centerx
        yy = dx*(i1+0.5) - centery
        zz = dx*(i2+0.5) - centerz
        rhoc2 = xx*xx + yy*yy
        rhoc = sqrt(rhoc2)
        r2 = xx*xx + yy*yy + zz*zz
        rr = sqrt(r2)
        costh = zz/rr
        sinth = rhoc/rr
        costh2 = costh*costh
        sinth2 = sinth*sinth
        cosph = xx/rhoc
        sinph = yy/rhoc
        cosph2 = cosph*cosph
        sinph2 = sinph*sinph
        do d0 = 0,2
          er(d0) = 0
          ephi(d0) = 0
          etheta(d0) = 0
        enddo
        er(0) = cosph*sinth
        er(1) = sinph*sinth
        er(2) =  costh
        ephi(0)= cosph*costh
        ephi(1)= sinph*costh
        ephi(2)= -sinth
        etheta(0)= -sinph
        etheta(1)= cosph
        etheta(2)= 0
        x = (dx*(i0+0.5) - centerx)
        y = (dx*(i1+0.5) - centery)
        z = (dx*(i2+0.5) - centerz)
        do d0=0,2; do d1=0,2; do d2=0,2
           epsilon_tensor(d0,d1,d2) = 0
        enddo; enddo; enddo
        epsilon_tensor(0,1,2) = 1
        epsilon_tensor(1,2,0) = 1
        epsilon_tensor(2,0,1) = 1
        epsilon_tensor(0,2,1) = -1
        epsilon_tensor(1,0,2) = -1
        epsilon_tensor(2,1,0) = -1
        v1(0) = -y
        v1(1) = x
        v1(2) = 0
        v2(0) = x
        v2(1) = y
        v2(2) = z
        v3(0) = 0
        v3(1) = 0
        v3(2) = 0
        do d0=0,2; do d1=0,2; do d2=0,2; do d3 = 0,2
          v3(d3) = v3(d3) + chi1**(-1.)*gu(d3,d0)*epsilon_tensor(d0,d1,d2)*v1(d1)*v2(d2)
        enddo; enddo; enddo; enddo
        omega11 = 0
        do d0=0,2; do d1=0,2
           omega11 = omega11 + v1(d0)*v1(d1)*g(d0,d1)/chi2
        enddo; enddo
        v1 = v1/sqrt(omega11)
        omega12 = 0
        do d0=0,2; do d1=0,2
           omega12 = omega12 + v1(d0)*v2(d1)*g(d0,d1)/chi2
        enddo; enddo
        v2 = v2 - omega12*v1
        omega22 = 0
        do d0=0,2; do d1=0,2
           omega22 = omega22 + v2(d0)*v2(d1)*g(d0,d1)/chi2
        enddo; enddo
        v2 = v2/sqrt(omega22)
        omega13 = 0
        omega23 = 0
        do d0=0,2; do d1=0,2
           omega13 = omega13 + v1(d0)*v3(d1)*g(d0,d1)/chi2
           omega23 = omega23 + v2(d0)*v3(d1)*g(d0,d1)/chi2
        enddo; enddo
        v3 = v3 - (omega13*v1 + omega23*v2)
        omega33 = 0
        do d0=0,2; do d1=0,2
           omega33 = omega33 + v3(d0)*v3(d1)*g(d0,d1)/chi2
        enddo; enddo
        v3 = v3/sqrt(omega33)
        do d0 = 0,2
           ephi(d0) = v1(d0)
           er(d0) = v2(d0)
           etheta(d0) = v3(d0)
        enddo
        weyl1(i0,i1,i2) = 0
        weyl2(i0,i1,i2) = 0
        do d0 = 0,CH_SPACEDIM-1; do d1 = 0,CH_SPACEDIM-1
           weyl1(i0,i1,i2) = weyl1(i0,i1,i2)
     &                            + 0.5*(E(d0,d1)*(etheta(d0)*etheta(d1)
     &                            - ephi(d0)*ephi(d1)) -
     &                            2.*B(d0,d1)*etheta(d0)*ephi(d1))
           weyl2(i0,i1,i2) = weyl2(i0,i1,i2)
     &                            + 0.5*(B(d0,d1)*(- etheta(d0)*etheta(d1)
     &                            + ephi(d0)*ephi(d1)) -
     &                            2.*E(d0,d1)*etheta(d0)*ephi(d1))
        enddo; enddo
      
      enddo
      enddo
      enddo
      return
      end
