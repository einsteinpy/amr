#include "REAL.H"
#include "SPACE.H"
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
      subroutine GETBSSNCRHSF(
     &           dchidt
     &           ,idchidtlo0,idchidtlo1,idchidtlo2
     &           ,idchidthi0,idchidthi1,idchidthi2
     &           ,dhdt
     &           ,idhdtlo0,idhdtlo1,idhdtlo2
     &           ,idhdthi0,idhdthi1,idhdthi2
     &           ,ndhdtcomp
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
     &           ,dlapsedt
     &           ,idlapsedtlo0,idlapsedtlo1,idlapsedtlo2
     &           ,idlapsedthi0,idlapsedthi1,idlapsedthi2
     &           ,dshiftdt
     &           ,idshiftdtlo0,idshiftdtlo1,idshiftdtlo2
     &           ,idshiftdthi0,idshiftdthi1,idshiftdthi2
     &           ,ndshiftdtcomp
     &           ,dBdt
     &           ,idBdtlo0,idBdtlo1,idBdtlo2
     &           ,idBdthi0,idBdthi1,idBdthi2
     &           ,ndBdtcomp
     &           ,chi
     &           ,ichilo0,ichilo1,ichilo2
     &           ,ichihi0,ichihi1,ichihi2
     &           ,h
     &           ,ihlo0,ihlo1,ihlo2
     &           ,ihhi0,ihhi1,ihhi2
     &           ,nhcomp
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
     &           ,lapse
     &           ,ilapselo0,ilapselo1,ilapselo2
     &           ,ilapsehi0,ilapsehi1,ilapsehi2
     &           ,shift
     &           ,ishiftlo0,ishiftlo1,ishiftlo2
     &           ,ishifthi0,ishifthi1,ishifthi2
     &           ,nshiftcomp
     &           ,B
     &           ,iBlo0,iBlo1,iBlo2
     &           ,iBhi0,iBhi1,iBhi2
     &           ,nBcomp
     &           ,dx
     &           ,LapseAdvectionCoeff
     &           ,ShiftAdvectionCoeff
     &           ,ShiftGammaCoeff
     &           ,BetaDriver
     &           ,kappa1
     &           ,kappa2
     &           ,kappa3
     &           ,covariantZ4
     &           ,sigma
     &           ,iboxlo0,iboxlo1,iboxlo2
     &           ,iboxhi0,iboxhi1,iboxhi2
     &           )

      implicit none
      integer CHF_ID(0:5,0:5)
      data CHF_ID/ 1,0,0,0,0,0 ,0,1,0,0,0,0 ,0,0,1,0,0,0 ,0,0,0,1,0,0 ,0,0,0,0,1,0 ,0,0,0,0,0,1 /


      integer idchidtlo0,idchidtlo1,idchidtlo2
      integer idchidthi0,idchidthi1,idchidthi2
      REAL_T dchidt(
     &           idchidtlo0:idchidthi0,
     &           idchidtlo1:idchidthi1,
     &           idchidtlo2:idchidthi2)
      integer ndhdtcomp
      integer idhdtlo0,idhdtlo1,idhdtlo2
      integer idhdthi0,idhdthi1,idhdthi2
      REAL_T dhdt(
     &           idhdtlo0:idhdthi0,
     &           idhdtlo1:idhdthi1,
     &           idhdtlo2:idhdthi2,
     &           0:ndhdtcomp-1)
      integer idKdtlo0,idKdtlo1,idKdtlo2
      integer idKdthi0,idKdthi1,idKdthi2
      REAL_T dKdt(
     &           idKdtlo0:idKdthi0,
     &           idKdtlo1:idKdthi1,
     &           idKdtlo2:idKdthi2)
      integer ndAdtcomp
      integer idAdtlo0,idAdtlo1,idAdtlo2
      integer idAdthi0,idAdthi1,idAdthi2
      REAL_T dAdt(
     &           idAdtlo0:idAdthi0,
     &           idAdtlo1:idAdthi1,
     &           idAdtlo2:idAdthi2,
     &           0:ndAdtcomp-1)
      integer idThetadtlo0,idThetadtlo1,idThetadtlo2
      integer idThetadthi0,idThetadthi1,idThetadthi2
      REAL_T dThetadt(
     &           idThetadtlo0:idThetadthi0,
     &           idThetadtlo1:idThetadthi1,
     &           idThetadtlo2:idThetadthi2)
      integer ndGamma1dtcomp
      integer idGamma1dtlo0,idGamma1dtlo1,idGamma1dtlo2
      integer idGamma1dthi0,idGamma1dthi1,idGamma1dthi2
      REAL_T dGamma1dt(
     &           idGamma1dtlo0:idGamma1dthi0,
     &           idGamma1dtlo1:idGamma1dthi1,
     &           idGamma1dtlo2:idGamma1dthi2,
     &           0:ndGamma1dtcomp-1)
      integer idlapsedtlo0,idlapsedtlo1,idlapsedtlo2
      integer idlapsedthi0,idlapsedthi1,idlapsedthi2
      REAL_T dlapsedt(
     &           idlapsedtlo0:idlapsedthi0,
     &           idlapsedtlo1:idlapsedthi1,
     &           idlapsedtlo2:idlapsedthi2)
      integer ndshiftdtcomp
      integer idshiftdtlo0,idshiftdtlo1,idshiftdtlo2
      integer idshiftdthi0,idshiftdthi1,idshiftdthi2
      REAL_T dshiftdt(
     &           idshiftdtlo0:idshiftdthi0,
     &           idshiftdtlo1:idshiftdthi1,
     &           idshiftdtlo2:idshiftdthi2,
     &           0:ndshiftdtcomp-1)
      integer ndBdtcomp
      integer idBdtlo0,idBdtlo1,idBdtlo2
      integer idBdthi0,idBdthi1,idBdthi2
      REAL_T dBdt(
     &           idBdtlo0:idBdthi0,
     &           idBdtlo1:idBdthi1,
     &           idBdtlo2:idBdthi2,
     &           0:ndBdtcomp-1)
      integer ichilo0,ichilo1,ichilo2
      integer ichihi0,ichihi1,ichihi2
      REAL_T chi(
     &           ichilo0:ichihi0,
     &           ichilo1:ichihi1,
     &           ichilo2:ichihi2)
      integer nhcomp
      integer ihlo0,ihlo1,ihlo2
      integer ihhi0,ihhi1,ihhi2
      REAL_T h(
     &           ihlo0:ihhi0,
     &           ihlo1:ihhi1,
     &           ihlo2:ihhi2,
     &           0:nhcomp-1)
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
      integer iThetalo0,iThetalo1,iThetalo2
      integer iThetahi0,iThetahi1,iThetahi2
      REAL_T Theta(
     &           iThetalo0:iThetahi0,
     &           iThetalo1:iThetahi1,
     &           iThetalo2:iThetahi2)
      integer nGamma1comp
      integer iGamma1lo0,iGamma1lo1,iGamma1lo2
      integer iGamma1hi0,iGamma1hi1,iGamma1hi2
      REAL_T Gamma1(
     &           iGamma1lo0:iGamma1hi0,
     &           iGamma1lo1:iGamma1hi1,
     &           iGamma1lo2:iGamma1hi2,
     &           0:nGamma1comp-1)
      integer ilapselo0,ilapselo1,ilapselo2
      integer ilapsehi0,ilapsehi1,ilapsehi2
      REAL_T lapse(
     &           ilapselo0:ilapsehi0,
     &           ilapselo1:ilapsehi1,
     &           ilapselo2:ilapsehi2)
      integer nshiftcomp
      integer ishiftlo0,ishiftlo1,ishiftlo2
      integer ishifthi0,ishifthi1,ishifthi2
      REAL_T shift(
     &           ishiftlo0:ishifthi0,
     &           ishiftlo1:ishifthi1,
     &           ishiftlo2:ishifthi2,
     &           0:nshiftcomp-1)
      integer nBcomp
      integer iBlo0,iBlo1,iBlo2
      integer iBhi0,iBhi1,iBhi2
      REAL_T B(
     &           iBlo0:iBhi0,
     &           iBlo1:iBhi1,
     &           iBlo2:iBhi2,
     &           0:nBcomp-1)
      REAL_T dx
      REAL_T LapseAdvectionCoeff
      REAL_T ShiftAdvectionCoeff
      REAL_T ShiftGammaCoeff
      REAL_T BetaDriver
      REAL_T kappa1
      REAL_T kappa2
      REAL_T kappa3
      integer covariantZ4
      REAL_T sigma
      integer iboxlo0,iboxlo1,iboxlo2
      integer iboxhi0,iboxhi1,iboxhi2
      integer i0,i1,i2, ii0,ii1,ii2, jj0,jj1,jj2
      REAL_T alph, beta(0:2), bb(0:2), Th, Zvec(0:2)
      REAL_T ch, hh(0:2,0:2), hu(0:2,0:2), trk,
     &       aa(0:2,0:2), atu(0:2,0:2), atm(0:2,0:2), ats(0:2,0:2), tra,
     &       gamma(0:2), dethh, dotgamma(0:2), dottrk
      REAL_T gg(0:2,0:2) , gu(0:2,0:2)
      REAL_T d1_alph(0:2), d1_beta(0:2,0:2), d1_B(0:2,0:2)
      REAL_T d1_ch(0:2), d1_hh(0:2,0:2,0:2), d1_trk(0:2), d1_aa(0:2,0:2,0:2),
     &       d1_gamma(0:2,0:2), d1_Th(0:2)
      REAL_T d2_alph(0:2,0:2), d2_beta(0:2,0:2,0:2)
      REAL_T d2_ch(0:2,0:2), d2_hh(0:2,0:2,0:2,0:2)
      REAL_T d2_phi(0:2,0:2)
      REAL_T ad1_alph, ad1_beta(0:2), ad1_B(0:2)
      REAL_T ad1_ch, ad1_hh(0:2,0:2), ad1_trk, ad1_aa(0:2,0:2),
     &       ad1_gamma(0:2), ad1_Th
      REAL_T cd2_ch(0:2,0:2), cd2_alph(0:2,0:2)
      REAL_T cf1(0:2,0:2,0:2), cf2(0:2,0:2,0:2), cft2(0:2,0:2,0:2),
     &       c_ri(0:2,0:2), c_ll(0:2,0:2)
      REAL_T c_ri_ph(0:2,0:2), c_ri_hh(0:2,0:2), ri_1(0:2,0:2), ri_2(0:2,0:2), ri_3(0:2,0:2),
     &       tr_ll, sq_aa, a2(0:2,0:2), trr,
     &       tf_c_ll(0:2,0:2), tf_c_ri(0:2,0:2), gamcon(0:2)
      REAL_T tr_cd2_ch, tr_dch_dch
      REAL_T divbeta, dbeta(0:2,0:2)
      REAL_T rhs_ch, rhs_hh(0:2,0:2), rhs_trk, rhs_aa(0:2,0:2),
     &       rhs_gamma(0:2), rhs_alph, rhs_beta(0:2), rhs_B(0:2)
      REAL_T rhs_Theta, dotTheta
      REAL_T odx12, odx60, odxsq12 , odxsq144, odxsq180, odxdy3600, odx2
      REAL_T dx12, dxsq12, dxdy144
      REAL_T dxinv, dxinv2
      integer d0, d1, d2, d3, d4, d5 , d6
      integer advec2
      integer IDX(0:2, 0:2)
      data IDX / _11, _12, _13, _12, _22, _23, _13, _23, _33  /
#if CH_SPACEDIM > 3
      call MAYDAY_ERROR()
#else
      odx60     = one/(60 * dx)
      odxsq180  = one / (180*dx**2)
      odxdy3600 = one / (3600*dx**2)
      odx2      = 1 / (2*dx)
      dx12      = 12*dx
      odx12     = one/(12*dx)
      odxsq12   = one / (12*dx**2)
      odxsq144  = one / (144*dx**2)
      dxsq12    = 12*dx**2
      dxdy144   = 144*dx**2
      dxinv = one/dx
      dxinv2 = one/(dx*dx)
      advec2 = 0
      
      do i2 = iboxlo2,iboxhi2
      do i1 = iboxlo1,iboxhi1
      do i0 = iboxlo0,iboxhi0

      ch    = chi(i0,i1,i2)
      trk   = K(i0,i1,i2)
      alph  = lapse(i0,i1,i2)
      Th    = Theta(i0,i1,i2)
      do d0 = 0,2
        gamma(d0) = Gamma1(i0,i1,i2, d0)
        beta(d0)  = shift(i0,i1,i2, d0)
        bb(d0)    = B(i0,i1,i2,d0)
        do d1 = 0,2
          hh(d0,d1) = h(i0,i1,i2, IDX(d0,d1))
          aa(d0,d1) = A(i0,i1,i2, IDX(d0,d1))
        enddo
      enddo
      dethh =        hh(0,0) * hh(1,1) * hh(2,2)
     &         + 2 * hh(0,1) * hh(0,2) * hh(1,2)
     &         -     hh(0,0) * hh(1,2) ** 2
     &         -     hh(1,1) * hh(0,2) ** 2
     &         -     hh(2,2) * hh(0,1) ** 2
      hu(0,0) = (hh(1,1) * hh(2,2) - hh(1,2) ** 2     ) / dethh
      hu(1,1) = (hh(0,0) * hh(2,2) - hh(0,2) ** 2     ) / dethh
      hu(2,2) = (hh(0,0) * hh(1,1) - hh(0,1) ** 2     ) / dethh
      hu(0,1) = (hh(0,2) * hh(1,2) - hh(0,1) * hh(2,2)) / dethh
      hu(0,2) = (hh(0,1) * hh(1,2) - hh(0,2) * hh(1,1)) / dethh
      hu(1,2) = (hh(0,2) * hh(0,1) - hh(1,2) * hh(0,0)) / dethh
      hu(1,0) = hu(0,1)
      hu(2,0) = hu(0,2)
      hu(2,1) = hu(1,2)
      gg = hh / ch
      gu = ch * hu
      do d0 = 0,2
        
          ii0 = CHF_ID(d0,0)
          ii1 = CHF_ID(d0,1)
          ii2 = CHF_ID(d0,2)
        d1_ch(d0) = odx12 * (
     &         chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*chi(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*chi(i0+ii0,i1+ii1,i2+ii2  )
     &     -   chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
        d1_trk(d0) = odx12 * (
     &         K(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*K(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*K(i0+ii0,i1+ii1,i2+ii2  )
     &     -   K(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
        d1_alph(d0) = odx12 * (
     &         lapse(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*lapse(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*lapse(i0+ii0,i1+ii1,i2+ii2  )
     &     -   lapse(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
        d1_Th(d0) = odx12 * (
     &         Theta(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     - 8*Theta(i0-ii0,i1-ii1,i2-ii2  )
     &     + 8*Theta(i0+ii0,i1+ii1,i2+ii2  )
     &     -   Theta(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
      enddo
      do d0 = 0,2; do d1 = 0,CH_SPACEDIM-1
        
            ii0 = CHF_ID(d1,0)
            ii1 = CHF_ID(d1,1)
            ii2 = CHF_ID(d1,2)
            d1_gamma(d0,d1) = odx12 * (
     &         Gamma1(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &     - 8*Gamma1(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &     + 8*Gamma1(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &     -   Gamma1(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &       )
            d1_beta(d0,d1) = odx12 * (
     &         shift(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &     - 8*shift(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &     + 8*shift(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &     -   shift(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &       )
            d1_B(d0,d1) = odx12 * (
     &         B(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &     - 8*B(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &     + 8*B(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &     -   B(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &       )
      enddo; enddo
      do d0 = 0,2; do d1 = 0,2; do d2 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
            d1_hh(d0,d1,d2) = odx12 * (
     &         h(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &     - 8*h(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &     + 8*h(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &     -   h(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &       )
            d1_aa(d0,d1,d2) = odx12 * (
     &         A(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &     - 8*A(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &     + 8*A(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &     -   A(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &       )
      enddo; enddo; enddo
         do d0 = 0,CH_SPACEDIM-1; do d1 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           
             jj0 = CHF_ID(d1,0)
             jj1 = CHF_ID(d1,1)
             jj2 = CHF_ID(d1,2)
           if (d0 .eq. d1) cycle
           d2_ch(d0,d1) = odxsq144 * (
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
           d2_alph(d0,d1) = odxsq144 * (
     &         lapse(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2)
     &     - 8*lapse(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      )
     &     + 8*lapse(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      )
     &     -   lapse(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2)
     &     - 8*lapse(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      )
     &     +64*lapse(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            )
     &     -64*lapse(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            )
     &     + 8*lapse(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      )
     &     + 8*lapse(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      )
     &     -64*lapse(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            )
     &     +64*lapse(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            )
     &     - 8*lapse(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      )
     &     -   lapse(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2)
     &     + 8*lapse(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      )
     &     - 8*lapse(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      )
     &     +   lapse(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2)
     &       )
         enddo; enddo
         do d0 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           d2_ch(d0,d0) = odxsq12 * (
     &         -chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     + 16*chi(i0-ii0,i1-ii1,i2-ii2  )
     &     - 30*chi(i0,i1,i2        )
     &     + 16*chi(i0+ii0,i1+ii1,i2+ii2  )
     &     -    chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
           d2_alph(d0,d0) = odxsq12 * (
     &         -lapse(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &     + 16*lapse(i0-ii0,i1-ii1,i2-ii2  )
     &     - 30*lapse(i0,i1,i2        )
     &     + 16*lapse(i0+ii0,i1+ii1,i2+ii2  )
     &     -    lapse(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &       )
         enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,CH_SPACEDIM-1; do d3 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           
             jj0 = CHF_ID(d3,0)
             jj1 = CHF_ID(d3,1)
             jj2 = CHF_ID(d3,2)
           if (d2 .eq. d3) cycle
           d2_hh(d0,d1,d2,d3) = odxsq144 * (
     &         h(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2,IDX(d0,d1))
     &     - 8*h(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      ,IDX(d0,d1))
     &     + 8*h(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      ,IDX(d0,d1))
     &     -   h(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2,IDX(d0,d1))
     &     - 8*h(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      ,IDX(d0,d1))
     &     +64*h(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            ,IDX(d0,d1))
     &     -64*h(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            ,IDX(d0,d1))
     &     + 8*h(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      ,IDX(d0,d1))
     &     + 8*h(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      ,IDX(d0,d1))
     &     -64*h(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            ,IDX(d0,d1))
     &     +64*h(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            ,IDX(d0,d1))
     &     - 8*h(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      ,IDX(d0,d1))
     &     -   h(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2,IDX(d0,d1))
     &     + 8*h(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      ,IDX(d0,d1))
     &     - 8*h(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      ,IDX(d0,d1))
     &     +   h(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2,IDX(d0,d1))
     &       )
         enddo; enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,2; do d2 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           d2_hh(d0,d1,d2,d2) = odxsq12 * (
     &         -h(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &     + 16*h(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &     - 30*h(i0,i1,i2        ,IDX(d0,d1))
     &     + 16*h(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &     -    h(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &       )
         enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,CH_SPACEDIM-1; do d2 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
           
             jj0 = CHF_ID(d2,0)
             jj1 = CHF_ID(d2,1)
             jj2 = CHF_ID(d2,2)
           if (d1 .eq. d2) cycle
           d2_beta(d0,d1,d2) = odxsq144 * (
     &         shift(i0-2*ii0-2*jj0,i1-2*ii1-2*jj1,i2-2*ii2-2*jj2,d0)
     &     - 8*shift(i0-ii0-2*jj0,i1-ii1-2*jj1,i2-ii2-2*jj2      ,d0)
     &     + 8*shift(i0+ii0-2*jj0,i1+ii1-2*jj1,i2+ii2-2*jj2      ,d0)
     &     -   shift(i0+2*ii0-2*jj0,i1+2*ii1-2*jj1,i2+2*ii2-2*jj2,d0)
     &     - 8*shift(i0-2*ii0-jj0,i1-2*ii1-jj1,i2-2*ii2-jj2      ,d0)
     &     +64*shift(i0-ii0-jj0,i1-ii1-jj1,i2-ii2-jj2            ,d0)
     &     -64*shift(i0+ii0-jj0,i1+ii1-jj1,i2+ii2-jj2            ,d0)
     &     + 8*shift(i0+2*ii0-jj0,i1+2*ii1-jj1,i2+2*ii2-jj2      ,d0)
     &     + 8*shift(i0-2*ii0+jj0,i1-2*ii1+jj1,i2-2*ii2+jj2      ,d0)
     &     -64*shift(i0-ii0+jj0,i1-ii1+jj1,i2-ii2+jj2            ,d0)
     &     +64*shift(i0+ii0+jj0,i1+ii1+jj1,i2+ii2+jj2            ,d0)
     &     - 8*shift(i0+2*ii0+jj0,i1+2*ii1+jj1,i2+2*ii2+jj2      ,d0)
     &     -   shift(i0-2*ii0+2*jj0,i1-2*ii1+2*jj1,i2-2*ii2+2*jj2,d0)
     &     + 8*shift(i0-ii0+2*jj0,i1-ii1+2*jj1,i2-ii2+2*jj2      ,d0)
     &     - 8*shift(i0+ii0+2*jj0,i1+ii1+2*jj1,i2+ii2+2*jj2      ,d0)
     &     +   shift(i0+2*ii0+2*jj0,i1+2*ii1+2*jj1,i2+2*ii2+2*jj2,d0)
     &       )
         enddo; enddo; enddo
         do d0 = 0,2; do d1 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
           d2_beta(d0,d1,d1) = odxsq12 * (
     &         -shift(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &     + 16*shift(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &     - 30*shift(i0,i1,i2        ,d0)
     &     + 16*shift(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &     -    shift(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &       )
         enddo; enddo
         ad1_ch    = 0
         ad1_trk   = 0
         ad1_alph  = 0
         ad1_gamma = 0
         ad1_beta  = 0
         ad1_B     = 0
         ad1_hh    = 0
         ad1_aa    = 0
         ad1_Th    = 0
         do d0 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d0,0)
             ii1 = CHF_ID(d0,1)
             ii2 = CHF_ID(d0,2)
           if ((shift(i0,i1,i2,d0) .lt. 0) .and. (advec2 .eq. 1)) then
           ad1_ch = ad1_ch
     &            + dxinv/2*shift(i0,i1,i2,d0)*(
     &            + chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 4*chi(i0-ii0,i1-ii1,i2-ii2)
     &            + 3*chi(i0,i1,i2      )
     &            )
           ad1_trk = ad1_trk
     &            + dxinv/2*shift(i0,i1,i2,d0)*(
     &            + K(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 4*K(i0-ii0,i1-ii1,i2-ii2)
     &            + 3*K(i0,i1,i2      )
     &            )
           ad1_alph = ad1_alph
     &            + dxinv/2*shift(i0,i1,i2,d0)*(
     &            + lapse(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 4*lapse(i0-ii0,i1-ii1,i2-ii2)
     &            + 3*lapse(i0,i1,i2      )
     &            )
           ad1_Th = ad1_Th
     &            + dxinv/2*shift(i0,i1,i2,d0)*(
     &            + Theta(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 4*Theta(i0-ii0,i1-ii1,i2-ii2)
     &            + 3*Theta(i0,i1,i2      )
     &            )
           elseif ((shift(i0,i1,i2,d0) .lt. 0) .and. (advec2 .eq. 0)) then
           ad1_ch = ad1_ch
     &            + dxinv/12*shift(i0,i1,i2,d0)*(
     &                -chi(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*chi(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*chi(i0,i1,i2        )
     &            +  3*chi(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           ad1_trk = ad1_trk
     &            + dxinv/12*shift(i0,i1,i2,d0)*(
     &                -K(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*K(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*K(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*K(i0,i1,i2        )
     &            +  3*K(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           ad1_alph = ad1_alph
     &            + dxinv/12*shift(i0,i1,i2,d0)*(
     &                -lapse(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*lapse(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*lapse(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*lapse(i0,i1,i2        )
     &            +  3*lapse(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           ad1_Th = ad1_Th
     &            + dxinv/12*shift(i0,i1,i2,d0)*(
     &                -Theta(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &            +  6*Theta(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &            - 18*Theta(i0-ii0,i1-ii1,i2-ii2  )
     &            + 10*Theta(i0,i1,i2        )
     &            +  3*Theta(i0+ii0,i1+ii1,i2+ii2  )
     &            )
           elseif((shift(i0,i1,i2,d0) .ge. 0) .and. (advec2 .eq. 1)) then
           ad1_ch = ad1_ch
     &            + dxinv/2*shift(i0,i1,i2,d0)*(
     &            - chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 4*chi(i0+ii0,i1+ii1,i2+ii2)
     &            - 3*chi(i0,i1,i2      )
     &            )
           ad1_trk = ad1_trk
     &            + dxinv/2*shift(i0,i1,i2,d0)*(
     &            - K(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 4*K(i0+ii0,i1+ii1,i2+ii2)
     &            - 3*K(i0,i1,i2      )
     &            )
           ad1_alph = ad1_alph
     &            + dxinv/2*shift(i0,i1,i2,d0)*(
     &            - lapse(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 4*lapse(i0+ii0,i1+ii1,i2+ii2)
     &            - 3*lapse(i0,i1,i2      )
     &            )
           ad1_Th = ad1_Th
     &            + dxinv/2*shift(i0,i1,i2,d0)*(
     &            - Theta(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 4*Theta(i0+ii0,i1+ii1,i2+ii2)
     &            - 3*Theta(i0,i1,i2      )
     &            )
           else
           ad1_ch = ad1_ch
     &            + dxinv/12*shift(i0,i1,i2,d0)*(
     &                 chi(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*chi(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*chi(i0,i1,i2        )
     &            -  3*chi(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           ad1_trk = ad1_trk
     &            + dxinv/12*shift(i0,i1,i2,d0)*(
     &                 K(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*K(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*K(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*K(i0,i1,i2        )
     &            -  3*K(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           ad1_alph = ad1_alph
     &            + dxinv/12*shift(i0,i1,i2,d0)*(
     &                 lapse(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*lapse(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*lapse(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*lapse(i0,i1,i2        )
     &            -  3*lapse(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           ad1_Th = ad1_Th
     &            + dxinv/12*shift(i0,i1,i2,d0)*(
     &                 Theta(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &            -  6*Theta(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &            + 18*Theta(i0+ii0,i1+ii1,i2+ii2  )
     &            - 10*Theta(i0,i1,i2        )
     &            -  3*Theta(i0-ii0,i1-ii1,i2-ii2  )
     &            )
           endif
         enddo
         do d0 =0,2; do d1 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d1,0)
             ii1 = CHF_ID(d1,1)
             ii2 = CHF_ID(d1,2)
           if ((shift(i0,i1,i2,d1) .lt. 0) .and. (advec2 .eq. 1)) then
           ad1_gamma(d0) = ad1_gamma(d0)
     &            + dxinv/2*shift(i0,i1,i2,d1)*(
     &            + Gamma1(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &            - 4*Gamma1(i0-ii0,i1-ii1,i2-ii2,d0)
     &            + 3*Gamma1(i0,i1,i2      ,d0)
     &            )
           ad1_beta(d0) = ad1_beta(d0)
     &            + dxinv/2*shift(i0,i1,i2,d1)*(
     &            + shift(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &            - 4*shift(i0-ii0,i1-ii1,i2-ii2,d0)
     &            + 3*shift(i0,i1,i2      ,d0)
     &            )
           ad1_B(d0) = ad1_B(d0)
     &            + dxinv/2*shift(i0,i1,i2,d1)*(
     &            + B(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &            - 4*B(i0-ii0,i1-ii1,i2-ii2,d0)
     &            + 3*B(i0,i1,i2      ,d0)
     &            )
           elseif ((shift(i0,i1,i2,d1) .lt. 0) .and. (advec2 .eq. 0)) then
           ad1_gamma(d0) = ad1_gamma(d0)
     &            + dxinv/12*shift(i0,i1,i2,d1)*(
     &                -Gamma1(i0-3*ii0,i1-3*ii1,i2-3*ii2,d0)
     &            +  6*Gamma1(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &            - 18*Gamma1(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &            + 10*Gamma1(i0,i1,i2        ,d0)
     &            +  3*Gamma1(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &            )
           ad1_beta(d0) = ad1_beta(d0)
     &            + dxinv/12*shift(i0,i1,i2,d1)*(
     &                -shift(i0-3*ii0,i1-3*ii1,i2-3*ii2,d0)
     &            +  6*shift(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &            - 18*shift(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &            + 10*shift(i0,i1,i2        ,d0)
     &            +  3*shift(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &            )
           ad1_B(d0) = ad1_B(d0)
     &            + dxinv/12*shift(i0,i1,i2,d1)*(
     &                -B(i0-3*ii0,i1-3*ii1,i2-3*ii2,d0)
     &            +  6*B(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &            - 18*B(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &            + 10*B(i0,i1,i2        ,d0)
     &            +  3*B(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &            )
           elseif ((shift(i0,i1,i2,d1) .ge. 0) .and. (advec2 .eq. 1)) then
           ad1_gamma(d0) = ad1_gamma(d0)
     &            + dxinv/2*shift(i0,i1,i2,d1)*(
     &            - Gamma1(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &            + 4*Gamma1(i0+ii0,i1+ii1,i2+ii2,d0)
     &            - 3*Gamma1(i0,i1,i2      ,d0)
     &            )
           ad1_beta(d0) = ad1_beta(d0)
     &            + dxinv/2*shift(i0,i1,i2,d1)*(
     &            - shift(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &            + 4*shift(i0+ii0,i1+ii1,i2+ii2,d0)
     &            - 3*shift(i0,i1,i2      ,d0)
     &            )
           ad1_B(d0) = ad1_B(d0)
     &            + dxinv/2*shift(i0,i1,i2,d1)*(
     &            - B(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &            + 4*B(i0+ii0,i1+ii1,i2+ii2,d0)
     &            - 3*B(i0,i1,i2      ,d0)
     &            )
           else
           ad1_gamma(d0) = ad1_gamma(d0)
     &            + dxinv/12*shift(i0,i1,i2,d1)*(
     &                 Gamma1(i0+3*ii0,i1+3*ii1,i2+3*ii2,d0)
     &            -  6*Gamma1(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &            + 18*Gamma1(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &            - 10*Gamma1(i0,i1,i2        ,d0)
     &            -  3*Gamma1(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &            )
           ad1_beta(d0) = ad1_beta(d0)
     &            + dxinv/12*shift(i0,i1,i2,d1)*(
     &                 shift(i0+3*ii0,i1+3*ii1,i2+3*ii2,d0)
     &            -  6*shift(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &            + 18*shift(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &            - 10*shift(i0,i1,i2        ,d0)
     &            -  3*shift(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &            )
           ad1_B(d0) = ad1_B(d0)
     &            + dxinv/12*shift(i0,i1,i2,d1)*(
     &                 B(i0+3*ii0,i1+3*ii1,i2+3*ii2,d0)
     &            -  6*B(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &            + 18*B(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &            - 10*B(i0,i1,i2        ,d0)
     &            -  3*B(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &            )
           endif
         enddo; enddo
         do d0 =0,2; do d1 = 0,2; do d2 = 0,CH_SPACEDIM-1
           
             ii0 = CHF_ID(d2,0)
             ii1 = CHF_ID(d2,1)
             ii2 = CHF_ID(d2,2)
           if ((shift(i0,i1,i2,d2) .lt. 0) .and. (advec2 .eq. 1)) then
           ad1_hh(d0,d1) = ad1_hh(d0,d1)
     &            + dxinv/2*shift(i0,i1,i2,d2)*(
     &            + h(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &            - 4*h(i0-ii0,i1-ii1,i2-ii2,IDX(d0,d1))
     &            + 3*h(i0,i1,i2      ,IDX(d0,d1))
     &            )
           ad1_aa(d0,d1) = ad1_aa(d0,d1)
     &            + dxinv/2*shift(i0,i1,i2,d2)*(
     &            + A(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &            - 4*A(i0-ii0,i1-ii1,i2-ii2,IDX(d0,d1))
     &            + 3*A(i0,i1,i2      ,IDX(d0,d1))
     &            )
           elseif ((shift(i0,i1,i2,d2) .lt. 0) .and. (advec2 .eq. 0)) then
           ad1_hh(d0,d1) = ad1_hh(d0,d1)
     &            + dxinv/12*shift(i0,i1,i2,d2)*(
     &                -h(i0-3*ii0,i1-3*ii1,i2-3*ii2,IDX(d0,d1))
     &            +  6*h(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &            - 18*h(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &            + 10*h(i0,i1,i2        ,IDX(d0,d1))
     &            +  3*h(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &            )
           ad1_aa(d0,d1) = ad1_aa(d0,d1)
     &            + dxinv/12*shift(i0,i1,i2,d2)*(
     &                -A(i0-3*ii0,i1-3*ii1,i2-3*ii2,IDX(d0,d1))
     &            +  6*A(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &            - 18*A(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &            + 10*A(i0,i1,i2        ,IDX(d0,d1))
     &            +  3*A(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &            )
           elseif ((shift(i0,i1,i2,d2) .ge. 0) .and. (advec2 .eq. 1)) then
           ad1_hh(d0,d1) = ad1_hh(d0,d1)
     &            + dxinv/2*shift(i0,i1,i2,d2)*(
     &            - h(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &            + 4*h(i0+ii0,i1+ii1,i2+ii2,IDX(d0,d1))
     &            - 3*h(i0,i1,i2      ,IDX(d0,d1))
     &            )
           ad1_aa(d0,d1) = ad1_aa(d0,d1)
     &            + dxinv/2*shift(i0,i1,i2,d2)*(
     &            - A(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &            + 4*A(i0+ii0,i1+ii1,i2+ii2,IDX(d0,d1))
     &            - 3*A(i0,i1,i2      ,IDX(d0,d1))
     &            )
           else
           ad1_hh(d0,d1) = ad1_hh(d0,d1)
     &            + dxinv/12*shift(i0,i1,i2,d2)*(
     &                 h(i0+3*ii0,i1+3*ii1,i2+3*ii2,IDX(d0,d1))
     &            -  6*h(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &            + 18*h(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &            - 10*h(i0,i1,i2        ,IDX(d0,d1))
     &            -  3*h(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &            )
           ad1_aa(d0,d1) = ad1_aa(d0,d1)
     &            + dxinv/12*shift(i0,i1,i2,d2)*(
     &                 A(i0+3*ii0,i1+3*ii1,i2+3*ii2,IDX(d0,d1))
     &            -  6*A(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &            + 18*A(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &            - 10*A(i0,i1,i2        ,IDX(d0,d1))
     &            -  3*A(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &            )
           endif
         enddo; enddo; enddo
      cf1 = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = d1, 2
            cf1(d0,d1,d2) = 0.5d0 * (d1_hh(d0,d1,d2) + d1_hh(d0,d2,d1) - d1_hh(d1,d2,d0))
          enddo
        enddo
      enddo
      cf1(:,1,0) = cf1(:,0,1)
      cf1(:,2,0) = cf1(:,0,2)
      cf1(:,2,1) = cf1(:,1,2)
      cf2  = 0
      cft2 = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            do d3 = 0, 2
              cf2(d0,d1,d2)  = cf2(d0,d1,d2)  + hu(d0,d3) * cf1(d3,d1,d2)
              cft2(d1,d2,d0) = cft2(d1,d2,d0) + cf1(d1,d2,d3) * hu(d3,d0)
            enddo
          enddo
        enddo
      enddo
      cf2(:,1,0) = cf2(:,0,1)
      cf2(:,2,0) = cf2(:,0,2)
      cf2(:,2,1) = cf2(:,1,2)
      gamcon = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            gamcon(d0) = gamcon(d0) + hu(d1,d2) * cf2(d0,d1,d2)
            do d3 = 0, 2
            enddo
          enddo
        enddo
      enddo
      Zvec = ch*(gamma-gamcon)/2.d0
      c_ri_hh = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            c_ri_hh(d0,d1) = c_ri_hh(d0,d1) + hh(d2,d0) * d1_gamma(d2,d1) / 2
     &                                      + hh(d2,d1) * d1_gamma(d2,d0) / 2
     &                                      + gamcon(d2) * (cf1(d0,d1,d2) + cf1(d1,d0,d2)) / 2
            do d3 = 0, 2
              c_ri_hh(d0,d1) = c_ri_hh(d0,d1) - hu(d2,d3) * d2_hh(d0,d1,d2,d3) / 2
     &                                        + cf2(d2,d0,d3) * cft2(d1,d2,d3)
     &                                        + cf2(d2,d1,d3) * cft2(d0,d2,d3)
     &                                        + cf2(d2,d0,d3) * cft2(d2,d1,d3)
            enddo
          enddo
        enddo
      enddo
      c_ri_hh(1,0) = c_ri_hh(0,1)
      c_ri_hh(2,0) = c_ri_hh(0,2)
      c_ri_hh(2,1) = c_ri_hh(1,2)
      cd2_ch   = d2_ch
      cd2_alph = d2_alph
      do d0 = 0, 2
        do d1 = d0, 2
          do d2 = 0, 2
            cd2_ch(d0,d1)   = cd2_ch(d0,d1)   - cf2(d2,d0,d1) * d1_ch(d2)
            cd2_alph(d0,d1) = cd2_alph(d0,d1) - cf2(d2,d0,d1) * d1_alph(d2)
          enddo
        enddo
      enddo
      cd2_ch(1,0)   = cd2_ch(0,1)
      cd2_ch(2,0)   = cd2_ch(0,2)
      cd2_ch(2,1)   = cd2_ch(1,2)
      cd2_alph(1,0) = cd2_alph(0,1)
      cd2_alph(2,0) = cd2_alph(0,2)
      cd2_alph(2,1) = cd2_alph(1,2)
      c_ri_ph = 0
      tr_cd2_ch = 0
      tr_dch_dch = 0
      do d3 = 0, 2
        do d4 = 0, 2
          tr_cd2_ch  = tr_cd2_ch  + hu(d3,d4) * cd2_ch(d3,d4)
          tr_dch_dch = tr_dch_dch + hu(d3,d4) * d1_ch(d3) * d1_ch(d4)
        enddo
      enddo
      do d0 = 0, 2
        do d1 = 0, 2
          c_ri_ph(d0,d1) =   (cd2_ch(d0,d1) + hh(d0,d1) * tr_cd2_ch)
     &                     - (d1_ch(d0) * d1_ch(d1) + 3 * hh(d0,d1) * tr_dch_dch) / (2 * ch)
        enddo
      enddo
      c_ri_ph = c_ri_ph / (2 * ch)
      c_ri_ph(1,0) = c_ri_ph(0,1)
      c_ri_ph(2,0) = c_ri_ph(0,2)
      c_ri_ph(2,1) = c_ri_ph(1,2)
      c_ri = c_ri_ph + c_ri_hh
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            c_ri(d0,d1) = c_ri(d0,d1) + (  gg(d0,d2) * Zvec(d2) * d1_ch(d1) + gg(d1,d2) * Zvec(d2) * d1_ch(d0)
     &                                   - gg(d0,d1) * Zvec(d2) * d1_ch(d2) ) / ch
     &                                + Zvec(d2) * d1_hh(d0,d1,d2) / ch
          enddo
        enddo
      enddo
      c_ri(1,0) = c_ri(0,1)
      c_ri(2,0) = c_ri(0,2)
      c_ri(2,1) = c_ri(1,2)
      atm = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            atm(d0,d1) = atm(d0,d1) + hu(d0,d2) * aa(d2,d1)
          enddo
        enddo
      enddo
      atu = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            atu(d0,d1) = atu(d0,d1) + atm(d0,d2) * hu(d2,d1)
          enddo
        enddo
      enddo
      atu(1,0) = atu(0,1)
      atu(2,0) = atu(0,2)
      atu(2,1) = atu(1,2)
      divbeta = 0
      do d0 = 0, 2
        divbeta = divbeta + d1_beta(d0,d0)
      enddo
      rhs_ch = 2 * ch * ( alph * trk - divbeta ) / 3
      dbeta = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            dbeta(d0,d1) = dbeta(d0,d1) + hh(d0,d2) * d1_beta(d2,d1)
          enddo
        enddo
      enddo
      tra = 0
      do d0 = 0, 2
        do d1 = 0, 2
          tra = tra + hu(d0,d1) * aa(d0,d1)
        enddo
      enddo
      rhs_hh = - 2 * alph * (aa - hh * tra / 3)
      do d0 = 0, 2
        do d1 = 0, 2
          rhs_hh(d0,d1) = rhs_hh(d0,d1) + dbeta(d0,d1) + dbeta(d1,d0)
     &                    - 2 * hh(d0,d1) * divbeta / 3
        enddo
      enddo
      dotgamma = 2 * gamcon * divbeta / 3
     &           + 4 * kappa3 * Zvec * divbeta / (3 * ch)
     &           - 4 * alph * Zvec * trk / (3 * ch)
      if (covariantZ4 .eq. 1) then
        dotgamma = dotgamma - 2 * kappa1 *        Zvec / ch
      else
        dotgamma = dotgamma - 2 * kappa1 * alph * Zvec / ch
      endif
      do d0 = 0, 2
        do d1 = 0, 2
          dotgamma(d0) = dotgamma(d0) - 4 * alph * hu(d0,d1) * d1_trk(d1) / 3
     &                    + atu(d0,d1) * (-3 * alph * d1_ch(d1) / ch - 2 * d1_alph(d1))
     &                    - gamcon(d1) * d1_beta(d0,d1)
     &                    - 2 * kappa3 * Zvec(d1) * d1_beta(d0,d1) / ch
     &                    + 2 * hu(d0,d1) * ( alph * d1_Th(d1) - Th * d1_alph(d1) )
          do d2 = 0, 2
            dotgamma(d0) = dotgamma(d0) + 2 * alph * cf2(d0,d1,d2) * atu(d1,d2)
     &                      + hu(d0,d1) * d2_beta(d2,d2,d1) / 3
     &                      + hu(d1,d2) * d2_beta(d0,d1,d2)
          enddo
        enddo
      enddo
      rhs_gamma = dotgamma
      dotTheta = alph * ( trk * trk / 3 -  trk * Th )
      if (covariantZ4 .eq. 1) then
        dotTheta = dotTheta - kappa1 *        (2 + kappa2) * Th
      else
        dotTheta = dotTheta - kappa1 * alph * (2 + kappa2) * Th
      endif
      do d0 = 0, 2
        dotTheta = dotTheta - Zvec(d0) * d1_alph(d0)
        do d1 = 0, 2
          dotTheta = dotTheta + alph * ( gu(d0,d1) * c_ri(d0,d1) - atm(d0,d1) * atm(d1,d0) ) / 2
        enddo
      enddo
      rhs_Theta = dotTheta
      dottrk = alph * trk * trk / 3
     &         + 2 * dotTheta
      if (covariantZ4 .eq. 1) then
        dottrk = dottrk + kappa1 *        (1-kappa2) * Th
      else
        dottrk = dottrk + kappa1 * alph * (1-kappa2) * Th
      endif
      do d0 = 0, 2
        dottrk = dottrk 
     &                  + ch * gamcon(d0) * d1_alph(d0)
     &                  + 2 * Zvec(d0) * d1_alph(d0)
        do d1 = 0, 2
          dottrk = dottrk 
     &                    - ch * hu(d0,d1) * ( d2_alph(d0,d1) - d1_ch(d0) * d1_alph(d1) / (2 * ch) )
     &                    + alph * atm(d0,d1) * atm(d1,d0)
        enddo
      enddo
      rhs_trk = dottrk
      ats = 0
      do d0 = 0, 2
        do d1 = 0, 2
         ats(d0,d1) = ats(d0,d1) - cd2_alph(d0,d1)
     &               - ( d1_alph(d0) * d1_ch(d1) + d1_alph(d1) * d1_ch(d0) )/(2*ch)
     &               + alph * c_ri(d0,d1)
        enddo
      enddo
      tra = 0
      do d0 = 0, 2
        do d1 = 0, 2
          tra = tra + gu(d0,d1) * ats(d0,d1)
        enddo
      enddo
      dbeta = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            dbeta(d0,d1) = dbeta(d0,d1) + aa(d0,d2) * d1_beta(d2,d1)
          enddo
        enddo
      enddo
      rhs_aa = ch * ( ats - gg * tra / 3 )
     &        + alph * (trk - 2 * Th) * aa
      do d0 = 0, 2
        do d1 = 0, 2
          rhs_aa(d0,d1) = rhs_aa(d0,d1) + dbeta(d0,d1) + dbeta(d1,d0)
     &                    - 2 * aa(d0,d1) * divbeta / 3
          do d2 = 0, 2
            rhs_aa(d0,d1) = rhs_aa(d0,d1) - 2 * alph * aa(d0,d2) * atm(d2,d1)
          enddo
        enddo
      enddo
      rhs_alph = - 2 * alph * (trk - 2 * Th)
      rhs_beta = ShiftGammaCoeff * bb
      rhs_B    = dotgamma - BetaDriver * bb
      rhs_ch    = rhs_ch + ad1_ch
      rhs_hh    = rhs_hh + ad1_hh
      rhs_gamma = rhs_gamma + ad1_gamma
      rhs_Theta = rhs_Theta + ad1_Th
      rhs_trk   = rhs_trk + ad1_trk
      rhs_aa    = rhs_aa + ad1_aa
      rhs_alph  = rhs_alph + LapseAdvectionCoeff * ad1_alph
      rhs_beta  = rhs_beta + ShiftAdvectionCoeff * ad1_beta
      rhs_B = rhs_B + ShiftAdvectionCoeff * ad1_B + (1-ShiftAdvectionCoeff) * ad1_gamma
      dchidt(i0,i1,i2)     = rhs_ch
      dKdt(i0,i1,i2)       = rhs_trk
      dThetadt(i0,i1,i2)   = rhs_Theta
      dlapsedt(i0,i1,i2)   = rhs_alph
      do d0=0,2
        dGamma1dt(i0,i1,i2,d0) = rhs_gamma(d0)
        dshiftdt(i0,i1,i2, d0) = rhs_beta(d0)
        dBdt(i0,i1,i2,d0)      = rhs_B(d0)
        do d1 = 0,2
          dhdt(i0,i1,i2, IDX(d0,d1)) = rhs_hh(d0,d1)
          dAdt(i0,i1,i2, IDX(d0,d1)) = rhs_aa(d0,d1)
        enddo
      enddo
        do d0 = 0,CH_SPACEDIM-1
          
            ii0 = CHF_ID(d0,0)
            ii1 = CHF_ID(d0,1)
            ii2 = CHF_ID(d0,2)
          dchidt(i0,i1,i2) = dchidt(i0,i1,i2)
     &    + sigma/(64*dx) * (
     &         chi(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &    -  6*chi(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &    + 15*chi(i0-ii0,i1-ii1,i2-ii2  )
     &    - 20*chi(i0,i1,i2        )
     &    + 15*chi(i0+ii0,i1+ii1,i2+ii2  )
     &    -  6*chi(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &    +    chi(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &    )
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
          dlapsedt(i0,i1,i2) = dlapsedt(i0,i1,i2)
     &    + sigma/(64*dx) * (
     &         lapse(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &    -  6*lapse(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &    + 15*lapse(i0-ii0,i1-ii1,i2-ii2  )
     &    - 20*lapse(i0,i1,i2        )
     &    + 15*lapse(i0+ii0,i1+ii1,i2+ii2  )
     &    -  6*lapse(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &    +    lapse(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &    )
          dThetadt(i0,i1,i2) = dThetadt(i0,i1,i2)
     &    + sigma/(64*dx) * (
     &         Theta(i0-3*ii0,i1-3*ii1,i2-3*ii2)
     &    -  6*Theta(i0-2*ii0,i1-2*ii1,i2-2*ii2)
     &    + 15*Theta(i0-ii0,i1-ii1,i2-ii2  )
     &    - 20*Theta(i0,i1,i2        )
     &    + 15*Theta(i0+ii0,i1+ii1,i2+ii2  )
     &    -  6*Theta(i0+2*ii0,i1+2*ii1,i2+2*ii2)
     &    +    Theta(i0+3*ii0,i1+3*ii1,i2+3*ii2)
     &    )
        enddo
        do d0 = 0,2; do d1 = 0,CH_SPACEDIM-1
          
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
          dshiftdt(i0,i1,i2,d0) = dshiftdt(i0,i1,i2,d0)
     &    + sigma/(64*dx) * (
     &         shift(i0-3*ii0,i1-3*ii1,i2-3*ii2,d0)
     &    -  6*shift(i0-2*ii0,i1-2*ii1,i2-2*ii2,d0)
     &    + 15*shift(i0-ii0,i1-ii1,i2-ii2  ,d0)
     &    - 20*shift(i0,i1,i2        ,d0)
     &    + 15*shift(i0+ii0,i1+ii1,i2+ii2  ,d0)
     &    -  6*shift(i0+2*ii0,i1+2*ii1,i2+2*ii2,d0)
     &    +    shift(i0+3*ii0,i1+3*ii1,i2+3*ii2,d0)
     &    )
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
        do d0 = 0,2; do d1 = d0,2; do d2 = 0,CH_SPACEDIM-1
          if (d0 .gt. d1) cycle
          
            ii0 = CHF_ID(d2,0)
            ii1 = CHF_ID(d2,1)
            ii2 = CHF_ID(d2,2)
          dhdt(i0,i1,i2,IDX(d0,d1)) = dhdt(i0,i1,i2,IDX(d0,d1))
     &    + sigma/(64*dx) * (
     &         h(i0-3*ii0,i1-3*ii1,i2-3*ii2,IDX(d0,d1))
     &    -  6*h(i0-2*ii0,i1-2*ii1,i2-2*ii2,IDX(d0,d1))
     &    + 15*h(i0-ii0,i1-ii1,i2-ii2  ,IDX(d0,d1))
     &    - 20*h(i0,i1,i2        ,IDX(d0,d1))
     &    + 15*h(i0+ii0,i1+ii1,i2+ii2  ,IDX(d0,d1))
     &    -  6*h(i0+2*ii0,i1+2*ii1,i2+2*ii2,IDX(d0,d1))
     &    +    h(i0+3*ii0,i1+3*ii1,i2+3*ii2,IDX(d0,d1))
     &    )
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
      
      enddo
      enddo
      enddo
#endif
      return
      end
