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
      subroutine GETBSSNCONSTRF(
     &           Ham
     &           ,iHamlo0,iHamlo1,iHamlo2
     &           ,iHamhi0,iHamhi1,iHamhi2
     &           ,Mom
     &           ,iMomlo0,iMomlo1,iMomlo2
     &           ,iMomhi0,iMomhi1,iMomhi2
     &           ,nMomcomp
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
     &           ,Gamma1
     &           ,iGamma1lo0,iGamma1lo1,iGamma1lo2
     &           ,iGamma1hi0,iGamma1hi1,iGamma1hi2
     &           ,nGamma1comp
     &           ,dx
     &           ,iboxlo0,iboxlo1,iboxlo2
     &           ,iboxhi0,iboxhi1,iboxhi2
     &           )

      implicit none
      integer CHF_ID(0:5,0:5)
      data CHF_ID/ 1,0,0,0,0,0 ,0,1,0,0,0,0 ,0,0,1,0,0,0 ,0,0,0,1,0,0 ,0,0,0,0,1,0 ,0,0,0,0,0,1 /


      integer iHamlo0,iHamlo1,iHamlo2
      integer iHamhi0,iHamhi1,iHamhi2
      REAL_T Ham(
     &           iHamlo0:iHamhi0,
     &           iHamlo1:iHamhi1,
     &           iHamlo2:iHamhi2)
      integer nMomcomp
      integer iMomlo0,iMomlo1,iMomlo2
      integer iMomhi0,iMomhi1,iMomhi2
      REAL_T Mom(
     &           iMomlo0:iMomhi0,
     &           iMomlo1:iMomhi1,
     &           iMomlo2:iMomhi2,
     &           0:nMomcomp-1)
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
      integer nGamma1comp
      integer iGamma1lo0,iGamma1lo1,iGamma1lo2
      integer iGamma1hi0,iGamma1hi1,iGamma1hi2
      REAL_T Gamma1(
     &           iGamma1lo0:iGamma1hi0,
     &           iGamma1lo1:iGamma1hi1,
     &           iGamma1lo2:iGamma1hi2,
     &           0:nGamma1comp-1)
      REAL_T dx
      integer iboxlo0,iboxlo1,iboxlo2
      integer iboxhi0,iboxhi1,iboxhi2
      REAL_T              ch, hh(0:2,0:2), hu(0:2,0:2), trk, aa(0:2,0:2), gamma(0:2),
     &                    au(0:2,0:2), dethh
      REAL_T              phi, pi
      REAL_T              d1_ch(0:2), d1_hh(0:2,0:2,0:2), d1_trk(0:2), d1_aa(0:2,0:2,0:2),
     &                    d1_gamma(0:2,0:2)
      REAL_T              d2_ch(0:2,0:2), d2_hh(0:2,0:2,0:2,0:2)
      REAL_T              cd2_ch(0:2,0:2), cd1_aa(0:2,0:2,0:2)
      REAL_T              hamC, momC(0:2)
      REAL_T              cf1(0:2,0:2,0:2), cf2(0:2,0:2,0:2), c_ri(0:2,0:2)
      REAL_T              c_ri_ph(0:2,0:2), c_ri_hh(0:2,0:2), ri_1(0:2,0:2), ri_2(0:2,0:2),
     &                    ri_3(0:2,0:2), sq_aa, a2(0:2,0:2), trr, gamcon(0:2)
      REAL_T              tr_cd2_ch, tr_dch_dch
      REAL_T              odx12, odxsq12, odxsq144
      REAL_T              odx60, ody60, odz60, odxsq180, odysq180, odzsq180,
     &                    odxdy3600, odxdz3600, odydz3600
      REAL_T              dx12, dy12, dz12, dxsq12, dysq12, dzsq12,
     &                    dxdy144, dxdz144, dydz144
      integer i0,i1,i2, ii0,ii1,ii2, jj0,jj1,jj2
      integer d0, d1, d2, d3, d4, d5, d6
      integer IDX(0:2, 0:2)
      data IDX / _11, _12, _13, _12, _22, _23, _13, _23, _33  /
#if CH_SPACEDIM > 3
      call MAYDAY_ERROR()
#else
      dx12      = 12*dx
      odx12     = one/(12*dx)
      odxsq12   = one / (12*dx**2)
      odxsq144  = one / (144*dx**2)
      
      do i2 = iboxlo2,iboxhi2
      do i1 = iboxlo1,iboxhi1
      do i0 = iboxlo0,iboxhi0

      ch    = chi(i0,i1,i2)
      trk   = K(i0,i1,i2)
      do d0 = 0,2
        gamma(d0) = Gamma1(i0,i1,i2, d0)
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
      cf2 = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = d1, 2
            do d3 = 0, 2
              cf2(d0,d1,d2) = cf2(d0,d1,d2) + hu(d0,d3) * cf1(d3,d1,d2)
            enddo
          enddo
        enddo
      enddo
      cf2(:,1,0) = cf2(:,0,1)
      cf2(:,2,0) = cf2(:,0,2)
      cf2(:,2,1) = cf2(:,1,2)
      cd2_ch   = d2_ch
      cd1_aa   = d1_aa
      do d0 = 0, 2
        do d1 = d0, 2
          do d3 = 0, 2
            cd2_ch(d0,d1)   = cd2_ch(d0,d1)   - cf2(d3,d0,d1) * d1_ch(d3)
          enddo
          do d2 = 0, 2
            do d3 = 0, 2
                cd1_aa(d0,d1,d2) = cd1_aa(d0,d1,d2) - cf2(d3,d0,d2) * aa(d3,d1)
     &                             - cf2(d3,d1,d2) * aa(d0,d3)
            enddo
          enddo
        enddo
      enddo
      cd2_ch(1,0)   = cd2_ch(0,1)
      cd2_ch(2,0)   = cd2_ch(0,2)
      cd2_ch(2,1)   = cd2_ch(1,2)
      cd1_aa(1,0,:) = cd1_aa(0,1,:)
      cd1_aa(2,0,:) = cd1_aa(0,2,:)
      cd1_aa(2,1,:) = cd1_aa(1,2,:)
      ri_1 = 0
      ri_2 = 0
      ri_3 = 0
      c_ri_ph = 0
      c_ri_hh = 0
      tr_cd2_ch = 0
      tr_dch_dch = 0
      do d3 = 0, 2
        do d4 = 0, 2
          tr_cd2_ch  = tr_cd2_ch  + hu(d3,d4) * cd2_ch(d3,d4)
          tr_dch_dch = tr_dch_dch + hu(d3,d4) * d1_ch(d3) * d1_ch(d4)
        enddo
      enddo
      do d0 = 0, 2
        do d1 = d0, 2
          c_ri_ph(d0,d1) = (cd2_ch(d0,d1) + hh(d0,d1) * tr_cd2_ch
     &                     - (d1_ch(d0) * d1_ch(d1) + 3 * hh(d0,d1) * tr_dch_dch)
     &                     / (2 * ch)) / 2
        enddo
      enddo
      do d0 = 0, 2
        do d1 = d0, 2
          do d2 = 0, 2
            ri_1(d0,d1) = ri_1(d0,d1) + hh(d2,d0) * d1_gamma(d2,d1) / 2
     &                                + hh(d2,d1) * d1_gamma(d2,d0) / 2
     &                                + gamma(d2) * (cf1(d0,d1,d2) + cf1(d1,d0,d2)) / 2
            do d3 = 0, 2
              ri_2(d0,d1) = ri_2(d0,d1) - hu(d2,d3) * d2_hh(d0,d1,d2,d3) / 2
              do d4 = 0, 2
                ri_3(d0,d1) = ri_3(d0,d1) + hu(d2,d3)
     &                                * ( cf2(d4,d2,d0) * cf1(d1,d4,d3)
     &                                + cf2(d4,d2,d1) * (cf1(d0,d4,d3) + cf1(d4,d3,d0)) )
              enddo
            enddo
          enddo
        enddo
      enddo
      c_ri_hh = ch * (ri_1 + ri_2 + ri_3)
      c_ri = c_ri_ph + c_ri_hh
      c_ri(1,0) = c_ri(0,1)
      c_ri(2,0) = c_ri(0,2)
      c_ri(2,1) = c_ri(1,2)
      trr = 0
      do d0 = 0, 2
        do d1 = 0, 2
          trr = trr + hu(d0,d1) * c_ri(d0,d1)
        enddo
      enddo
      sq_aa = 0
      a2    = 0
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            do d3 = 0, 2
              a2(d0,d1) = a2(d0,d1) + hu(d2,d3) * aa(d0,d2) * aa(d1,d3)
            enddo
          enddo
          sq_aa = sq_aa + hu(d0,d1) * a2(d0,d1)
        enddo
      enddo
      hamC = trr + 2 * trk**2 / 3 - sq_aa
      momC = -2 * d1_trk / 3
      do d0 = 0, 2
        do d1 = 0, 2
          do d2 = 0, 2
            momC(d0) = momC(d0) + hu(d1,d2) * (cd1_aa(d1,d0,d2)
     &                - 3 * aa(d1,d0) * d1_ch(d2) / (2 * ch))
          enddo
        enddo
      enddo
      Ham(i0,i1,i2)  = hamC
      do d0 = 0,2
        Mom(i0,i1,i2,d0) = momC(d0)
      enddo
      
      enddo
      enddo
      enddo
#endif
      return
      end
