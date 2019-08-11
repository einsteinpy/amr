#ifndef _GRBSSNCHOMBOF_F_H_
#define _GRBSSNCHOMBOF_F_H_

#include "FORT_PROTO.H"
#include "CH_Timer.H"
#include "REAL.H"

extern "C"
{

#ifndef GUARDGETV 
#define GUARDGETV 
// Prototype for Fortran procedure GETV ...
//
void FORTRAN_NAME( GETV ,getv )(
      CHFp_REAL(V)
      ,CHFp_CONST_REAL(phi)
      ,CHFp_CONST_REAL(phi0)
      ,CHFp_CONST_REAL(alp)
      ,CHFp_CONST_REAL(s)
      ,CHFp_CONST_REAL(eps) );

#define FORT_GETV FORTRAN_NAME( inlineGETV, inlineGETV)
#define FORTNT_GETV FORTRAN_NAME( GETV, getv)

inline void FORTRAN_NAME(inlineGETV, inlineGETV)(
      CHFp_REAL(V)
      ,CHFp_CONST_REAL(phi)
      ,CHFp_CONST_REAL(phi0)
      ,CHFp_CONST_REAL(alp)
      ,CHFp_CONST_REAL(s)
      ,CHFp_CONST_REAL(eps) )
{
 CH_TIMELEAF("FORT_GETV");
 FORTRAN_NAME( GETV ,getv )(
      CHFt_REAL(V)
      ,CHFt_CONST_REAL(phi)
      ,CHFt_CONST_REAL(phi0)
      ,CHFt_CONST_REAL(alp)
      ,CHFt_CONST_REAL(s)
      ,CHFt_CONST_REAL(eps) );
}
#endif  // GUARDGETV 

#ifndef GUARDGETVPHI 
#define GUARDGETVPHI 
// Prototype for Fortran procedure GETVPHI ...
//
void FORTRAN_NAME( GETVPHI ,getvphi )(
      CHFp_REAL(Vphi)
      ,CHFp_CONST_REAL(phi)
      ,CHFp_CONST_REAL(phi0)
      ,CHFp_CONST_REAL(alp)
      ,CHFp_CONST_REAL(s)
      ,CHFp_CONST_REAL(eps) );

#define FORT_GETVPHI FORTRAN_NAME( inlineGETVPHI, inlineGETVPHI)
#define FORTNT_GETVPHI FORTRAN_NAME( GETVPHI, getvphi)

inline void FORTRAN_NAME(inlineGETVPHI, inlineGETVPHI)(
      CHFp_REAL(Vphi)
      ,CHFp_CONST_REAL(phi)
      ,CHFp_CONST_REAL(phi0)
      ,CHFp_CONST_REAL(alp)
      ,CHFp_CONST_REAL(s)
      ,CHFp_CONST_REAL(eps) )
{
 CH_TIMELEAF("FORT_GETVPHI");
 FORTRAN_NAME( GETVPHI ,getvphi )(
      CHFt_REAL(Vphi)
      ,CHFt_CONST_REAL(phi)
      ,CHFt_CONST_REAL(phi0)
      ,CHFt_CONST_REAL(alp)
      ,CHFt_CONST_REAL(s)
      ,CHFt_CONST_REAL(eps) );
}
#endif  // GUARDGETVPHI 

#ifndef GUARDGETBSSNCRHSF 
#define GUARDGETBSSNCRHSF 
// Prototype for Fortran procedure GETBSSNCRHSF ...
//
void FORTRAN_NAME( GETBSSNCRHSF ,getbssncrhsf )(
      CHFp_FRA1(dchidt)
      ,CHFp_FRA(dgammadt)
      ,CHFp_FRA1(dKdt)
      ,CHFp_FRA(dAdt)
      ,CHFp_FRA1(dThetadt)
      ,CHFp_FRA(dGamma1dt)
      ,CHFp_FRA1(dalphadt)
      ,CHFp_FRA(dbetadt)
      ,CHFp_FRA(dBdt)
      ,CHFp_FRA1(dphidt)
      ,CHFp_FRA1(dphiMdt)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA(gamma)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA1(Theta)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_FRA1(alpha)
      ,CHFp_CONST_FRA(beta)
      ,CHFp_CONST_FRA(B)
      ,CHFp_CONST_FRA1(phi)
      ,CHFp_CONST_FRA1(phiM)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_CONST_REAL(kappa1)
      ,CHFp_CONST_REAL(kappa2)
      ,CHFp_CONST_REAL(kappa3)
      ,CHFp_CONST_REAL(eta)
      ,CHFp_CONST_REAL(mubeta1)
      ,CHFp_CONST_REAL(sigma)
      ,CHFp_CONST_REAL(s)
      ,CHFp_BOX(box) );

#define FORT_GETBSSNCRHSF FORTRAN_NAME( inlineGETBSSNCRHSF, inlineGETBSSNCRHSF)
#define FORTNT_GETBSSNCRHSF FORTRAN_NAME( GETBSSNCRHSF, getbssncrhsf)

inline void FORTRAN_NAME(inlineGETBSSNCRHSF, inlineGETBSSNCRHSF)(
      CHFp_FRA1(dchidt)
      ,CHFp_FRA(dgammadt)
      ,CHFp_FRA1(dKdt)
      ,CHFp_FRA(dAdt)
      ,CHFp_FRA1(dThetadt)
      ,CHFp_FRA(dGamma1dt)
      ,CHFp_FRA1(dalphadt)
      ,CHFp_FRA(dbetadt)
      ,CHFp_FRA(dBdt)
      ,CHFp_FRA1(dphidt)
      ,CHFp_FRA1(dphiMdt)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA(gamma)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA1(Theta)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_FRA1(alpha)
      ,CHFp_CONST_FRA(beta)
      ,CHFp_CONST_FRA(B)
      ,CHFp_CONST_FRA1(phi)
      ,CHFp_CONST_FRA1(phiM)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_CONST_REAL(kappa1)
      ,CHFp_CONST_REAL(kappa2)
      ,CHFp_CONST_REAL(kappa3)
      ,CHFp_CONST_REAL(eta)
      ,CHFp_CONST_REAL(mubeta1)
      ,CHFp_CONST_REAL(sigma)
      ,CHFp_CONST_REAL(s)
      ,CHFp_BOX(box) )
{
 CH_TIMELEAF("FORT_GETBSSNCRHSF");
 FORTRAN_NAME( GETBSSNCRHSF ,getbssncrhsf )(
      CHFt_FRA1(dchidt)
      ,CHFt_FRA(dgammadt)
      ,CHFt_FRA1(dKdt)
      ,CHFt_FRA(dAdt)
      ,CHFt_FRA1(dThetadt)
      ,CHFt_FRA(dGamma1dt)
      ,CHFt_FRA1(dalphadt)
      ,CHFt_FRA(dbetadt)
      ,CHFt_FRA(dBdt)
      ,CHFt_FRA1(dphidt)
      ,CHFt_FRA1(dphiMdt)
      ,CHFt_CONST_FRA1(chi)
      ,CHFt_CONST_FRA(gamma)
      ,CHFt_CONST_FRA1(K)
      ,CHFt_CONST_FRA(A)
      ,CHFt_CONST_FRA1(Theta)
      ,CHFt_CONST_FRA(Gamma1)
      ,CHFt_CONST_FRA1(alpha)
      ,CHFt_CONST_FRA(beta)
      ,CHFt_CONST_FRA(B)
      ,CHFt_CONST_FRA1(phi)
      ,CHFt_CONST_FRA1(phiM)
      ,CHFt_CONST_REAL(dx)
      ,CHFt_CONST_REAL(kappa1)
      ,CHFt_CONST_REAL(kappa2)
      ,CHFt_CONST_REAL(kappa3)
      ,CHFt_CONST_REAL(eta)
      ,CHFt_CONST_REAL(mubeta1)
      ,CHFt_CONST_REAL(sigma)
      ,CHFt_CONST_REAL(s)
      ,CHFt_BOX(box) );
}
#endif  // GUARDGETBSSNCRHSF 

#ifndef GUARDGETBSSNCONSTRF 
#define GUARDGETBSSNCONSTRF 
// Prototype for Fortran procedure GETBSSNCONSTRF ...
//
void FORTRAN_NAME( GETBSSNCONSTRF ,getbssnconstrf )(
      CHFp_FRA1(H)
      ,CHFp_FRA(M)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA(gamma)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_FRA1(alpha)
      ,CHFp_CONST_FRA(beta)
      ,CHFp_CONST_FRA1(phi)
      ,CHFp_CONST_FRA1(phiM)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_CONST_REAL(s)
      ,CHFp_BOX(box) );

#define FORT_GETBSSNCONSTRF FORTRAN_NAME( inlineGETBSSNCONSTRF, inlineGETBSSNCONSTRF)
#define FORTNT_GETBSSNCONSTRF FORTRAN_NAME( GETBSSNCONSTRF, getbssnconstrf)

inline void FORTRAN_NAME(inlineGETBSSNCONSTRF, inlineGETBSSNCONSTRF)(
      CHFp_FRA1(H)
      ,CHFp_FRA(M)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA(gamma)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_FRA1(alpha)
      ,CHFp_CONST_FRA(beta)
      ,CHFp_CONST_FRA1(phi)
      ,CHFp_CONST_FRA1(phiM)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_CONST_REAL(s)
      ,CHFp_BOX(box) )
{
 CH_TIMELEAF("FORT_GETBSSNCONSTRF");
 FORTRAN_NAME( GETBSSNCONSTRF ,getbssnconstrf )(
      CHFt_FRA1(H)
      ,CHFt_FRA(M)
      ,CHFt_CONST_FRA1(chi)
      ,CHFt_CONST_FRA(gamma)
      ,CHFt_CONST_FRA1(K)
      ,CHFt_CONST_FRA(A)
      ,CHFt_CONST_FRA(Gamma1)
      ,CHFt_CONST_FRA1(alpha)
      ,CHFt_CONST_FRA(beta)
      ,CHFt_CONST_FRA1(phi)
      ,CHFt_CONST_FRA1(phiM)
      ,CHFt_CONST_REAL(dx)
      ,CHFt_CONST_REAL(s)
      ,CHFt_BOX(box) );
}
#endif  // GUARDGETBSSNCONSTRF 

}

#endif
