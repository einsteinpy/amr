#ifndef _GRBSSNCHOMBOF_F_H_
#define _GRBSSNCHOMBOF_F_H_

#include "FORT_PROTO.H"
#include "CH_Timer.H"
#include "REAL.H"

extern "C"
{

#ifndef GUARDGETBSSNCRHSF 
#define GUARDGETBSSNCRHSF 
// Prototype for Fortran procedure GETBSSNCRHSF ...
//
void FORTRAN_NAME( GETBSSNCRHSF ,getbssncrhsf )(
      CHFp_FRA1(dchidt)
      ,CHFp_FRA(dhdt)
      ,CHFp_FRA1(dKdt)
      ,CHFp_FRA(dAdt)
      ,CHFp_FRA1(dThetadt)
      ,CHFp_FRA(dGamma1dt)
      ,CHFp_FRA1(dlapsedt)
      ,CHFp_FRA(dshiftdt)
      ,CHFp_FRA(dBdt)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA(h)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA1(Theta)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_FRA1(lapse)
      ,CHFp_CONST_FRA(shift)
      ,CHFp_CONST_FRA(B)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_CONST_REAL(LapseAdvectionCoeff)
      ,CHFp_CONST_REAL(ShiftAdvectionCoeff)
      ,CHFp_CONST_REAL(ShiftGammaCoeff)
      ,CHFp_CONST_REAL(BetaDriver)
      ,CHFp_CONST_REAL(kappa1)
      ,CHFp_CONST_REAL(kappa2)
      ,CHFp_CONST_REAL(kappa3)
      ,CHFp_CONST_INT(covariantZ4)
      ,CHFp_CONST_REAL(sigma)
      ,CHFp_BOX(box) );

#define FORT_GETBSSNCRHSF FORTRAN_NAME( inlineGETBSSNCRHSF, inlineGETBSSNCRHSF)
#define FORTNT_GETBSSNCRHSF FORTRAN_NAME( GETBSSNCRHSF, getbssncrhsf)

inline void FORTRAN_NAME(inlineGETBSSNCRHSF, inlineGETBSSNCRHSF)(
      CHFp_FRA1(dchidt)
      ,CHFp_FRA(dhdt)
      ,CHFp_FRA1(dKdt)
      ,CHFp_FRA(dAdt)
      ,CHFp_FRA1(dThetadt)
      ,CHFp_FRA(dGamma1dt)
      ,CHFp_FRA1(dlapsedt)
      ,CHFp_FRA(dshiftdt)
      ,CHFp_FRA(dBdt)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA(h)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA1(Theta)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_FRA1(lapse)
      ,CHFp_CONST_FRA(shift)
      ,CHFp_CONST_FRA(B)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_CONST_REAL(LapseAdvectionCoeff)
      ,CHFp_CONST_REAL(ShiftAdvectionCoeff)
      ,CHFp_CONST_REAL(ShiftGammaCoeff)
      ,CHFp_CONST_REAL(BetaDriver)
      ,CHFp_CONST_REAL(kappa1)
      ,CHFp_CONST_REAL(kappa2)
      ,CHFp_CONST_REAL(kappa3)
      ,CHFp_CONST_INT(covariantZ4)
      ,CHFp_CONST_REAL(sigma)
      ,CHFp_BOX(box) )
{
 CH_TIMELEAF("FORT_GETBSSNCRHSF");
 FORTRAN_NAME( GETBSSNCRHSF ,getbssncrhsf )(
      CHFt_FRA1(dchidt)
      ,CHFt_FRA(dhdt)
      ,CHFt_FRA1(dKdt)
      ,CHFt_FRA(dAdt)
      ,CHFt_FRA1(dThetadt)
      ,CHFt_FRA(dGamma1dt)
      ,CHFt_FRA1(dlapsedt)
      ,CHFt_FRA(dshiftdt)
      ,CHFt_FRA(dBdt)
      ,CHFt_CONST_FRA1(chi)
      ,CHFt_CONST_FRA(h)
      ,CHFt_CONST_FRA1(K)
      ,CHFt_CONST_FRA(A)
      ,CHFt_CONST_FRA1(Theta)
      ,CHFt_CONST_FRA(Gamma1)
      ,CHFt_CONST_FRA1(lapse)
      ,CHFt_CONST_FRA(shift)
      ,CHFt_CONST_FRA(B)
      ,CHFt_CONST_REAL(dx)
      ,CHFt_CONST_REAL(LapseAdvectionCoeff)
      ,CHFt_CONST_REAL(ShiftAdvectionCoeff)
      ,CHFt_CONST_REAL(ShiftGammaCoeff)
      ,CHFt_CONST_REAL(BetaDriver)
      ,CHFt_CONST_REAL(kappa1)
      ,CHFt_CONST_REAL(kappa2)
      ,CHFt_CONST_REAL(kappa3)
      ,CHFt_CONST_INT(covariantZ4)
      ,CHFt_CONST_REAL(sigma)
      ,CHFt_BOX(box) );
}
#endif  // GUARDGETBSSNCRHSF 

}

#endif
