#ifndef _GRAVWAVDECF_F_H_
#define _GRAVWAVDECF_F_H_

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

#ifndef GUARDGRAVWAVDEC 
#define GUARDGRAVWAVDEC 
// Prototype for Fortran procedure GRAVWAVDEC ...
//
void FORTRAN_NAME( GRAVWAVDEC ,gravwavdec )(
      CHFp_FRA1(weyl1)
      ,CHFp_FRA1(weyl2)
      ,CHFp_CONST_FRA(gamma)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA(beta)
      ,CHFp_CONST_FRA1(phi)
      ,CHFp_CONST_FRA1(phiM)
      ,CHFp_CONST_FRA1(alpha)
      ,CHFp_CONST_FRA1(rho)
      ,CHFp_CONST_REAL(phi0)
      ,CHFp_CONST_REAL(mass)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_CONST_REAL(centerx)
      ,CHFp_CONST_REAL(centery)
      ,CHFp_CONST_REAL(centerz)
      ,CHFp_BOX(box) );

#define FORT_GRAVWAVDEC FORTRAN_NAME( inlineGRAVWAVDEC, inlineGRAVWAVDEC)
#define FORTNT_GRAVWAVDEC FORTRAN_NAME( GRAVWAVDEC, gravwavdec)

inline void FORTRAN_NAME(inlineGRAVWAVDEC, inlineGRAVWAVDEC)(
      CHFp_FRA1(weyl1)
      ,CHFp_FRA1(weyl2)
      ,CHFp_CONST_FRA(gamma)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA(beta)
      ,CHFp_CONST_FRA1(phi)
      ,CHFp_CONST_FRA1(phiM)
      ,CHFp_CONST_FRA1(alpha)
      ,CHFp_CONST_FRA1(rho)
      ,CHFp_CONST_REAL(phi0)
      ,CHFp_CONST_REAL(mass)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_CONST_REAL(centerx)
      ,CHFp_CONST_REAL(centery)
      ,CHFp_CONST_REAL(centerz)
      ,CHFp_BOX(box) )
{
 CH_TIMELEAF("FORT_GRAVWAVDEC");
 FORTRAN_NAME( GRAVWAVDEC ,gravwavdec )(
      CHFt_FRA1(weyl1)
      ,CHFt_FRA1(weyl2)
      ,CHFt_CONST_FRA(gamma)
      ,CHFt_CONST_FRA(Gamma1)
      ,CHFt_CONST_FRA1(chi)
      ,CHFt_CONST_FRA1(K)
      ,CHFt_CONST_FRA(A)
      ,CHFt_CONST_FRA(beta)
      ,CHFt_CONST_FRA1(phi)
      ,CHFt_CONST_FRA1(phiM)
      ,CHFt_CONST_FRA1(alpha)
      ,CHFt_CONST_FRA1(rho)
      ,CHFt_CONST_REAL(phi0)
      ,CHFt_CONST_REAL(mass)
      ,CHFt_CONST_REAL(dx)
      ,CHFt_CONST_REAL(centerx)
      ,CHFt_CONST_REAL(centery)
      ,CHFt_CONST_REAL(centerz)
      ,CHFt_BOX(box) );
}
#endif  // GUARDGRAVWAVDEC 

}

#endif
