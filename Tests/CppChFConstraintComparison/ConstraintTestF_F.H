#ifndef _CONSTRAINTTESTF_F_H_
#define _CONSTRAINTTESTF_F_H_

#include "FORT_PROTO.H"
#include "CH_Timer.H"
#include "REAL.H"

extern "C"
{

#ifndef GUARDGETBSSNCONSTRF 
#define GUARDGETBSSNCONSTRF 
// Prototype for Fortran procedure GETBSSNCONSTRF ...
//
void FORTRAN_NAME( GETBSSNCONSTRF ,getbssnconstrf )(
      CHFp_FRA1(Ham)
      ,CHFp_FRA(Mom)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA(h)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_BOX(box) );

#define FORT_GETBSSNCONSTRF FORTRAN_NAME( inlineGETBSSNCONSTRF, inlineGETBSSNCONSTRF)
#define FORTNT_GETBSSNCONSTRF FORTRAN_NAME( GETBSSNCONSTRF, getbssnconstrf)

inline void FORTRAN_NAME(inlineGETBSSNCONSTRF, inlineGETBSSNCONSTRF)(
      CHFp_FRA1(Ham)
      ,CHFp_FRA(Mom)
      ,CHFp_CONST_FRA1(chi)
      ,CHFp_CONST_FRA(h)
      ,CHFp_CONST_FRA1(K)
      ,CHFp_CONST_FRA(A)
      ,CHFp_CONST_FRA(Gamma1)
      ,CHFp_CONST_REAL(dx)
      ,CHFp_BOX(box) )
{
 CH_TIMELEAF("FORT_GETBSSNCONSTRF");
 FORTRAN_NAME( GETBSSNCONSTRF ,getbssnconstrf )(
      CHFt_FRA1(Ham)
      ,CHFt_FRA(Mom)
      ,CHFt_CONST_FRA1(chi)
      ,CHFt_CONST_FRA(h)
      ,CHFt_CONST_FRA1(K)
      ,CHFt_CONST_FRA(A)
      ,CHFt_CONST_FRA(Gamma1)
      ,CHFt_CONST_REAL(dx)
      ,CHFt_BOX(box) );
}
#endif  // GUARDGETBSSNCONSTRF 

}

#endif
