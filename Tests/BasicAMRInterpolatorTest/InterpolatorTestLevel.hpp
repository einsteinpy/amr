/* GRChombo
 * Copyright 2012 The GRChombo collaboration.
 * Please refer to LICENSE in GRChombo's root directory.
 */

#ifndef INTERPOLATORTESTLEVEL_HPP_
#define INTERPOLATORTESTLEVEL_HPP_

#include "GRAMRLevel.hpp"

class InterpolatorTestLevel : public GRAMRLevel
{
    friend class DefaultLevelFactory<InterpolatorTestLevel>;
    // Inherit the contructors from GRAMRLevel
    using GRAMRLevel::GRAMRLevel;

    // initialize data
    virtual void initialData() { m_state_new.setVal(42.); }

    virtual void specificEvalRHS(GRLevelData &a_soln, GRLevelData &a_rhs,
                                 const double a_time)
    {
    }

    virtual void computeTaggingCriterion(FArrayBox &tagging_criterion,
                                         const FArrayBox &current_state){};
};

#endif /* INTERPOLATORTESTLEVEL_HPP_ */
