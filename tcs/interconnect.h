/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __interconnect_
#define __interconnect_

#include <string>
#include <vector>
#include "htf_props.h"


enum class CpntType {
    Fitting,
    Pipe,
    Flex_Hose,
    FINAL_ENTRY,
};

struct IntcOutputs
{
    double heat_loss;
    double temp_drop;
    double temp_out;
    double temp_ave;
    double pressure_drop;
    double pressure_out;
    double pressure_ave;
    double internal_energy;

    IntcOutputs();
};


class intc_cpnt
{
private:
    double k_;                       // minor loss coefficient [-]
    double d_in_;                    // inner diameter [m]
    double l_;                       // length [m]
    double rough_;                   // roughness (inside) [m]
    double hl_coef_;                 // overall heat loss coefficient [W/(m2-K)]
    double mc_;                      // heat capacity w/o htf [J/K]
    double wall_thick_;              // wall thickness [m]
    CpntType Type;
    bool OuterSurfArea_valid_;
    double OuterSurfArea_;
    bool FlowArea_valid_;
    double FlowArea_;                // cross-sectional area for flow [m^2]
    bool FluidVolume_valid_;
    double FluidVolume_;

    void calcOuterSurfArea();
    void calcFlowArea();
    void calcFluidVolume();

public:
    intc_cpnt(double k = 0, double d = 0, double l = 0, double rough = 0, double u = 0,
        double mc = 0, CpntType type = CpntType::Fitting);
    ~intc_cpnt();

    double getK() const;
    void setK(double k);
    double getD() const;
    void setD(double d);
    double getLength() const;
    void setLength(double l);
    double getRelRough() const;
    void setRelRough(double rough);
    double getHLCoef() const;
    void setHLCoef(double u);
    double getHeatCap() const;
    void setHeatCap(double mc);
    double getWallThick() const;
    void setWallThick(double wall_thick);
    CpntType getType() const;
    double getOuterSurfArea();
    double getFlowArea();
    double getFluidVolume();

    double HeatLoss(double T_cpnt, double T_db);
    double TempDrop(HTFProperties *fluidProps, double m_dot, double T_in, double heatLoss);
    double TempDrop(HTFProperties *fluidProps, double m_dot, double T_in, double T_cpnt, double T_db);
    double PressureDrop(HTFProperties *fluidProps, double m_dot, double T_htf_ave, double P_htf_ave);
    double InternalEnergy(HTFProperties *fluidProps, double T_cpnt, double T_htf, double P_htf_ave);
    IntcOutputs State(HTFProperties *fluidProps, double m_dot, double T_in, double T_cpnt, double T_db, double P_htf_ave);
};

class interconnect
{
private:
    std::vector<intc_cpnt> cpnts;
    int N_cpnts_;
    HTFProperties *FluidProps_;
    bool Length_valid_;
    double l_;
    bool HeatCap_valid_;
    double mc_;
    bool OuterSurfArea_valid_;
    double OuterSurfArea_;
    bool FluidVolume_valid_;
    double FluidVolume_;

    void calcLength();
    void calcHeatCap();
    void calcOuterSurfArea();
    void calcFluidVolume();
public:
    interconnect();
    interconnect(HTFProperties *fluidProps, double *k, double *d, double *l, double *rough, double *u, double *mc,
        double *type, int n_cpnts);
    ~interconnect();

    void import_cpnts(double *k, double *d, double *l, double *rough, double *u, double *mc, double *type, int num_cpnts);
    void resetValues();

    void setFluidProps(HTFProperties *fluidProps);
    int getNcpnts();
    double getK(std::size_t cpnt) const;
    double getD(std::size_t cpnt) const;
    double getLength();
    double getLength(std::size_t cpnt) const;
    double getRelRough(std::size_t cpnt) const;
    double getHLCoef(std::size_t cpnt) const;
    double getHeatCap();
    double getHeatCap(std::size_t cpnt) const;
    CpntType getType(std::size_t cpnt) const;
    double getOuterSurfArea();
    double getOuterSurfArea(std::size_t cpnt);
    double getFlowArea(std::size_t cpnt);
    double getFluidVolume();
    double getFluidVolume(std::size_t cpnt);

    IntcOutputs State(double m_dot, double T_in, double T_db, double P_in);
};

#endif
