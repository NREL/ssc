/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/



#ifndef __LIB_SHARED_INVERTER_H__
#define __LIB_SHARED_INVERTER_H__

#include "lib_sandia.h"
#include "lib_pvinv.h"
#include "lib_ondinv.h"
#include "lib_util.h"
#include <vector>

/**
*
* \class SharedInverter
*
*  A SharedInverter describes a PV inverter that can be optionally hooked up to a DC-connected battery which
*  combines with the PV output through the shared inverter.
*/
class SharedInverter
{
public:

    /// Construct a shared inverter by registering the previously constructed inverter
    SharedInverter(int inverterType, size_t numberOfInverters,
        sandia_inverter_t* sandiaInverter, partload_inverter_t* partloadInverter, ond_inverter* ondInverter, double numberOfInvertersClipping = 0);

    SharedInverter(const SharedInverter& orig);

    /// Setup efficiency vs ambient T curves for temp derating, returns which curve has error if fails, 0 success
    int setTempDerateCurves(std::vector<std::vector<double>> tempDerateCurves);

    std::vector<std::vector<double>> getTempDerateCurves();

    /// Modifies pDc, power ratio of max output to rated output, and loss by calculating derate, using curves interpolated by input V
    void calculateTempDerate(double V, double tempC, double& p_dc_rated, double& ratio, double& loss);

    /// Find the Maximum DC power of the inverter at a given time step to use for clipping comparisons
    double getInverterDCMaxPower(double p_dc_rated);

    /// Given the combined PV plus battery DC power (kW), voltage and ambient T, compute the AC power (kW) for a single inverter with one MPPT input
    void calculateACPower(const double powerDC_kW, const double DCStringVoltage, double tempC);

    void calculateACPower(const double powerDC_kW, const double DCStringVoltage, double tempC, bool clippingEnabled);

    /// Given the combined PV plus battery DC power (kW), voltage and ambient T, compute the AC power (kW) for a single inverter with multiple MPPT inputs
    void calculateACPower(const std::vector<double> powerDC_kW, const std::vector<double> DCStringVoltage, double tempC);

    /// Given a target AC power production, calculate the required DC power if possible, otherwise if eff is too low return kwAC. Does not modify state
    double calculateRequiredDCPower(const double kwAC, const double DCStringV, double tempC);

    /// Return the nominal DC voltage input
    double getInverterDCNominalVoltage();

    /// Return the efficiency at max power (Paco, Vdco);
    double getMaxPowerEfficiency();

    /// Return the nameplate AC capacity
    double getACNameplateCapacitykW();

    util::matrix_t<double> SubhourlyClippingMatrix();

    enum { SANDIA_INVERTER, DATASHEET_INVERTER, PARTLOAD_INVERTER, COEFFICIENT_GENERATOR, OND_INVERTER, NONE };

    const constexpr static double NONE_INVERTER_EFF = 0.96;

public:
    double StringV;
    double Tdry_C;
    // calculated values for the current timestep
    double powerDC_kW;
    double powerAC_kW;
    double powerAC_kW_clipping;
    double efficiencyAC;        // 0-100
    double powerClipLoss_kW;
    double powerConsumptionLoss_kW;
    double powerNightLoss_kW;
    double powerTempLoss_kW;
    double powerLossTotal_kW;
    double dcWiringLoss_ond_kW;
    double acWiringLoss_ond_kW;

protected:

    int m_inverterType;  ///< The inverter type
    size_t m_numInverters;  ///< The number of inverters in the system
    double m_numInvertersClipping;
    double m_nameplateAC_kW; ///< The total nameplate AC capacity for all inverters in kW

    /// Temperate Derating: each curve contains DC voltage and pairs of start-derate temp [C] and slope [efficiency% lost per C]
    bool m_tempEnabled;
    bool m_subhourlyClippingEnabled;
    std::vector<std::vector<double>> m_thermalDerateCurves;		/// ordered by DC V	
    /// Given a temp, find which slope to apply
    void findPointOnCurve(size_t idx, double T, double& startT, double& slope);

    // Memory managed elsewehre
    sandia_inverter_t* m_sandiaInverter;
    partload_inverter_t* m_partloadInverter;
    ond_inverter* m_ondInverter;

private:

    void convertOutputsToKWandScale(double tempLoss, double powerAC_watts);

    // x[0] is dc power input in kW, x[1] is target ac power output in kW
    void solve_kwdc_for_kwac(const double* x, double* f);

    double solver_AC;
};


#endif
