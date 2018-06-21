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

#include <algorithm>
#include <cmath>
#include "interconnect.h"
#include "htf_props.h"

double pi = acos(-1.);
double T_ref_K = 298.150;
int NA_intc = -1;

IntcOutputs::IntcOutputs() {
    heat_loss = 0;
    temp_drop = 0;
    temp_out = 0;
    temp_ave = 0;
    pressure_drop = 0;
    pressure_out = 0;
    pressure_ave = 0;
    internal_energy = 0;
}

double MinorPressureDrop(double vel, double rho, double k) {
    return k * (vel * vel) * rho / 2;
}

double MajorPressureDrop(double vel, double rho, double ff, double l, double d) {
// Darcy Weisbach pressure drop for an incompressible fluid using a Darcy friction factor
    if (d <= 0) throw std::invalid_argument("The diameter must be greater than 0.");

    return ff * (vel * vel) * l * rho / (2 * d);
}

double FrictionFactor(double rel_rough, double Re) {
    if (Re < 2100) {  // laminar flow
        return 64 / Re;
    }
    else if(Re < 4000) {  // transitional flow
        // see Cheng, N.S. 2008 "Formulas for friction factors in transitional regions" for non-iterative solution
        return FricFactor_Iter(rel_rough, Re);
    }
    else {  // turbulent flow
        // Calculates according to Zigrang, Sylvester 1982 for Re = 4e3 to 1e8 and e/D = 4e-5 to 5e-2
        return pow(-2.0*log10(rel_rough / 3.7 - 5.02 / Re * log10(rel_rough / 3.7 - 5.02 / Re *
            log10(rel_rough / 3.7 + 13.0 / Re))), -2);
    }
}

double FricFactor_Iter(double rel_rough, double Re) {
// Uses an iterative method to solve the implicit Colebrook friction factor function.
// Taken from Piping loss model

    double Test, TestOld, X, Xold, Slope;
    double Acc = .01; //0.0001
    int NumTries;

    if (Re < 2750.) {
        return 64. / std::max(Re, 1.0);
    }

    X = 33.33333;  //1. / 0.03
    TestOld = X + 2. * log10(rel_rough / 3.7 + 2.51 * X / Re);
    Xold = X;
    X = 28.5714;  //1. / (0.03 + 0.005)
    NumTries = 0;

    while (NumTries < 21) {
        NumTries++;
        Test = X + 2 * log10(rel_rough / 3.7 + 2.51 * X / Re);
        if (fabs(Test - TestOld) <= Acc) {
            return 1. / (X * X);
        }

        Slope = (Test - TestOld) / (X - Xold);
        Xold = X;
        TestOld = Test;
        X = std::max((Slope * X - Test) / Slope, 1.e-5);
    }

    //call Messages(-1," Could not find friction factor solution",'Warning',0,250) 
    return 0;
}


//double Re(double rho, double vel, double d, double mu) {
//// Reynold's Number
//    return rho * vel * d / mu;
//}


interconnect::interconnect(double k = 0, double d = 0, double l = 0, double rough = 0, double u = 0,
    double mc = 0, IntcType type = IntcType::Fitting)
    :k_(k),
    d_(d),
    l_(l),
    rough_(rough),
    hl_coef_(u),
    mc_(mc),
    Type(type),
    SurfArea_valid_(false),
    SurfArea_(0),
    CrossSecArea_valid_(false),
    CrossSecArea_(0),
    Volume_valid_(false),
    Volume_(0)
{
    if (k_ < 0) throw std::invalid_argument("The minor loss coefficient (K) cannot be less than 0.");
    if (d_ < 0) throw std::invalid_argument("The diameter (D) cannot be less than 0.");
    if (l_ < 0) throw std::invalid_argument("The length (L) cannot be less than 0.");
    if (rough_ < 0) throw std::invalid_argument("The relative roughness cannot be less than 0.");
    if (hl_coef_ < 0) throw std::invalid_argument("The heat loss coefficient (U) cannot be less than 0.");
    if (mc_ < 0) throw std::invalid_argument("The heat capacity cannot be less than 0.");
}

interconnect::~interconnect()
{
}

double interconnect::getK() const {
    return k_;
}

void interconnect::setK(double k) {
    if (k >= 0)
    {
        k_ = k;
    }
    else {
        throw std::invalid_argument("The minor loss coefficient (K) cannot be less than 0.");
    }
}

double interconnect::getD() const {
    return d_;
}

void interconnect::setD(double d) {
    if (d >= 0)
    {
        d_ = d;
        SurfArea_valid_ = false;
    }
    else {
        throw std::invalid_argument("The diameter (D) cannot be less than 0.");
    }
}

double interconnect::getLength() const {
    return l_;
}

void interconnect::setLength(double l) {
    if (l >= 0)
    {
        l_ = l;
        SurfArea_valid_ = false;
    }
    else {
        throw std::invalid_argument("The length (L) cannot be less than 0.");
    }
}

double interconnect::getRelRough() const {
    return rough_;
}

void interconnect::setRelRough(double rough) {
    if (rough >= 0)
    {
        rough_ = rough;
    }
    else {
        throw std::invalid_argument("The relative roughness cannot be less than 0.");
    }
}

double interconnect::getHLCoef() const {
    return hl_coef_;
}

void interconnect::setHLCoef(double u) {
    if (u >= 0)
    {
        hl_coef_ = u;
    }
    else {
        throw std::invalid_argument("The heat loss coefficient (U) cannot be less than 0.");
    }
}

double interconnect::getHeatCap() const {
    return mc_;
}

void interconnect::setHeatCap(double mc) {
    if (mc >= 0)
    {
        mc_ = mc;
    }
    else {
        throw std::invalid_argument("The heat capacity cannot be less than 0.");
    }
}

IntcType interconnect::getType() const {
    return Type;
}

double interconnect::getSurfArea() {
    if (!SurfArea_valid_) { calcSurfArea(); }
    return SurfArea_;
}

void interconnect::calcSurfArea() {
    SurfArea_ = pi * d_ * l_;
    SurfArea_valid_ = true;
}

double interconnect::getCrossSecArea() {
    if (!CrossSecArea_valid_) { calcCrossSecArea(); }
    return CrossSecArea_;
}

void interconnect::calcCrossSecArea() {
    CrossSecArea_ = pi * (d_ * d_) / 4;
    CrossSecArea_valid_ = true;
}

double interconnect::getVolume() {
    if (!Volume_valid_) { calcVolume(); }
    return Volume_;
}

void interconnect::calcVolume() {
    Volume_ = pi * (d_ * d_) / 4. * l_;
    Volume_valid_ = true;
}

double interconnect::HeatLoss(double T_intc, double T_db) {
    double A = getSurfArea();  // fun needed b/c area is not always valid
    return hl_coef_ * A * (T_intc - T_db);
}

double interconnect::TempDrop(HTFProperties *fluidProps, double m_dot, double T_in, double heatLoss) {
    return heatLoss / (m_dot * fluidProps->Cp(T_in));   // positive value means T_out < T_in
}

double interconnect::TempDrop(HTFProperties *fluidProps, double m_dot, double T_in, double T_intc, double T_db) {
    return HeatLoss(T_intc, T_db) / (m_dot * fluidProps->Cp(T_in));   // positive value means T_out < T_in
}

double interconnect::PressureDrop(HTFProperties *fluidProps, double m_dot, double T_htf_ave, double P_htf_ave) {
    double rho = fluidProps->dens(T_htf_ave, P_htf_ave);
    double vel = m_dot / ( rho * getCrossSecArea() );
    double Re, ff;

    switch (Type)
    {
        case IntcType::Fitting:
            return MinorPressureDrop(vel, rho, k_);
        case IntcType::Pipe:
            Re = fluidProps->Re(T_htf_ave, P_htf_ave, vel, d_);
            ff = FrictionFactor(rough_ / d_, Re);
            return MajorPressureDrop(vel, rho, ff, l_, d_);
        case IntcType::Flex_Hose:
            // TODO : Differentiate this pressure drop relation for flex hoses
            Re = fluidProps->Re(T_htf_ave, P_htf_ave, vel, d_);
            ff = FrictionFactor(rough_ / d_, Re);
            return MajorPressureDrop(vel, rho, ff, l_, d_);
        default:
            throw std::invalid_argument("This interconnect type has no pressure drop calculation.");
    }
}

double interconnect::InternalEnergy(HTFProperties *fluidProps, double T_intc, double T_htf_ave, double P_htf_ave) {
    return (getVolume() * fluidProps->dens(T_htf_ave, P_htf_ave) * fluidProps->Cp(T_htf_ave) +
        getHeatCap()) * (T_intc - T_ref_K);
}

IntcOutputs interconnect::State(HTFProperties *fluidProps, double m_dot, double T_in, double T_intc, double T_db, double P_htf_ave) {
    IntcOutputs output;
    output.heat_loss = HeatLoss(T_intc, T_db);
    output.temp_drop = TempDrop(fluidProps, m_dot, T_in, output.heat_loss);
    output.temp_out = T_in - output.temp_drop;
    output.temp_ave = (T_in + output.temp_out) / 2;
    output.pressure_drop = PressureDrop(fluidProps, m_dot, output.temp_ave, P_htf_ave);
    output.pressure_out = P_htf_ave - output.pressure_drop / 2;  // just an approximation to fill an output
    output.pressure_ave = P_htf_ave;
    output.internal_energy = InternalEnergy(fluidProps, T_intc, output.temp_ave, P_htf_ave);

    return output;
}



intc_assy::intc_assy()
    :N_intcs_(0),
    Length_valid_(false),
    l_(0),
    HeatCap_valid_(false),
    mc_(0),
    SurfArea_valid_(false),
    SurfArea_(0),
    Volume_valid_(false),
    Volume_(0)
{
}

intc_assy::intc_assy(HTFProperties *fluidProps, double *k, double *d, double *l, double *rel_rough, double *u, double *mc, double *type, int n_intcs)
    :N_intcs_(0),
    Length_valid_(false),
    l_(0),
    HeatCap_valid_(false),
    mc_(0),
    SurfArea_valid_(false),
    SurfArea_(0),
    Volume_valid_(false),
    Volume_(0)
{
    import_intcs(k, d, l, rel_rough, u, mc, type, n_intcs);
    setFluidProps(fluidProps);
}

intc_assy::~intc_assy()
{
}

void intc_assy::import_intcs(double *k, double *d, double *l, double *rel_rough, double *u, double *mc, double *type, int n_intcs)
{
    std::size_t max_ints = n_intcs;
    std::size_t n_ints = 0;  // double check number of interconnects
    while (k[n_ints] != NA_intc && n_ints < max_ints) { n_ints++; }

    if (!intcs.empty()) { intcs.clear(); }
    intcs.reserve(n_ints);

    interconnect intc;
    for (int i = 0; i < n_ints; i++) {
        if (type[i] < 0 || type[i] >= static_cast<int>(IntcType::FINAL_ENTRY)) {
            throw std::invalid_argument("The interconnect type is out of range at index" + std::to_string(i));
        }
        intc = interconnect(k[i], d[i], l[i], rel_rough[i], u[i], mc[i], static_cast<IntcType>((int)type[i]));
        intcs.push_back(intc);
        N_intcs_++;
        l_ += intc.getLength();
        mc_ += intc.getHeatCap();
        SurfArea_ += intc.getSurfArea();
        Volume_ += intc.getVolume();
    }
    Length_valid_ = true;
    HeatCap_valid_ = true;
    SurfArea_valid_ = true;
    Volume_valid_ = true;
}

void intc_assy::resetValues() {
    intcs.clear();
    N_intcs_ = 0;
    FluidProps_ = NULL;
    Length_valid_ = false;
    l_ = 0;
    HeatCap_valid_ = false;
    mc_ = 0;
    SurfArea_valid_ = false;
    SurfArea_ = 0;
    Volume_valid_ = false;
    Volume_ = 0;
}

void intc_assy::setFluidProps(HTFProperties *fluidProps) {
    FluidProps_ = fluidProps;
}

int intc_assy::getNintcs() {
    return N_intcs_;
}

double intc_assy::getK(std::size_t intc) const
{
    return intcs.at(intc).getK();
}

double intc_assy::getD(std::size_t intc) const
{
    return intcs.at(intc).getD();
}

double intc_assy::getLength() {
    if (!Length_valid_) { calcLength(); }
    return l_;
}

double intc_assy::getLength(std::size_t intc) const
{
    return intcs.at(intc).getLength();
}

void intc_assy::calcLength() {
    l_ = 0;
    for (std::vector<interconnect>::iterator it = intcs.begin(); it < intcs.end(); ++it) {
        l_ += it->getLength();  // interconnect::getLength()
    }
    Length_valid_ = true;
}

double intc_assy::getRelRough(std::size_t intc) const
{
    return intcs.at(intc).getRelRough();
}

double intc_assy::getHLCoef(std::size_t intc) const
{
    return intcs.at(intc).getHLCoef();
}

double intc_assy::getHeatCap() {
    if (!HeatCap_valid_) { calcHeatCap(); }
    return mc_;
}

double intc_assy::getHeatCap(std::size_t intc) const
{
    return intcs.at(intc).getHeatCap();
}

void intc_assy::calcHeatCap() {
    mc_ = 0;
    for (std::vector<interconnect>::iterator it = intcs.begin(); it < intcs.end(); ++it) {
        mc_ += it->getHeatCap();  // interconnect::getHeatCap()
    }
    HeatCap_valid_ = true;
}

IntcType intc_assy::getType(std::size_t intc) const {
    return intcs.at(intc).getType();
}

double intc_assy::getSurfArea() {
    if (!SurfArea_valid_) { calcSurfArea(); }
    return SurfArea_;
}

double intc_assy::getSurfArea(std::size_t intc) {
    return intcs.at(intc).getSurfArea();
}

void intc_assy::calcSurfArea() {
    SurfArea_ = 0;
    for (std::vector<interconnect>::iterator it = intcs.begin(); it < intcs.end(); ++it) {
        SurfArea_ += it->getSurfArea();  // interconnect::getSurfArea()
    }
    SurfArea_ = true;
}

double intc_assy::getCrossSecArea(std::size_t intc) {
    return intcs.at(intc).getCrossSecArea();
}

double intc_assy::getVolume() {
    if (!Volume_valid_) { calcVolume(); }
    return Volume_;
}

double intc_assy::getVolume(std::size_t intc) {
    return intcs.at(intc).getVolume();
}

void intc_assy::calcVolume() {
    Volume_ = 0;
    for (std::vector<interconnect>::iterator it = intcs.begin(); it < intcs.end(); ++it) {
        Volume_ += it->getVolume();  // interconnect::getVolume()
    }
    Volume_valid_ = true;
}

IntcOutputs intc_assy::State(double m_dot, double T_in, double T_db, double P_in) {
    IntcOutputs IntcOutput;
    IntcOutputs AssyOutput;
    double T_out_prev = T_in;
    double P_out_prev = P_in;

    for (std::vector<interconnect>::iterator it = intcs.begin(); it < intcs.end(); ++it) {
        IntcOutput = it->State(FluidProps_, m_dot, T_out_prev, T_out_prev, T_db, P_out_prev);  // interconnect::State()
        AssyOutput.heat_loss += IntcOutput.heat_loss;
        AssyOutput.pressure_drop += IntcOutput.pressure_drop;
        AssyOutput.internal_energy += IntcOutput.internal_energy;

        T_out_prev = IntcOutput.temp_out;
        P_out_prev = P_out_prev - IntcOutput.pressure_drop;
    }
    AssyOutput.temp_drop = T_in - IntcOutput.temp_out;
    AssyOutput.temp_out = IntcOutput.temp_out;
    AssyOutput.temp_ave = (T_in + AssyOutput.temp_out) / 2;
    AssyOutput.pressure_out = P_in - AssyOutput.pressure_drop;
    AssyOutput.pressure_ave = (P_in + AssyOutput.pressure_out) / 2;
    
    return AssyOutput;
}


