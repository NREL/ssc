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

#ifndef __wind_obos_h
#define __wind_obos_h

#include <vector>
#include <limits>

using namespace std;

struct wobos //WIND OFFSHORE BOS STRUCTURE TO HOLD ALL INPUTS AND OUTPUTS AND ALLOW MEMBER FUNCTIONS TO OPERATE ON THOSE VALUES
{
	//MAIN INPUTS************************************************************************************************************
	double turbCapEx; //turbine capital cost ($/kW)
	double nTurb;//number of turbines
	double rotorD;//rotor diameter (m)
	double turbR;//turbine rating (MW)
	double hubH;//hub height (m)
	double waterD;// water depth (m)
	double distShore;//distance to shore from install site (km)
	double distPort;//distance to install site from install port (km)
	double distPtoA;//distance from install port to inshore assembly area (km) (spar only)
	double distAtoS;//distance from inshore assembly area to install site (km) (spar Only)
	int substructure; //type of substructure
	int anchor; //anchor type
	int turbInstallMethod; //turbine installation method
	int towerInstallMethod; //tower installation method
	int installStrategy; //installation vessel strategy
	int cableOptimizer; //switch to run the cable optimizer or not
	double moorLines;//number of mooring lines for floating substructures
	double buryDepth;//array and export cable burial depth (m)
	double arrayY;//turbine array spacing between turbines on same row (rotor diameters)
	double arrayX;// turbine array spacing between turbine rows (rotor diameters)
	double substructCont;//substructure install weather contingency
	double turbCont;//turbine install weather contingency
	double elecCont;//turbine install weather contingency
	double interConVolt;//grid interconnect voltage (kV)
	double distInterCon;//distance from onshore substation to grid interconnect (miles)
	double scrapVal;//scrap value of decommissioned components ($)
    double number_install_seasons; //number of vessel mobilization/install seasons

	//DETAILED INPUTS************************************************************************************************************
	//General
	double projLife;//economic lifetime of the project (years)
	double inspectClear;//inspection clearance for substructure and turbine components (m)
	double plantComm; //plant commissioning cost factor
    double procurement_contingency; //contingency factor for procurement costs
    double install_contingency; //contingency factor for installation costs
    double construction_insurance; //insurance during construction factor
    double capital_cost_year_0; //capital cost spent in year 0
    double capital_cost_year_1; //capital cost spent in year 1
    double capital_cost_year_2; //capital cost spent in year 2
    double capital_cost_year_3; //capital cost spent in year 3
    double capital_cost_year_4; //capital cost spent in year 4
    double capital_cost_year_5; //capital cost spent in year 5
    double tax_rate; //effective tax_rate (federal & state)
    double interest_during_construction; //interest rate during construction

	//Substructure & Foundation
	double mpileCR;//monopile pile cost rate ($/tonne)
	double mtransCR;//monopile transition piece cost rate ($/tonne)
	double mpileD;//monopile pile diameter (m)
	double mpileL;//monopile length (m)
	double jlatticeCR;//jacket lattice cost rate ($/tonne)
	double jtransCR;//jacket transition piece cost rate ($/tonne)
	double jpileCR;//jacket pile cost rate ($/tonne)
	double jlatticeA;//jacket lattice footprint area
	double jpileL;//jacket pile length
	double jpileD;//jacket pile diameter
	double spStifColCR;//spar stiffened column cost rate ($/tonne)
	double spTapColCR;//spar tapered column cost rate ($/tonne)
	double ballCR;//ballast cost rate ($/tonne)
	double deaFixLeng;//drag embedment anchor fixed mooring line length
	double ssStifColCR;//semisubmersible stiffened column cost rate ($/tonne)
	double ssTrussCR;// semisubmersible truss cost rate ($/tonne)
	double ssHeaveCR;//semisubmersible heave plate cost rate ($/tonne)
	double sSteelCR;//secondary steel cost rate ($/tonne)
	double moorDia;//mooring line diameter
	double moorCR;//mooring line cost rate ($/m)
	double mpEmbedL;//monopile embedment length (m)
    double scourMat;
	
	//Electrical Infrastructure
	double pwrFac;//power factor to estimate losses
	double buryFac;//cable burial factor
	double arrVoltage;//array cable voltage (kV)
	double arrCab1Size;//diameter in square millimeters of array cable 1
	double arrCab1Mass;//mass of array cable 1 (kg/m)
	double cab1CurrRating;//current rating of array cable 1 (amps)
	double cab1CR;//cost rate of array cable 1 ($/m)
	double cab1TurbInterCR;//array cable size 1 turbine interface cost rate ($/interface)
	double arrCab2Size;//diameter in square millimeters of array cable 2
	double arrCab2Mass;//mass of array cable 2 (kg/m)
	double cab2CurrRating;//current rating of array cable 2 (amps)
	double cab2CR;//cost rate of array cable 2 ($/m)
	double cab2TurbInterCR;//array cable size 2 turbine interface cost rate ($/interface)
	double cab2SubsInterCR;//array cable size 2 substation interface cost rate ($/interface)
	double catLengFac;//free hanging or catenary cable length factor
	double exCabFac;// excess cable factor
	double subsTopFab;//substation topside fabrication cost ($/tonne)
	double subsTopDes;//substation topside design cost ($)
	double topAssemblyFac;//land based substation topside assembly factor
	double subsJackCR;//substation jacket substructure cost rate ($/tonne)
	double subsPileCR;//substation jacket pile cost rate ($/tonne)
	double dynCabFac;//dynamic/free hanging cable cost premium
	double shuntCR;//shunt reactor cost rate ($/MVA)
	double highVoltSG;//high voltage switchgear cost ($)
	double medVoltSG;//medium voltage switchgear cost ($)
	double backUpGen;//back up generator cost ($)
	double workSpace;//substation workshop and accommodations cost ($)
	double otherAncillary;//substation other ancillary costs ($)
	double mptCR;//main power transformer cost rate ($/MVA)
	double expVoltage;//export cable voltage (kV)
	double expCabSize;//diameter in square millimeters of the export cable
	double expCabMass;//mass of the export cable (kg/m)
	double expCabCR;//cost rate of the export cable ($/m)
	double expCurrRating;//export cable rating (amps)
	double expSubsInterCR;//cost rate of export cable substation interfaces ($/interface)
	
	//Assembly & Installation
	double moorTimeFac;//mooring installation timing factor (hrs/m)
	double moorLoadout;//mooring system loadout timing (hrs)
	double moorSurvey;//mooring system anchor position survey timing (hrs)
	double prepAA;//prep inshore assembly area timing (hrs)
	double prepSpar;//prep spare for tow out to assembly area timing (hrs)
	double upendSpar;//upend and ballast the spar timing (hrs)
	double prepSemi;//prep semisubmersible for turbine install timing (hrs)
	double turbFasten;//fasten turbine for transport timing (hrs)
	double boltTower;// bolt tower to substructure timing (hrs)
	double boltNacelle1;//bolt nacelle to tower timing individual components method (hrs)
	double boltNacelle2;//bolt nacelle to tower timing bunny ears method (hrs)
	double boltNacelle3;//bolt nacelle to tower timing assembled rotor method (hrs)
	double boltBlade1;//bolt blade to rotor timing individual components method (hrs)
	double boltBlade2;//bolt blade to rotor timing bunny ears method (hrs)
	double boltRotor;//bolt rotor to nacelle timing assembled rotor method (hrs)
	double vesselPosTurb;//vessel positioning timing turbine install (hrs)
	double vesselPosJack;//vessel positioning timing jacket install (hrs)
	double vesselPosMono;//vessel positioning timing monopile install (hrs)
	double subsVessPos;//vessel positioning timing offshore substation install (hrs)
	double monoFasten;//fasten monopile for transport timing (hrs)
	double jackFasten;//fasten jacket for transport timing (hrs)
	double prepGripperMono;//prepare pile gripper and upender timing monopile install (hrs)
	double prepGripperJack;//prepare pile gripper and upender timing iacket install (hrs)
	double placePiles;//lift and place jacket piles timing (hrs)
	double prepHamMono;//prepare pile hammer timing monopile install (hrs)
	double removeHamMono;//remove hammer timing monopile install (hrs)
	double prepHamJack;//prepare pile hammer timing iacket install (hrs)
	double removeHamJack;//remove hammer timing iacket install (hrs)
	double placeJack;//place  jacket timing (hrs)
	double levJack;//level jacket timing (hrs)
	double placeTemplate;//place jacket template timing (hrs)
	double hamRate;//pile hammer rate (m/hr)
	double placeMP;//place monopile pile timing (hrs)
	double instScour;//install scour protection (hrs)
	double placeTP;//place transition piece on monopile timing (hrs)
	double groutTP;//grout transition piece (hrs)
	double tpCover;//install transition piece cover timing (hrs)
	double prepTow;//prep floating substructure for towing timing (hrs)
	double spMoorCon;//connect spar to mooring system timing (hrs)
	double ssMoorCon;//connect semisubmersible to mooring system (hrs)
	double spMoorCheck;//check mooring connections to spar timing (hrs)
	double ssMoorCheck;//check mooring connections to semisubmersible timing (hrs)
	double ssBall;//ballast semisubmersible timing (hrs)
	double surfLayRate;//electrical cable surface lay rate (m/hr)
	double cabPullIn;//array cable pull in to interfaces timing (hrs)
	double cabTerm;//cable termination and testing timing (hrs)
	double cabLoadout;//array cable loadout timing (hrs)
	double buryRate;//cable bury rate (m/hr)
	double subsPullIn;//cable pull in to substation timing (hrs)
	double shorePullIn;//cable pull in to shore timing (hrs)
	double landConstruct;//land construction of required onshore electrical systems timing (days)
	double expCabLoad;//export cable loadout timing (hrs)
	double subsLoad;//substation loadout timing (hrs)
	double placeTop;//lift and place substation topside timing (hrs)
	double pileSpreadDR;//piling equipment spread day rate ($/day)
	double pileSpreadMob;//piling equipment spread mobilization/demobilization cost ($)
	double groutSpreadDR;//grouting equipment spread day rate ($/day)
	double groutSpreadMob;//grouting equipment spread mobilization/demobilization cost ($)
	double seaSpreadDR;//suction pile anchor vessel and equipment spread day rate ($/day)
	double seaSpreadMob;//suction pile anchor vessel and equipment spread mobilization/demobilization cost ($)
	double compRacks;//component racks cost ($)
	double cabSurveyCR;//cost rate of surveying and verifying electrical cable installation ($/)
    double cabDrillDist;//horizontal drilling distance for cable landfall (m)
    double cabDrillCR;//horizontal drilling cost rate ($/m)
    double mpvRentalDR;//MPV rental day rate ($/day)
    double diveTeamDR;//cable landfall dive team day rate ($/day)
    double winchDR;//Cable winch day rate
    double civilWork;//civil construction work cost ($)
    double elecWork;//electrical work cost ($)
	
	//Port & Staging
	double nCrane600;
	double nCrane1000;
	double crane600DR;//600 tonne capacity crawler crane day rate ($/day)
	double crane1000DR;//1000 tonne capacity crawler crane day rate ($/day)
	double craneMobDemob;//crane mobilization and demobilization cost ($)
	double entranceExitRate;//port entrance and exit cost ($/m^2/occurrence)
	double dockRate;//port docking cost ($/day)
	double wharfRate;//port wharf loading and unloading cost ($/tonne)
	double laydownCR;//port laydown and storage cost ($/m/day)
	
	//Engineering & Management
	double estEnMFac;//estimated engineering and management cost factor
	
	//Development
	double preFEEDStudy;//pre-fornt end engineering design (FEED) study cost ($)
	double feedStudy;// FEED study cost ($)
	double stateLease;//state leasing cost ($)
	double outConShelfLease;//outer continental shelf lease cost ($)
	double saPlan;//site assessment plan cost ($)
	double conOpPlan;//construction operations plan cost ($)
	double nepaEisMet;//national environmental protection agency (NEPA) environmental impact (EIS) meteorological (met) tower study cost ($)
	double physResStudyMet;//physical resource met tower study cost ($)
	double bioResStudyMet;//biological resource met tower study ($)
	double socEconStudyMet;//socioeconomic met tower study cost ($)
	double navStudyMet;//navigation met tower study ($)
	double nepaEisProj;// NEPA EIS project site study cost ($)
	double physResStudyProj;//physical resource project site study cost ($)
	double bioResStudyProj;//biological resource project site study cost ($)
	double socEconStudyProj;//socioeconomic project site study cost ($)
	double navStudyProj;//navigation project site study cost ($)
	double coastZoneManAct;//coastal zone management act compliance cost ($)
	double rivsnHarbsAct;//rivers & harbors act section 10 compliance cost ($)
	double cleanWatAct402;//clean water act section 402 compliance cost ($)
	double cleanWatAct404;//clean water act section 404 compliance cost ($)
	double faaPlan;//federal aviation administration (FAA) plans and mitigation cost ($)
	double endSpecAct;//endangered species act compliance cost ($)
	double marMamProtAct;//marine mammal protection act compliance cost ($)
	double migBirdAct;//migratory bird act compliance ($)
	double natHisPresAct;//national historic preservation act compliance cost ($)
	double addLocPerm;//additional local and state permissions and compliance cost ($)
	double metTowCR;//meteorological tower fabrication, design, and install cost rate ($/MW)
	double decomDiscRate;//decommissioning expense discount rate

	//VECTORS TO HOLD VARIABLES************************************************************************************************************
    //cost vectors
	vector< vector<double> > turbCostsByVessel;
	vector<vector<double> > subCostsByVessel;
	vector<vector<double> > elecCostsByVessel;
	vector<vector<double> > mobDemobCostByVessel;
    //cable vectors
    vector<vector<double> > arrayVolt;
    vector<vector<vector<double> > > arrCables;
    vector<vector<double> > expCabVolt;
    vector<vector<vector<double> > > expCables;
    //vessel vectors
    vector<vector<double> > elecTugs;
    vector<double> turbInstVessel;
    vector<double> turbFeederBarge;
    vector<vector<double> >turbSupportVessels;

    vector<double> subInstVessel;
    vector<double> subFeederBarge;
    vector<double> scourProtVessel;
    vector<vector<double> > subSupportVessels;
    vector<double> arrCabInstVessel;
    vector<double> expCabInstVessel;
    vector<double> substaInstVessel;
    vector<vector<double> > elecSupportVessels;
	//OUTPUTS************************************************************************************************************
    //General outputs
    double hubD;
    double bladeL;
    double chord;
    double nacelleW;
    double nacelleL;
    double rnaM;
    double towerD;
    double towerM;
  double bos_capex;
  double construction_insurance_cost;
  double total_contingency_cost;
  double construction_finance_cost;
  double construction_finance_factor; //factor for construction financing
  double soft_costs;

    //Substructure & Foundation outputs
    double mpileM;
    double mtransM;
    double mPileCost;
    double mTransCost;
    double jlatticeM;
    double jtransM;
    double jpileM;
    double jLatticeCost;
    double jTransCost;
    double jPileCost;
    double spStifColM;
    double spTapColM;
    double spStifColCost;
    double spTapColCost;
    double ballM;
    double ballCost;
    double ssStifColM;
    double ssTrussM;
    double ssHeaveM;
    double ssStifColCost;
    double ssTrussCost;
    double ssHeaveCost;
    double moorSysCost;
    double sSteelM;
    double sSteelCost;
    double subTotM;
    //Electrical Infrastructure outputs
	double systAngle;
	double freeCabLeng;
	double fixCabLeng;
    double nExpCab;
    double nSubstation;
    double fullStrings;
    double nTurbPS;
    double nTurbCab1;
    double nTurbCab2;
    double nTurbInter1;
    double nTurbInter2;
    double nSubsInter;
	double cab1Leng;
	double cab2Leng;
	double expCabLeng;
    double nMPT;
	double mptRating;
    double mptCost;
	double subsTopM;
	double subsTopCost;
    double arrCab1Cost;
    double arrCab2Cost;
    double expCabCost;
	double shuntReactors;
    double switchGear;
	double ancillarySys;
	double subsSubM;
    double subsPileM;
	double subsLandAssembly;
    double subsSubCost;
    double switchYard;
    double onShoreSubs;
    double onshoreMisc;
    double transLine;
    double subCabCost;
	double offSubsCost;
    double onshoreTransCost;
    //Assembly & Installation outputs
    double moorTime;
    double floatPrepTime;
    double turbDeckArea;
    double nTurbPerTrip;
    double turbInstTime;
    double subDeckArea;
    double nSubPerTrip;
    double subInstTime;
    double cab1SecM;
    double cab2SecM;
    double cab1SecPerTrip;
    double cab2SecPerTrip;
    double arrInstTime;
    double expCabSecM;
    double expCabSecPerTrip;
    double expInstTime;
    double subsInstTime;
    double totInstTime;
	double cabSurvey;
    double array_cable_install_cost;
    double export_cable_install_cost;
    double substation_install_cost;
    double turbine_install_cost;
    double substructure_install_cost;
    double electrical_install_cost;
    double mob_demob_cost;
    //Port & Staging outputs
	double entrExitCost;
	double wharfCost;
    double dockCost;
	double subLaydownA;
	double subLayCost;
	double turbLaydownA;
	double turbLayCost;
	double craneCost;
    double totPortCost;
    double totStageCost;
    //Development outputs
    double feedCost;
    double permStudyComp;
    double metFabCost;
    double decomCost;
    //Main Cost Outputs
    double subTotCost;
    double totElecCost;
    double totAnICost;
	double totPnSCost;
    double totEnMCost;
    double totDevCost;
    double commissioning;
    double total_bos_cost;
	
	//SUPPORTING FUNCTIONS************************************************************************************************************
	//General Module
	double HubDiameter();
	double BladeLength();
	double NacelleWidth();
	double NacelleLength();
	double RNAMass();
	double TowerDiameter();
	double TowerMass();
    void Soft_costs();
    void Construction_finance_factor();
    void Total_bos_cost();

	//Substructure & Foundation Module
    double MonopileLength();
	double MonoPileMass();
	double MonoPileCost();
	double MonoTransMass();
	double MonoTransCost();
	double JackLatticeMass();
	double JackLatticeCost();
	double JackTransMass();
	double JackTransCost();
	double JackPileMass();
	double JackPileCost();
	double SubstructTotalMass();
	double SparStifColMass();
	double SparTapColMass();
	double BallMass();
	double SparStifColCost();
	double SparTapColCost();
	double BallCost();
	double SemiStifColMass();
	double SemiTrussMass();
	double SemiHeaveMass();
	double SemiStifColCost();
	double SemiTrussCost();
	double SemiHeaveCost();
	double MooringSys();
	double SecondarySteelMass();
	double SecondarySteelCost();
	double SubstructTotCost();

	//Electrical Infrastructure Module
	double Strings(double& cab2CurrRating, double& arrVoltage);
	double NumTurbParStr(double& cab2CurrRating, double& arrVoltage);
	double NumTurbCable1(double& cab1CurrRating, double& arrVoltage);
	double NumTurbCable2(double& cab2CurrRating, double& arrVoltage);
	double InterfacesCable1(double& fullStrings, double& nTurbPS, double& nTurbCab1);
	double InterfacesCable2(double& fullStrings, double& nTurbPS, double&nTurbCab1, double& nTurbCab2);
	double SubstationInterfaces(double& fullStrings, double& nTurbPS);
	double SystemAngle();
	double FreeCable();
	double FixedCable();
	double Cable1Length(double& nTurbInter1);
	double Cable2Length(double& nTurbCab1, double& nTurbCab2, double& fullStrings, double& nTurbPS);
	double NumberExportCable(double& expCurrRating, double& expVoltage);
	double ExportCableLength(double& nExportCab);
	double NumberSubstation(double& nExportCab);
	double NumberMPT();
	double SingleMPTRating();
	double MPTCost();
	double SubstationTopsideMass();
	double SubstationSubMass();
	double SubstationSubPileMass();
	double SubstationSubCost();
	double SubstationTopsideCost();
	double LandTopsideAssembly();
	double OnshoreSubsCost();
	double OnshoreSubsMiscCost();
	double TransLineCost();
	double SwitchYardCost();
	double ArrayCable1Cost(double& cab1Leng, double& cab1CR, double& turbInterCR1, double& nTurbInter1);
	double ArrayCable2Cost(double& cab2Leng, double& cab2CR, double& turbInterCR2, double& nTurbInter2, double& nSubsInter, double& arrSubsInterCR);
	double ExportCableCost(double& expSubsInterCR, double& expCabCR, double& expCabLeng, double& nExpCab);
	double Switchgear();
	double ShuntReactors();
	double AncillarySystems();
	double SubseaCableCost();
	double OffshoreSubstationCost();
	double OnshoreTransCost();
	double TotElectricalCost();

	//Assembly & Installation Module
	double MooringSysInstall();
	double PrepFloatSubstructure();
	double MinTurbDeckArea();
	double TurbsPerTrip();
	double MinSubDeckArea();
	double SubPerTrip();
	double TurbineInstall();
	double SubstructureInstTime();
	double Cab1SecMass(double& arrCab1Mass);
	double Cab2SecMass(double& arrCab2Mass);
	double Cab1SecPerTrip(double& cab1SecM);
	double Cab2SecPerTrip(double cab2SecM);
	double ArrayCabInstTime(double& cab1Leng, double& cab2Leng, double& nTurbInter1, double& nTurbInter2, double& subsInter,double& cab1SecPerTrip, double& cab2SecPerTrip, double& fullStrings, double& nTurbPS,
		double& nTurbCab1, double nTurbCab2);
	double ExportCableSecMass(double& expCabMass, double& exportLeng, double& nExportCab);
	double ExportCabSecPerTrip(double& expCabSecM);
	double ExportCabInstallTime(double& expCabSecPerTrip, double& nExportCab);
	double SubsInstallTime();
	double TotalInstallTime();
	void TurbInstCost();
	void SubInstCost();
	void ElectricalInstCost();
	void VesselMobDemobCost();
	double CableRouteSurveyCost();
	double TotInstCost();

	//Port & Staging Module
	double EntranceExitCost();
	double DockingCost();
	double WharfCost();
	double SubstructureLaydownArea();
	double SubstructureLaydownCost();
	double TurbLaydownArea();
	double TurbLaydownCost();
	double NumCranes();
	double CraneCost();
	double TotalPortCost();
	double TotalStagingCost();
	double TotalPnSCost();

	//Engineering & Management Module
	double TotalEnMCost();

	//Development Module
	double FEEDCost();
	double PermitsStudiesCompliance();
	double MetTowerFabnInst();
	double DecomissExpense();
	double TotalDevCost();
	double PlantCommissioning();

    //cable cost optimizing functions
    void ArrayCabCostOptimizer();
    void ExportCabCostOptimizer();

	//EXECUTE FUNCTION************************************************************************************************************
	void run();

	// add default constructor/initializer
	wobos(): 
		 turbCapEx(std::numeric_limits<double>::quiet_NaN()), //turbine capital cost ($/kW)
		 nTurb(std::numeric_limits<double>::quiet_NaN()),//number of turbines
		 rotorD(std::numeric_limits<double>::quiet_NaN()),//rotor diameter (m)
		 turbR(std::numeric_limits<double>::quiet_NaN()),//turbine rating (MW)
		 hubH(std::numeric_limits<double>::quiet_NaN()),//hub height (m)
		 waterD(std::numeric_limits<double>::quiet_NaN()),// water depth (m)
		 distShore(std::numeric_limits<double>::quiet_NaN()),//distance to shore from install site (km)
		 distPort(std::numeric_limits<double>::quiet_NaN()),//distance to install site from install port (km)
		 distPtoA(std::numeric_limits<double>::quiet_NaN()),//distance from install port to inshore assembly area (km) (spar only)
		 distAtoS(std::numeric_limits<double>::quiet_NaN()),//distance from inshore assembly area to install site (km) (spar Only)
		 substructure(std::numeric_limits<int>::quiet_NaN()), //type of substructure
		 anchor(std::numeric_limits<int>::quiet_NaN()), //anchor type
		 turbInstallMethod(std::numeric_limits<int>::quiet_NaN()), //turbine installation method
		 towerInstallMethod(std::numeric_limits<int>::quiet_NaN()), //tower installation method
		 installStrategy(std::numeric_limits<int>::quiet_NaN()), //installation vessel strategy
		 cableOptimizer(std::numeric_limits<int>::quiet_NaN()), //switch to run the cable optimizer or not
		 moorLines(std::numeric_limits<double>::quiet_NaN()),//number of mooring lines for floating substructures
		 buryDepth(std::numeric_limits<double>::quiet_NaN()),//array and export cable burial depth (m)
		 arrayY(std::numeric_limits<double>::quiet_NaN()),//turbine array spacing between turbines on same row (rotor diameters)
		 arrayX(std::numeric_limits<double>::quiet_NaN()),// turbine array spacing between turbine rows (rotor diameters)
		 substructCont(std::numeric_limits<double>::quiet_NaN()),//substructure install weather contingency
		 turbCont(std::numeric_limits<double>::quiet_NaN()),//turbine install weather contingency
		 elecCont(std::numeric_limits<double>::quiet_NaN()),//turbine install weather contingency
		 interConVolt(std::numeric_limits<double>::quiet_NaN()),//grid interconnect voltage (kV)
		 distInterCon(std::numeric_limits<double>::quiet_NaN()),//distance from onshore substation to grid interconnect (miles)
		 scrapVal(std::numeric_limits<double>::quiet_NaN()),//scrap value of decommissioned components ($)
		 number_install_seasons(std::numeric_limits<double>::quiet_NaN()), //number of vessel mobilization/install seasons
		 projLife(std::numeric_limits<double>::quiet_NaN()),//economic lifetime of the project (years)
		 inspectClear(std::numeric_limits<double>::quiet_NaN()),//inspection clearance for substructure and turbine components (m)
		 plantComm(std::numeric_limits<double>::quiet_NaN()), //plant commissioning cost factor
		 procurement_contingency(std::numeric_limits<double>::quiet_NaN()), //contingency factor for procurement costs
		 install_contingency(std::numeric_limits<double>::quiet_NaN()), //contingency factor for installation costs
		 construction_insurance(std::numeric_limits<double>::quiet_NaN()), //insurance during construction factor
		 capital_cost_year_0(std::numeric_limits<double>::quiet_NaN()), //capital cost spent in year 0
		 capital_cost_year_1(std::numeric_limits<double>::quiet_NaN()), //capital cost spent in year 1
		 capital_cost_year_2(std::numeric_limits<double>::quiet_NaN()), //capital cost spent in year 2
		 capital_cost_year_3(std::numeric_limits<double>::quiet_NaN()), //capital cost spent in year 3
		 capital_cost_year_4(std::numeric_limits<double>::quiet_NaN()), //capital cost spent in year 4
		 capital_cost_year_5(std::numeric_limits<double>::quiet_NaN()), //capital cost spent in year 5
		 tax_rate(std::numeric_limits<double>::quiet_NaN()), //effective tax_rate (federal & state)
		 interest_during_construction(std::numeric_limits<double>::quiet_NaN()), //interest rate during construction
		 mpileCR(std::numeric_limits<double>::quiet_NaN()),//monopile pile cost rate ($/tonne)
		 mtransCR(std::numeric_limits<double>::quiet_NaN()),//monopile transition piece cost rate ($/tonne)
		 mpileD(std::numeric_limits<double>::quiet_NaN()),//monopile pile diameter (m)
		 mpileL(std::numeric_limits<double>::quiet_NaN()),//monopile length (m)
		 jlatticeCR(std::numeric_limits<double>::quiet_NaN()),//jacket lattice cost rate ($/tonne)
		 jtransCR(std::numeric_limits<double>::quiet_NaN()),//jacket transition piece cost rate ($/tonne)
		 jpileCR(std::numeric_limits<double>::quiet_NaN()),//jacket pile cost rate ($/tonne)
		 jlatticeA(std::numeric_limits<double>::quiet_NaN()),//jacket lattice footprint area
		 jpileL(std::numeric_limits<double>::quiet_NaN()),//jacket pile length
		 jpileD(std::numeric_limits<double>::quiet_NaN()),//jacket pile diameter
		 spStifColCR(std::numeric_limits<double>::quiet_NaN()),//spar stiffened column cost rate ($/tonne)
		 spTapColCR(std::numeric_limits<double>::quiet_NaN()),//spar tapered column cost rate ($/tonne)
		 ballCR(std::numeric_limits<double>::quiet_NaN()),//ballast cost rate ($/tonne)
		 deaFixLeng(std::numeric_limits<double>::quiet_NaN()),//drag embedment anchor fixed mooring line length
		 ssStifColCR(std::numeric_limits<double>::quiet_NaN()),//semisubmersible stiffened column cost rate ($/tonne)
		 ssTrussCR(std::numeric_limits<double>::quiet_NaN()),// semisubmersible truss cost rate ($/tonne)
		 ssHeaveCR(std::numeric_limits<double>::quiet_NaN()),//semisubmersible heave plate cost rate ($/tonne)
		 sSteelCR(std::numeric_limits<double>::quiet_NaN()),//secondary steel cost rate ($/tonne)
		 moorDia(std::numeric_limits<double>::quiet_NaN()),//mooring line diameter
		 moorCR(std::numeric_limits<double>::quiet_NaN()),//mooring line cost rate ($/m)
		 mpEmbedL(std::numeric_limits<double>::quiet_NaN()),//monopile embedment length (m)
		 scourMat(std::numeric_limits<double>::quiet_NaN()),
		 pwrFac(std::numeric_limits<double>::quiet_NaN()),//power factor to estimate losses
		 buryFac(std::numeric_limits<double>::quiet_NaN()),//cable burial factor
		 arrVoltage(std::numeric_limits<double>::quiet_NaN()),//array cable voltage (kV)
		 arrCab1Size(std::numeric_limits<double>::quiet_NaN()),//diameter in square millimeters of array cable 1
		 arrCab1Mass(std::numeric_limits<double>::quiet_NaN()),//mass of array cable 1 (kg/m)
		 cab1CurrRating(std::numeric_limits<double>::quiet_NaN()),//current rating of array cable 1 (amps)
		 cab1CR(std::numeric_limits<double>::quiet_NaN()),//cost rate of array cable 1 ($/m)
		 cab1TurbInterCR(std::numeric_limits<double>::quiet_NaN()),//array cable size 1 turbine interface cost rate ($/interface)
		 arrCab2Size(std::numeric_limits<double>::quiet_NaN()),//diameter in square millimeters of array cable 2
		 arrCab2Mass(std::numeric_limits<double>::quiet_NaN()),//mass of array cable 2 (kg/m)
		 cab2CurrRating(std::numeric_limits<double>::quiet_NaN()),//current rating of array cable 2 (amps)
		 cab2CR(std::numeric_limits<double>::quiet_NaN()),//cost rate of array cable 2 ($/m)
		 cab2TurbInterCR(std::numeric_limits<double>::quiet_NaN()),//array cable size 2 turbine interface cost rate ($/interface)
		 cab2SubsInterCR(std::numeric_limits<double>::quiet_NaN()),//array cable size 2 substation interface cost rate ($/interface)
		 catLengFac(std::numeric_limits<double>::quiet_NaN()),//free hanging or catenary cable length factor
		 exCabFac(std::numeric_limits<double>::quiet_NaN()),// excess cable factor
		 subsTopFab(std::numeric_limits<double>::quiet_NaN()),//substation topside fabrication cost ($/tonne)
		 subsTopDes(std::numeric_limits<double>::quiet_NaN()),//substation topside design cost ($)
		 topAssemblyFac(std::numeric_limits<double>::quiet_NaN()),//land based substation topside assembly factor
		 subsJackCR(std::numeric_limits<double>::quiet_NaN()),//substation jacket substructure cost rate ($/tonne)
		 subsPileCR(std::numeric_limits<double>::quiet_NaN()),//substation jacket pile cost rate ($/tonne)
		 dynCabFac(std::numeric_limits<double>::quiet_NaN()),//dynamic/free hanging cable cost premium
		 shuntCR(std::numeric_limits<double>::quiet_NaN()),//shunt reactor cost rate ($/MVA)
		 highVoltSG(std::numeric_limits<double>::quiet_NaN()),//high voltage switchgear cost ($)
		 medVoltSG(std::numeric_limits<double>::quiet_NaN()),//medium voltage switchgear cost ($)
		 backUpGen(std::numeric_limits<double>::quiet_NaN()),//back up generator cost ($)
		 workSpace(std::numeric_limits<double>::quiet_NaN()),//substation workshop and accommodations cost ($)
		 otherAncillary(std::numeric_limits<double>::quiet_NaN()),//substation other ancillary costs ($)
		 mptCR(std::numeric_limits<double>::quiet_NaN()),//main power transformer cost rate ($/MVA)
		 expVoltage(std::numeric_limits<double>::quiet_NaN()),//export cable voltage (kV)
		 expCabSize(std::numeric_limits<double>::quiet_NaN()),//diameter in square millimeters of the export cable
		 expCabMass(std::numeric_limits<double>::quiet_NaN()),//mass of the export cable (kg/m)
		 expCabCR(std::numeric_limits<double>::quiet_NaN()),//cost rate of the export cable ($/m)
		 expCurrRating(std::numeric_limits<double>::quiet_NaN()),//export cable rating (amps)
		 expSubsInterCR(std::numeric_limits<double>::quiet_NaN()),//cost rate of export cable substation interfaces ($/interface)
		 moorTimeFac(std::numeric_limits<double>::quiet_NaN()),//mooring installation timing factor (hrs/m)
		 moorLoadout(std::numeric_limits<double>::quiet_NaN()),//mooring system loadout timing (hrs)
		 moorSurvey(std::numeric_limits<double>::quiet_NaN()),//mooring system anchor position survey timing (hrs)
		 prepAA(std::numeric_limits<double>::quiet_NaN()),//prep inshore assembly area timing (hrs)
		 prepSpar(std::numeric_limits<double>::quiet_NaN()),//prep spare for tow out to assembly area timing (hrs)
		 upendSpar(std::numeric_limits<double>::quiet_NaN()),//upend and ballast the spar timing (hrs)
		 prepSemi(std::numeric_limits<double>::quiet_NaN()),//prep semisubmersible for turbine install timing (hrs)
		 turbFasten(std::numeric_limits<double>::quiet_NaN()),//fasten turbine for transport timing (hrs)
		 boltTower(std::numeric_limits<double>::quiet_NaN()),// bolt tower to substructure timing (hrs)
		 boltNacelle1(std::numeric_limits<double>::quiet_NaN()),//bolt nacelle to tower timing individual components method (hrs)
		 boltNacelle2(std::numeric_limits<double>::quiet_NaN()),//bolt nacelle to tower timing bunny ears method (hrs)
		 boltNacelle3(std::numeric_limits<double>::quiet_NaN()),//bolt nacelle to tower timing assembled rotor method (hrs)
		 boltBlade1(std::numeric_limits<double>::quiet_NaN()),//bolt blade to rotor timing individual components method (hrs)
		 boltBlade2(std::numeric_limits<double>::quiet_NaN()),//bolt blade to rotor timing bunny ears method (hrs)
		 boltRotor(std::numeric_limits<double>::quiet_NaN()),//bolt rotor to nacelle timing assembled rotor method (hrs)
		 vesselPosTurb(std::numeric_limits<double>::quiet_NaN()),//vessel positioning timing turbine install (hrs)
		 vesselPosJack(std::numeric_limits<double>::quiet_NaN()),//vessel positioning timing jacket install (hrs)
		 vesselPosMono(std::numeric_limits<double>::quiet_NaN()),//vessel positioning timing monopile install (hrs)
		 subsVessPos(std::numeric_limits<double>::quiet_NaN()),//vessel positioning timing offshore substation install (hrs)
		 monoFasten(std::numeric_limits<double>::quiet_NaN()),//fasten monopile for transport timing (hrs)
		 jackFasten(std::numeric_limits<double>::quiet_NaN()),//fasten jacket for transport timing (hrs)
		 prepGripperMono(std::numeric_limits<double>::quiet_NaN()),//prepare pile gripper and upender timing monopile install (hrs)
		 prepGripperJack(std::numeric_limits<double>::quiet_NaN()),//prepare pile gripper and upender timing iacket install (hrs)
		 placePiles(std::numeric_limits<double>::quiet_NaN()),//lift and place jacket piles timing (hrs)
		 prepHamMono(std::numeric_limits<double>::quiet_NaN()),//prepare pile hammer timing monopile install (hrs)
		 removeHamMono(std::numeric_limits<double>::quiet_NaN()),//remove hammer timing monopile install (hrs)
		 prepHamJack(std::numeric_limits<double>::quiet_NaN()),//prepare pile hammer timing iacket install (hrs)
		 removeHamJack(std::numeric_limits<double>::quiet_NaN()),//remove hammer timing iacket install (hrs)
		 placeJack(std::numeric_limits<double>::quiet_NaN()),//place  jacket timing (hrs)
		 levJack(std::numeric_limits<double>::quiet_NaN()),//level jacket timing (hrs)
		 placeTemplate(std::numeric_limits<double>::quiet_NaN()),//place jacket template timing (hrs)
		 hamRate(std::numeric_limits<double>::quiet_NaN()),//pile hammer rate (m/hr)
		 placeMP(std::numeric_limits<double>::quiet_NaN()),//place monopile pile timing (hrs)
		 instScour(std::numeric_limits<double>::quiet_NaN()),//install scour protection (hrs)
		 placeTP(std::numeric_limits<double>::quiet_NaN()),//place transition piece on monopile timing (hrs)
		 groutTP(std::numeric_limits<double>::quiet_NaN()),//grout transition piece (hrs)
		 tpCover(std::numeric_limits<double>::quiet_NaN()),//install transition piece cover timing (hrs)
		 prepTow(std::numeric_limits<double>::quiet_NaN()),//prep floating substructure for towing timing (hrs)
		 spMoorCon(std::numeric_limits<double>::quiet_NaN()),//connect spar to mooring system timing (hrs)
		 ssMoorCon(std::numeric_limits<double>::quiet_NaN()),//connect semisubmersible to mooring system (hrs)
		 spMoorCheck(std::numeric_limits<double>::quiet_NaN()),//check mooring connections to spar timing (hrs)
		 ssMoorCheck(std::numeric_limits<double>::quiet_NaN()),//check mooring connections to semisubmersible timing (hrs)
		 ssBall(std::numeric_limits<double>::quiet_NaN()),//ballast semisubmersible timing (hrs)
		 surfLayRate(std::numeric_limits<double>::quiet_NaN()),//electrical cable surface lay rate (m/hr)
		 cabPullIn(std::numeric_limits<double>::quiet_NaN()),//array cable pull in to interfaces timing (hrs)
		 cabTerm(std::numeric_limits<double>::quiet_NaN()),//cable termination and testing timing (hrs)
		 cabLoadout(std::numeric_limits<double>::quiet_NaN()),//array cable loadout timing (hrs)
		 buryRate(std::numeric_limits<double>::quiet_NaN()),//cable bury rate (m/hr)
		 subsPullIn(std::numeric_limits<double>::quiet_NaN()),//cable pull in to substation timing (hrs)
		 shorePullIn(std::numeric_limits<double>::quiet_NaN()),//cable pull in to shore timing (hrs)
		 landConstruct(std::numeric_limits<double>::quiet_NaN()),//land construction of required onshore electrical systems timing (days)
		 expCabLoad(std::numeric_limits<double>::quiet_NaN()),//export cable loadout timing (hrs)
		 subsLoad(std::numeric_limits<double>::quiet_NaN()),//substation loadout timing (hrs)
		 placeTop(std::numeric_limits<double>::quiet_NaN()),//lift and place substation topside timing (hrs)
		 pileSpreadDR(std::numeric_limits<double>::quiet_NaN()),//piling equipment spread day rate ($/day)
		 pileSpreadMob(std::numeric_limits<double>::quiet_NaN()),//piling equipment spread mobilization/demobilization cost ($)
		 groutSpreadDR(std::numeric_limits<double>::quiet_NaN()),//grouting equipment spread day rate ($/day)
		 groutSpreadMob(std::numeric_limits<double>::quiet_NaN()),//grouting equipment spread mobilization/demobilization cost ($)
		 seaSpreadDR(std::numeric_limits<double>::quiet_NaN()),//suction pile anchor vessel and equipment spread day rate ($/day)
		 seaSpreadMob(std::numeric_limits<double>::quiet_NaN()),//suction pile anchor vessel and equipment spread mobilization/demobilization cost ($)
		 compRacks(std::numeric_limits<double>::quiet_NaN()),//component racks cost ($)
		 cabSurveyCR(std::numeric_limits<double>::quiet_NaN()),//cost rate of surveying and verifying electrical cable installation ($/)
		 cabDrillDist(std::numeric_limits<double>::quiet_NaN()),//horizontal drilling distance for cable landfall (m)
		 cabDrillCR(std::numeric_limits<double>::quiet_NaN()),//horizontal drilling cost rate ($/m)
		 mpvRentalDR(std::numeric_limits<double>::quiet_NaN()),//MPV rental day rate ($/day)
		 diveTeamDR(std::numeric_limits<double>::quiet_NaN()),//cable landfall dive team day rate ($/day)
		 winchDR(std::numeric_limits<double>::quiet_NaN()),//Cable winch day rate
		 civilWork(std::numeric_limits<double>::quiet_NaN()),//civil construction work cost ($)
		 elecWork(std::numeric_limits<double>::quiet_NaN()),//electrical work cost ($)
		 nCrane600(std::numeric_limits<double>::quiet_NaN()),
		 nCrane1000(std::numeric_limits<double>::quiet_NaN()),
		 crane600DR(std::numeric_limits<double>::quiet_NaN()),//600 tonne capacity crawler crane day rate ($/day)
		 crane1000DR(std::numeric_limits<double>::quiet_NaN()),//1000 tonne capacity crawler crane day rate ($/day)
		 craneMobDemob(std::numeric_limits<double>::quiet_NaN()),//crane mobilization and demobilization cost ($)
		 entranceExitRate(std::numeric_limits<double>::quiet_NaN()),//port entrance and exit cost ($/m^2/occurrence)
		 dockRate(std::numeric_limits<double>::quiet_NaN()),//port docking cost ($/day)
		 wharfRate(std::numeric_limits<double>::quiet_NaN()),//port wharf loading and unloading cost ($/tonne)
		 laydownCR(std::numeric_limits<double>::quiet_NaN()),//port laydown and storage cost ($/m/day)
		 estEnMFac(std::numeric_limits<double>::quiet_NaN()),//estimated engineering and management cost factor
		 preFEEDStudy(std::numeric_limits<double>::quiet_NaN()),//pre-fornt end engineering design (FEED) study cost ($)
		 feedStudy(std::numeric_limits<double>::quiet_NaN()),// FEED study cost ($)
		 stateLease(std::numeric_limits<double>::quiet_NaN()),//state leasing cost ($)
		 outConShelfLease(std::numeric_limits<double>::quiet_NaN()),//outer continental shelf lease cost ($)
		 saPlan(std::numeric_limits<double>::quiet_NaN()),//site assessment plan cost ($)
		 conOpPlan(std::numeric_limits<double>::quiet_NaN()),//construction operations plan cost ($)
		 nepaEisMet(std::numeric_limits<double>::quiet_NaN()),//national environmental protection agency (NEPA) environmental impact (EIS) meteorological (met) tower study cost ($)
		 physResStudyMet(std::numeric_limits<double>::quiet_NaN()),//physical resource met tower study cost ($)
		 bioResStudyMet(std::numeric_limits<double>::quiet_NaN()),//biological resource met tower study ($)
		 socEconStudyMet(std::numeric_limits<double>::quiet_NaN()),//socioeconomic met tower study cost ($)
		 navStudyMet(std::numeric_limits<double>::quiet_NaN()),//navigation met tower study ($)
		 nepaEisProj(std::numeric_limits<double>::quiet_NaN()),// NEPA EIS project site study cost ($)
		 physResStudyProj(std::numeric_limits<double>::quiet_NaN()),//physical resource project site study cost ($)
		 bioResStudyProj(std::numeric_limits<double>::quiet_NaN()),//biological resource project site study cost ($)
		 socEconStudyProj(std::numeric_limits<double>::quiet_NaN()),//socioeconomic project site study cost ($)
		 navStudyProj(std::numeric_limits<double>::quiet_NaN()),//navigation project site study cost ($)
		 coastZoneManAct(std::numeric_limits<double>::quiet_NaN()),//coastal zone management act compliance cost ($)
		 rivsnHarbsAct(std::numeric_limits<double>::quiet_NaN()),//rivers & harbors act section 10 compliance cost ($)
		 cleanWatAct402(std::numeric_limits<double>::quiet_NaN()),//clean water act section 402 compliance cost ($)
		 cleanWatAct404(std::numeric_limits<double>::quiet_NaN()),//clean water act section 404 compliance cost ($)
		 faaPlan(std::numeric_limits<double>::quiet_NaN()),//federal aviation administration (FAA) plans and mitigation cost ($)
		 endSpecAct(std::numeric_limits<double>::quiet_NaN()),//endangered species act compliance cost ($)
		 marMamProtAct(std::numeric_limits<double>::quiet_NaN()),//marine mammal protection act compliance cost ($)
		 migBirdAct(std::numeric_limits<double>::quiet_NaN()),//migratory bird act compliance ($)
		 natHisPresAct(std::numeric_limits<double>::quiet_NaN()),//national historic preservation act compliance cost ($)
		 addLocPerm(std::numeric_limits<double>::quiet_NaN()),//additional local and state permissions and compliance cost ($)
		 metTowCR(std::numeric_limits<double>::quiet_NaN()),//meteorological tower fabrication, design, and install cost rate ($/MW)
		 decomDiscRate(std::numeric_limits<double>::quiet_NaN()),//decommissioning expense discount rate
		 hubD(std::numeric_limits<double>::quiet_NaN()),
		 bladeL(std::numeric_limits<double>::quiet_NaN()),
		 chord(std::numeric_limits<double>::quiet_NaN()),
		 nacelleW(std::numeric_limits<double>::quiet_NaN()),
		 nacelleL(std::numeric_limits<double>::quiet_NaN()),
		 rnaM(std::numeric_limits<double>::quiet_NaN()),
		 towerD(std::numeric_limits<double>::quiet_NaN()),
		 towerM(std::numeric_limits<double>::quiet_NaN()),
		 construction_insurance_cost(std::numeric_limits<double>::quiet_NaN()),
		 total_contingency_cost(std::numeric_limits<double>::quiet_NaN()),
		 construction_finance_cost(std::numeric_limits<double>::quiet_NaN()),
		 construction_finance_factor(std::numeric_limits<double>::quiet_NaN()),
		 soft_costs(std::numeric_limits<double>::quiet_NaN()),
		 mpileM(std::numeric_limits<double>::quiet_NaN()),
		 mtransM(std::numeric_limits<double>::quiet_NaN()),
		 mPileCost(std::numeric_limits<double>::quiet_NaN()),
		 mTransCost(std::numeric_limits<double>::quiet_NaN()),
		 jlatticeM(std::numeric_limits<double>::quiet_NaN()),
		 jtransM(std::numeric_limits<double>::quiet_NaN()),
		 jpileM(std::numeric_limits<double>::quiet_NaN()),
		 jLatticeCost(std::numeric_limits<double>::quiet_NaN()),
		 jTransCost(std::numeric_limits<double>::quiet_NaN()),
		 jPileCost(std::numeric_limits<double>::quiet_NaN()),
		 spStifColM(std::numeric_limits<double>::quiet_NaN()),
		 spTapColM(std::numeric_limits<double>::quiet_NaN()),
		 spStifColCost(std::numeric_limits<double>::quiet_NaN()),
		 spTapColCost(std::numeric_limits<double>::quiet_NaN()),
		 ballM(std::numeric_limits<double>::quiet_NaN()),
		 ballCost(std::numeric_limits<double>::quiet_NaN()),
		 ssStifColM(std::numeric_limits<double>::quiet_NaN()),
		 ssTrussM(std::numeric_limits<double>::quiet_NaN()),
		 ssHeaveM(std::numeric_limits<double>::quiet_NaN()),
		 ssStifColCost(std::numeric_limits<double>::quiet_NaN()),
		 ssTrussCost(std::numeric_limits<double>::quiet_NaN()),
		 ssHeaveCost(std::numeric_limits<double>::quiet_NaN()),
		 moorSysCost(std::numeric_limits<double>::quiet_NaN()),
		 sSteelM(std::numeric_limits<double>::quiet_NaN()),
		 sSteelCost(std::numeric_limits<double>::quiet_NaN()),
		 subTotM(std::numeric_limits<double>::quiet_NaN()),
		 systAngle(std::numeric_limits<double>::quiet_NaN()),
		 freeCabLeng(std::numeric_limits<double>::quiet_NaN()),
		 fixCabLeng(std::numeric_limits<double>::quiet_NaN()),
		 nExpCab(std::numeric_limits<double>::quiet_NaN()),
		 nSubstation(std::numeric_limits<double>::quiet_NaN()),
		 fullStrings(std::numeric_limits<double>::quiet_NaN()),
		 nTurbPS(std::numeric_limits<double>::quiet_NaN()),
		 nTurbCab1(std::numeric_limits<double>::quiet_NaN()),
		 nTurbCab2(std::numeric_limits<double>::quiet_NaN()),
		 nTurbInter1(std::numeric_limits<double>::quiet_NaN()),
		 nTurbInter2(std::numeric_limits<double>::quiet_NaN()),
		 nSubsInter(std::numeric_limits<double>::quiet_NaN()),
		 cab1Leng(std::numeric_limits<double>::quiet_NaN()),
		 cab2Leng(std::numeric_limits<double>::quiet_NaN()),
		 expCabLeng(std::numeric_limits<double>::quiet_NaN()),
		 nMPT(std::numeric_limits<double>::quiet_NaN()),
		 mptRating(std::numeric_limits<double>::quiet_NaN()),
		 mptCost(std::numeric_limits<double>::quiet_NaN()),
		 subsTopM(std::numeric_limits<double>::quiet_NaN()),
		 subsTopCost(std::numeric_limits<double>::quiet_NaN()),
		 arrCab1Cost(std::numeric_limits<double>::quiet_NaN()),
		 arrCab2Cost(std::numeric_limits<double>::quiet_NaN()),
		 expCabCost(std::numeric_limits<double>::quiet_NaN()),
		 shuntReactors(std::numeric_limits<double>::quiet_NaN()),
		 switchGear(std::numeric_limits<double>::quiet_NaN()),
		 ancillarySys(std::numeric_limits<double>::quiet_NaN()),
		 subsSubM(std::numeric_limits<double>::quiet_NaN()),
		 subsPileM(std::numeric_limits<double>::quiet_NaN()),
		 subsLandAssembly(std::numeric_limits<double>::quiet_NaN()),
		 subsSubCost(std::numeric_limits<double>::quiet_NaN()),
		 switchYard(std::numeric_limits<double>::quiet_NaN()),
		 onShoreSubs(std::numeric_limits<double>::quiet_NaN()),
		 onshoreMisc(std::numeric_limits<double>::quiet_NaN()),
		 transLine(std::numeric_limits<double>::quiet_NaN()),
		 subCabCost(std::numeric_limits<double>::quiet_NaN()),
		 offSubsCost(std::numeric_limits<double>::quiet_NaN()),
		 onshoreTransCost(std::numeric_limits<double>::quiet_NaN()),
		 moorTime(std::numeric_limits<double>::quiet_NaN()),
		 floatPrepTime(std::numeric_limits<double>::quiet_NaN()),
		 turbDeckArea(std::numeric_limits<double>::quiet_NaN()),
		 nTurbPerTrip(std::numeric_limits<double>::quiet_NaN()),
		 turbInstTime(std::numeric_limits<double>::quiet_NaN()),
		 subDeckArea(std::numeric_limits<double>::quiet_NaN()),
		 nSubPerTrip(std::numeric_limits<double>::quiet_NaN()),
		 subInstTime(std::numeric_limits<double>::quiet_NaN()),
		 cab1SecM(std::numeric_limits<double>::quiet_NaN()),
		 cab2SecM(std::numeric_limits<double>::quiet_NaN()),
		 cab1SecPerTrip(std::numeric_limits<double>::quiet_NaN()),
		 cab2SecPerTrip(std::numeric_limits<double>::quiet_NaN()),
		 arrInstTime(std::numeric_limits<double>::quiet_NaN()),
		 expCabSecM(std::numeric_limits<double>::quiet_NaN()),
		 expCabSecPerTrip(std::numeric_limits<double>::quiet_NaN()),
		 expInstTime(std::numeric_limits<double>::quiet_NaN()),
		 subsInstTime(std::numeric_limits<double>::quiet_NaN()),
		 totInstTime(std::numeric_limits<double>::quiet_NaN()),
		 cabSurvey(std::numeric_limits<double>::quiet_NaN()),
		 array_cable_install_cost(std::numeric_limits<double>::quiet_NaN()),
		 export_cable_install_cost(std::numeric_limits<double>::quiet_NaN()),
		 substation_install_cost(std::numeric_limits<double>::quiet_NaN()),
		 turbine_install_cost(std::numeric_limits<double>::quiet_NaN()),
		 substructure_install_cost(std::numeric_limits<double>::quiet_NaN()),
		 electrical_install_cost(std::numeric_limits<double>::quiet_NaN()),
		 mob_demob_cost(std::numeric_limits<double>::quiet_NaN()),
		 entrExitCost(std::numeric_limits<double>::quiet_NaN()),
		 wharfCost(std::numeric_limits<double>::quiet_NaN()),
		 dockCost(std::numeric_limits<double>::quiet_NaN()),
		 subLaydownA(std::numeric_limits<double>::quiet_NaN()),
		 subLayCost(std::numeric_limits<double>::quiet_NaN()),
		 turbLaydownA(std::numeric_limits<double>::quiet_NaN()),
		 turbLayCost(std::numeric_limits<double>::quiet_NaN()),
		 craneCost(std::numeric_limits<double>::quiet_NaN()),
		 totPortCost(std::numeric_limits<double>::quiet_NaN()),
		 totStageCost(std::numeric_limits<double>::quiet_NaN()),
		 feedCost(std::numeric_limits<double>::quiet_NaN()),
		 permStudyComp(std::numeric_limits<double>::quiet_NaN()),
		 metFabCost(std::numeric_limits<double>::quiet_NaN()),
		 decomCost(std::numeric_limits<double>::quiet_NaN()),
		 subTotCost(std::numeric_limits<double>::quiet_NaN()),
		 totElecCost(std::numeric_limits<double>::quiet_NaN()),
		 totAnICost(std::numeric_limits<double>::quiet_NaN()),
		 totPnSCost(std::numeric_limits<double>::quiet_NaN()),
		 totEnMCost(std::numeric_limits<double>::quiet_NaN()),
		 totDevCost(std::numeric_limits<double>::quiet_NaN()),
		 commissioning(std::numeric_limits<double>::quiet_NaN()),
		 total_bos_cost(std::numeric_limits<double>::quiet_NaN())
	{}
};
extern wobos wobos_obj;
extern wobos *wobos_cm;
#endif
