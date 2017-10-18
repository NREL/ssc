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
    void Insurance_during_construction();
    void Construction_finance();
    void Construction_finance_factor();
    void Total_contingency();
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
		 turbCapEx(0), //turbine capital cost ($/kW)
		 nTurb(0),//number of turbines
		 rotorD(0),//rotor diameter (m)
		 turbR(0),//turbine rating (MW)
		 hubH(0),//hub height (m)
		 waterD(0),// water depth (m)
		 distShore(0),//distance to shore from install site (km)
		 distPort(0),//distance to install site from install port (km)
		 distPtoA(0),//distance from install port to inshore assembly area (km) (spar only)
		 distAtoS(0),//distance from inshore assembly area to install site (km) (spar Only)
		 substructure(0), //type of substructure
		 anchor(0), //anchor type
		 turbInstallMethod(0), //turbine installation method
		 towerInstallMethod(0), //tower installation method
		 installStrategy(0), //installation vessel strategy
		 cableOptimizer(0), //switch to run the cable optimizer or not
		 moorLines(0),//number of mooring lines for floating substructures
		 buryDepth(0),//array and export cable burial depth (m)
		 arrayY(0),//turbine array spacing between turbines on same row (rotor diameters)
		 arrayX(0),// turbine array spacing between turbine rows (rotor diameters)
		 substructCont(0),//substructure install weather contingency
		 turbCont(0),//turbine install weather contingency
		 elecCont(0),//turbine install weather contingency
		 interConVolt(0),//grid interconnect voltage (kV)
		 distInterCon(0),//distance from onshore substation to grid interconnect (miles)
		 scrapVal(0),//scrap value of decommissioned components ($)
		 number_install_seasons(0), //number of vessel mobilization/install seasons
		 projLife(0),//economic lifetime of the project (years)
		 inspectClear(0),//inspection clearance for substructure and turbine components (m)
		 plantComm(0), //plant commissioning cost factor
		 procurement_contingency(0), //contingency factor for procurement costs
		 install_contingency(0), //contingency factor for installation costs
		 construction_insurance(0), //insurance during construction factor
		 capital_cost_year_0(0), //capital cost spent in year 0
		 capital_cost_year_1(0), //capital cost spent in year 1
		 capital_cost_year_2(0), //capital cost spent in year 2
		 capital_cost_year_3(0), //capital cost spent in year 3
		 capital_cost_year_4(0), //capital cost spent in year 4
		 capital_cost_year_5(0), //capital cost spent in year 5
		 tax_rate(0), //effective tax_rate (federal & state)
		 interest_during_construction(0), //interest rate during construction
		 mpileCR(0),//monopile pile cost rate ($/tonne)
		 mtransCR(0),//monopile transition piece cost rate ($/tonne)
		 mpileD(0),//monopile pile diameter (m)
		 mpileL(0),//monopile length (m)
		 jlatticeCR(0),//jacket lattice cost rate ($/tonne)
		 jtransCR(0),//jacket transition piece cost rate ($/tonne)
		 jpileCR(0),//jacket pile cost rate ($/tonne)
		 jlatticeA(0),//jacket lattice footprint area
		 jpileL(0),//jacket pile length
		 jpileD(0),//jacket pile diameter
		 spStifColCR(0),//spar stiffened column cost rate ($/tonne)
		 spTapColCR(0),//spar tapered column cost rate ($/tonne)
		 ballCR(0),//ballast cost rate ($/tonne)
		 deaFixLeng(0),//drag embedment anchor fixed mooring line length
		 ssStifColCR(0),//semisubmersible stiffened column cost rate ($/tonne)
		 ssTrussCR(0),// semisubmersible truss cost rate ($/tonne)
		 ssHeaveCR(0),//semisubmersible heave plate cost rate ($/tonne)
		 sSteelCR(0),//secondary steel cost rate ($/tonne)
		 moorDia(0),//mooring line diameter
		 moorCR(0),//mooring line cost rate ($/m)
		 mpEmbedL(0),//monopile embedment length (m)
		 scourMat(0),
		 pwrFac(0),//power factor to estimate losses
		 buryFac(0),//cable burial factor
		 arrVoltage(0),//array cable voltage (kV)
		 arrCab1Size(0),//diameter in square millimeters of array cable 1
		 arrCab1Mass(0),//mass of array cable 1 (kg/m)
		 cab1CurrRating(0),//current rating of array cable 1 (amps)
		 cab1CR(0),//cost rate of array cable 1 ($/m)
		 cab1TurbInterCR(0),//array cable size 1 turbine interface cost rate ($/interface)
		 arrCab2Size(0),//diameter in square millimeters of array cable 2
		 arrCab2Mass(0),//mass of array cable 2 (kg/m)
		 cab2CurrRating(0),//current rating of array cable 2 (amps)
		 cab2CR(0),//cost rate of array cable 2 ($/m)
		 cab2TurbInterCR(0),//array cable size 2 turbine interface cost rate ($/interface)
		 cab2SubsInterCR(0),//array cable size 2 substation interface cost rate ($/interface)
		 catLengFac(0),//free hanging or catenary cable length factor
		 exCabFac(0),// excess cable factor
		 subsTopFab(0),//substation topside fabrication cost ($/tonne)
		 subsTopDes(0),//substation topside design cost ($)
		 topAssemblyFac(0),//land based substation topside assembly factor
		 subsJackCR(0),//substation jacket substructure cost rate ($/tonne)
		 subsPileCR(0),//substation jacket pile cost rate ($/tonne)
		 dynCabFac(0),//dynamic/free hanging cable cost premium
		 shuntCR(0),//shunt reactor cost rate ($/MVA)
		 highVoltSG(0),//high voltage switchgear cost ($)
		 medVoltSG(0),//medium voltage switchgear cost ($)
		 backUpGen(0),//back up generator cost ($)
		 workSpace(0),//substation workshop and accommodations cost ($)
		 otherAncillary(0),//substation other ancillary costs ($)
		 mptCR(0),//main power transformer cost rate ($/MVA)
		 expVoltage(0),//export cable voltage (kV)
		 expCabSize(0),//diameter in square millimeters of the export cable
		 expCabMass(0),//mass of the export cable (kg/m)
		 expCabCR(0),//cost rate of the export cable ($/m)
		 expCurrRating(0),//export cable rating (amps)
		 expSubsInterCR(0),//cost rate of export cable substation interfaces ($/interface)
		 moorTimeFac(0),//mooring installation timing factor (hrs/m)
		 moorLoadout(0),//mooring system loadout timing (hrs)
		 moorSurvey(0),//mooring system anchor position survey timing (hrs)
		 prepAA(0),//prep inshore assembly area timing (hrs)
		 prepSpar(0),//prep spare for tow out to assembly area timing (hrs)
		 upendSpar(0),//upend and ballast the spar timing (hrs)
		 prepSemi(0),//prep semisubmersible for turbine install timing (hrs)
		 turbFasten(0),//fasten turbine for transport timing (hrs)
		 boltTower(0),// bolt tower to substructure timing (hrs)
		 boltNacelle1(0),//bolt nacelle to tower timing individual components method (hrs)
		 boltNacelle2(0),//bolt nacelle to tower timing bunny ears method (hrs)
		 boltNacelle3(0),//bolt nacelle to tower timing assembled rotor method (hrs)
		 boltBlade1(0),//bolt blade to rotor timing individual components method (hrs)
		 boltBlade2(0),//bolt blade to rotor timing bunny ears method (hrs)
		 boltRotor(0),//bolt rotor to nacelle timing assembled rotor method (hrs)
		 vesselPosTurb(0),//vessel positioning timing turbine install (hrs)
		 vesselPosJack(0),//vessel positioning timing jacket install (hrs)
		 vesselPosMono(0),//vessel positioning timing monopile install (hrs)
		 subsVessPos(0),//vessel positioning timing offshore substation install (hrs)
		 monoFasten(0),//fasten monopile for transport timing (hrs)
		 jackFasten(0),//fasten jacket for transport timing (hrs)
		 prepGripperMono(0),//prepare pile gripper and upender timing monopile install (hrs)
		 prepGripperJack(0),//prepare pile gripper and upender timing iacket install (hrs)
		 placePiles(0),//lift and place jacket piles timing (hrs)
		 prepHamMono(0),//prepare pile hammer timing monopile install (hrs)
		 removeHamMono(0),//remove hammer timing monopile install (hrs)
		 prepHamJack(0),//prepare pile hammer timing iacket install (hrs)
		 removeHamJack(0),//remove hammer timing iacket install (hrs)
		 placeJack(0),//place  jacket timing (hrs)
		 levJack(0),//level jacket timing (hrs)
		 placeTemplate(0),//place jacket template timing (hrs)
		 hamRate(0),//pile hammer rate (m/hr)
		 placeMP(0),//place monopile pile timing (hrs)
		 instScour(0),//install scour protection (hrs)
		 placeTP(0),//place transition piece on monopile timing (hrs)
		 groutTP(0),//grout transition piece (hrs)
		 tpCover(0),//install transition piece cover timing (hrs)
		 prepTow(0),//prep floating substructure for towing timing (hrs)
		 spMoorCon(0),//connect spar to mooring system timing (hrs)
		 ssMoorCon(0),//connect semisubmersible to mooring system (hrs)
		 spMoorCheck(0),//check mooring connections to spar timing (hrs)
		 ssMoorCheck(0),//check mooring connections to semisubmersible timing (hrs)
		 ssBall(0),//ballast semisubmersible timing (hrs)
		 surfLayRate(0),//electrical cable surface lay rate (m/hr)
		 cabPullIn(0),//array cable pull in to interfaces timing (hrs)
		 cabTerm(0),//cable termination and testing timing (hrs)
		 cabLoadout(0),//array cable loadout timing (hrs)
		 buryRate(0),//cable bury rate (m/hr)
		 subsPullIn(0),//cable pull in to substation timing (hrs)
		 shorePullIn(0),//cable pull in to shore timing (hrs)
		 landConstruct(0),//land construction of required onshore electrical systems timing (days)
		 expCabLoad(0),//export cable loadout timing (hrs)
		 subsLoad(0),//substation loadout timing (hrs)
		 placeTop(0),//lift and place substation topside timing (hrs)
		 pileSpreadDR(0),//piling equipment spread day rate ($/day)
		 pileSpreadMob(0),//piling equipment spread mobilization/demobilization cost ($)
		 groutSpreadDR(0),//grouting equipment spread day rate ($/day)
		 groutSpreadMob(0),//grouting equipment spread mobilization/demobilization cost ($)
		 seaSpreadDR(0),//suction pile anchor vessel and equipment spread day rate ($/day)
		 seaSpreadMob(0),//suction pile anchor vessel and equipment spread mobilization/demobilization cost ($)
		 compRacks(0),//component racks cost ($)
		 cabSurveyCR(0),//cost rate of surveying and verifying electrical cable installation ($/)
		 cabDrillDist(0),//horizontal drilling distance for cable landfall (m)
		 cabDrillCR(0),//horizontal drilling cost rate ($/m)
		 mpvRentalDR(0),//MPV rental day rate ($/day)
		 diveTeamDR(0),//cable landfall dive team day rate ($/day)
		 winchDR(0),//Cable winch day rate
		 civilWork(0),//civil construction work cost ($)
		 elecWork(0),//electrical work cost ($)
		 nCrane600(0),
		 nCrane1000(0),
		 crane600DR(0),//600 tonne capacity crawler crane day rate ($/day)
		 crane1000DR(0),//1000 tonne capacity crawler crane day rate ($/day)
		 craneMobDemob(0),//crane mobilization and demobilization cost ($)
		 entranceExitRate(0),//port entrance and exit cost ($/m^2/occurrence)
		 dockRate(0),//port docking cost ($/day)
		 wharfRate(0),//port wharf loading and unloading cost ($/tonne)
		 laydownCR(0),//port laydown and storage cost ($/m/day)
		 estEnMFac(0),//estimated engineering and management cost factor
		 preFEEDStudy(0),//pre-fornt end engineering design (FEED) study cost ($)
		 feedStudy(0),// FEED study cost ($)
		 stateLease(0),//state leasing cost ($)
		 outConShelfLease(0),//outer continental shelf lease cost ($)
		 saPlan(0),//site assessment plan cost ($)
		 conOpPlan(0),//construction operations plan cost ($)
		 nepaEisMet(0),//national environmental protection agency (NEPA) environmental impact (EIS) meteorological (met) tower study cost ($)
		 physResStudyMet(0),//physical resource met tower study cost ($)
		 bioResStudyMet(0),//biological resource met tower study ($)
		 socEconStudyMet(0),//socioeconomic met tower study cost ($)
		 navStudyMet(0),//navigation met tower study ($)
		 nepaEisProj(0),// NEPA EIS project site study cost ($)
		 physResStudyProj(0),//physical resource project site study cost ($)
		 bioResStudyProj(0),//biological resource project site study cost ($)
		 socEconStudyProj(0),//socioeconomic project site study cost ($)
		 navStudyProj(0),//navigation project site study cost ($)
		 coastZoneManAct(0),//coastal zone management act compliance cost ($)
		 rivsnHarbsAct(0),//rivers & harbors act section 10 compliance cost ($)
		 cleanWatAct402(0),//clean water act section 402 compliance cost ($)
		 cleanWatAct404(0),//clean water act section 404 compliance cost ($)
		 faaPlan(0),//federal aviation administration (FAA) plans and mitigation cost ($)
		 endSpecAct(0),//endangered species act compliance cost ($)
		 marMamProtAct(0),//marine mammal protection act compliance cost ($)
		 migBirdAct(0),//migratory bird act compliance ($)
		 natHisPresAct(0),//national historic preservation act compliance cost ($)
		 addLocPerm(0),//additional local and state permissions and compliance cost ($)
		 metTowCR(0),//meteorological tower fabrication, design, and install cost rate ($/MW)
		 decomDiscRate(0),//decommissioning expense discount rate
		 hubD(0),
		 bladeL(0),
		 chord(0),
		 nacelleW(0),
		 nacelleL(0),
		 rnaM(0),
		 towerD(0),
		 towerM(0),
		 construction_insurance_cost(0),
		 total_contingency_cost(0),
		 construction_finance_cost(0),
		 construction_finance_factor(0),
		 soft_costs(0),
		 mpileM(0),
		 mtransM(0),
		 mPileCost(0),
		 mTransCost(0),
		 jlatticeM(0),
		 jtransM(0),
		 jpileM(0),
		 jLatticeCost(0),
		 jTransCost(0),
		 jPileCost(0),
		 spStifColM(0),
		 spTapColM(0),
		 spStifColCost(0),
		 spTapColCost(0),
		 ballM(0),
		 ballCost(0),
		 ssStifColM(0),
		 ssTrussM(0),
		 ssHeaveM(0),
		 ssStifColCost(0),
		 ssTrussCost(0),
		 ssHeaveCost(0),
		 moorSysCost(0),
		 sSteelM(0),
		 sSteelCost(0),
		 subTotM(0),
		 systAngle(0),
		 freeCabLeng(0),
		 fixCabLeng(0),
		 nExpCab(0),
		 nSubstation(0),
		 fullStrings(0),
		 nTurbPS(0),
		 nTurbCab1(0),
		 nTurbCab2(0),
		 nTurbInter1(0),
		 nTurbInter2(0),
		 nSubsInter(0),
		 cab1Leng(0),
		 cab2Leng(0),
		 expCabLeng(0),
		 nMPT(0),
		 mptRating(0),
		 mptCost(0),
		 subsTopM(0),
		 subsTopCost(0),
		 arrCab1Cost(0),
		 arrCab2Cost(0),
		 expCabCost(0),
		 shuntReactors(0),
		 switchGear(0),
		 ancillarySys(0),
		 subsSubM(0),
		 subsPileM(0),
		 subsLandAssembly(0),
		 subsSubCost(0),
		 switchYard(0),
		 onShoreSubs(0),
		 onshoreMisc(0),
		 transLine(0),
		 subCabCost(0),
		 offSubsCost(0),
		 onshoreTransCost(0),
		 moorTime(0),
		 floatPrepTime(0),
		 turbDeckArea(0),
		 nTurbPerTrip(0),
		 turbInstTime(0),
		 subDeckArea(0),
		 nSubPerTrip(0),
		 subInstTime(0),
		 cab1SecM(0),
		 cab2SecM(0),
		 cab1SecPerTrip(0),
		 cab2SecPerTrip(0),
		 arrInstTime(0),
		 expCabSecM(0),
		 expCabSecPerTrip(0),
		 expInstTime(0),
		 subsInstTime(0),
		 totInstTime(0),
		 cabSurvey(0),
		 array_cable_install_cost(0),
		 export_cable_install_cost(0),
		 substation_install_cost(0),
		 turbine_install_cost(0),
		 substructure_install_cost(0),
		 electrical_install_cost(0),
		 mob_demob_cost(0),
		 entrExitCost(0),
		 wharfCost(0),
		 dockCost(0),
		 subLaydownA(0),
		 subLayCost(0),
		 turbLaydownA(0),
		 turbLayCost(0),
		 craneCost(0),
		 totPortCost(0),
		 totStageCost(0),
		 feedCost(0),
		 permStudyComp(0),
		 metFabCost(0),
		 decomCost(0),
		 subTotCost(0),
		 totElecCost(0),
		 totAnICost(0),
		 totPnSCost(0),
		 totEnMCost(0),
		 totDevCost(0),
		 commissioning(0),
		 total_bos_cost(0)
	{}
};
extern wobos wobos_obj;
extern wobos *wobos_cm;
#endif
