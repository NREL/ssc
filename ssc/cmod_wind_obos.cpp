#include "core.h"
#include "lib_wind_obos.h"

//#include <iostream>
//#include <cmath>
//#include <math.h>
//#include <vector>
//#include <algorithm>
//#include <map>
//#include <string>
//#include <array>
//#include <fstream>

//using namespace std;

static var_info _cm_vtab_wind_obos[] = {
/*  VARTYPE           DATATYPE         NAME                              LABEL                                                      UNITS                 META                      GROUP          REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/

//Main inputs
   { SSC_INPUT,        SSC_NUMBER,      "nTurb",                          "Number of Turbines",                                       "",                   "",                       "wobos",            "?=20",                    "MIN=2,Max=200",                 ""},
   { SSC_INPUT,        SSC_NUMBER,      "turbR",                          "Turbine Rating",                                           "MW",                 "",                       "wobos",            "?=5",                     "MIN=1,MAX=10",                  ""},
   { SSC_INPUT,        SSC_NUMBER,      "rotorD",                         "Rotor Diameter",                                           "m",                  "",                       "wobos",            "?=120",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "hubH",                           "Hub Height",                                               "m",                  "",                       "wobos",            "?=90",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "waterD",                         "Max Water Depth",                                          "m",                  "",                       "wobos",            "?=30",                    "MIN=3,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "distShore",                      "Distance to Landfall",                                     "km",                 "",                       "wobos",            "?=90",                    "MIN=5,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "distPort",                       "Distance from Installation Port to Site",                  "km",                 "",                       "wobos",            "?=90",                    "MIN=5,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "distPtoA",                       "Distance from Installation Port to Inshore Assembly Area", "km",                 "",                       "wobos",            "?=90",                    "MIN=5,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "distAtoS",                       "Distance form Inshore Assembly Area to Site",              "km",                 "",                       "wobos",            "?=90",                    "MIN=5,MAX=1000",                ""},
   { SSC_INPUT,        SSC_NUMBER,      "substructure",                   "Substructure Type",                                        "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "anchor",                         "Anchor Type",                                              "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "turbInstallMethod",              "Turbine Installation Method",                              "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "towerInstallMethod",             "Tower Installation Method",                                "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "installStrategy",                "Installation Vessel Strategy",                             "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "cableOptimizer",                 "Electrical Cable Cost Optimization",                       "",                   "",                       "wobos",            "?=0",                     "INTEGER",                       ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorLines",                      "Number Of Mooring Lines",                                  "",                   "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "buryDepth",                      "Electrical Cable Burial Depth"                             "m",                  "",                       "wobos",            "?=2",                     "MIN=0,MAX=15",                  ""},
   { SSC_INPUT,        SSC_NUMBER,      "arrayY",                         "Spacing Between Turbines in Rows",                         "rotor diameters",    "",                       "wobos",            "?=9",                     "MIN=1",                         ""},
   { SSC_INPUT,        SSC_NUMBER,      "arrayX",                         "Spacing Between Turbine Rows",                             "rotor diameters",    "",                       "wobos",            "?=9",                     "MIN=1",                         ""},
   { SSC_INPUT,        SSC_ARRAY,       "arrVoltage",                     "Array Cable System Voltage",                              "kV",                 "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "expVoltage",                     "Export Cable System Voltage",                              "kV",                 "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "substructCon",                   "Substructure Install Weather Contingency",                 "%",                  "",                       "wobos",            "?=0.3",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "turbCont",                       "Turbine Install Weather Contingency",                      "%",                  "",                       "wobos",            "?=0.3",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "elecCont",                       "Electrical Install Weather Contingency",                   "%",                  "",                       "wobos",            "?=0.3",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "interConVolt",                   "Grid Interconnect Voltage",                                "kV",                 "",                       "wobos",            "?=345",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "distInterCon",                   "Distance Over Land to Grid Interconnect",                  "miles",              "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "scrapVal",                       "Total Scrap Value of Decommissioned Components",           "$",                  "",                       "wobos",            "?=0",                     "",                              ""},

//General
   { SSC_INPUT,        SSC_NUMBER,      "projLife",                       "Project Economic Life",                                    "years",              "",                       "wobos",            "?=20",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "inspectClear",                   "Inspection Clearance",                                     "m",                  "",                       "wobos",            "?=20",                    "",                              ""},

//Substructure & Foundation
   { SSC_INPUT,        SSC_NUMBER,      "mpileCR",                        "Monopile Cost Rate",                                       "$/tonne",            "",                       "wobos",            "?=2250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mtransCR",                       "Monopile Transition Piece Cost Rate",                      "$/tonne",            "",                       "wobos",            "?=3230",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mpileD",                         "Monopile Diameter",                                        "m",                  "",                       "wobos",            "",                        "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mpileL",                         "Monopile Length",                                          "m",                  "",                       "wobos",            "",                        "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mpEmbedL",                       "Monopile Embedment Length",                                "m",                  "",                       "wobos",            "?=30",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jlatticeCR"                      "Jacket Main Lattice Cost Rate",                            "$/tonne",            "",                       "wobos",            "?=4680",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jtransCR",                       "Jacket Transition Piece Cost Rate",                        "$/tonne",            "",                       "wobos",            "?=4500",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jpileCR",                        "Jacket Pile Cost Rate",                                    "$/tonne",            "",                       "wobos",            "?=2250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jlatticeA",                      "Jacket Main Lattice Footprint Area",                       "m^2"                 "",                       "wobos",            "?=26",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jpileL",                         "Jacket Pile Length",                                       "m",                  "",                       "wobos",            "?=47.5",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jpileD",                         "Jacket Pile Diameter",                                     "m",                  "",                       "wobos",            "?=1.6",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "spStifColCR",                    "Spar Stiffened Column Cost Rate",                          "$/tonne",            "",                       "wobos",            "?=3120",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "spTapColCR",                     "Spar Tapered Column Cost Rate",                            "$/tonne",            "",                       "wobos",            "?=4220",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ballCR",                         "Floating Ballast Cost Rate",                               "$/tonne",            "",                       "wobos",            "?=100",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "deaFixLeng",                     "Fixed Mooring Length for Drag Embedment Anchors",          "m",                  "",                       "wobos",            "?=500",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssStifColCR",                    "Semi-submersible Stiffened Column Cost Rate",              "$/tonne",            "",                       "wobos",            "?=3120",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssTrussCR",                      "Semi-submersible Truss Cost Rate",                         "$/tonne",            "",                       "wobos",            "?=6250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssHeaveCR",                      "Semi-submersible Heave Plate Cost Rate",                   "$/tonne",            "",                       "wobos",            "?=6250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "sSteelCR",                       "Secondary/Outfitting Steel Cost Rate",                     "$/tonne",            "",                       "wobos",            "?=7250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorDia",                        "Mooring Line Diameter",                                    "m",                  "",                       "wobos",            "",                        "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorCR",                         "Mooring Line Cost Rate",                                   "$/m",                "",                       "wobos",            "",                        "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "scourMat",                       "Scour Protection Material Cost",                           "$/location"          "",                       "wobos",            "?=250000",                "",                              ""},

//Electrical Infrastructure
   { SSC_INPUT,        SSC_NUMBER,      "pwrFac",                         "Power Transfer Efficiency Factor",                         "%",                  "",                       "wobos",            "?=0.95",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "buryFac",                        "Cable Burial Depth Factor",                                "%/m",                "",                       "wobos",            "?=0.1",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "catLengFac",                     "Catenary Cable Length Factor",                             "%",                  "",                       "wobos",            "?=0.04",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "exCabFac",                       "Excess Cable Factor",                                      "%",                  "",                       "wobos",            "?=0.1",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsTobFab",                     "Offshore Substation Fabrication Cost",                     "$/tonne",            "",                       "wobos",            "?=14500",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsTopDes",                     "Offshore Substation Design Cost",                          "$",                  "",                       "wobos",            "?=4500000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "topAssemblyFac",                 "Offshore Substation Land-based Assembly Factor",           "%",                  "",                       "wobos",            "?=0.08",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsJacketCR",                   "Offshore Substation Jacket Lattice Cost Rate",             "$/tonne",            "",                       "wobos",            "?=6250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsPileCR",                     "Offshore Substation Jacket Pile Cost Rate",                "$/tonne",            "",                       "wobos",            "?=2250",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "dynCabFac",                      "Dynamic Cable Cost Premium Factor",                        "",                   "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "shuntCR",                        "Shunt Reactor Cost Rate",                                  "$/MVA",              "",                       "wobos",            "?=35000",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "highVoltSG",                     "High Voltage Switchgear Cost",                             "$",                  "",                       "wobos",            "?=950000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "medVoltSG",                      "Medium Voltage Switchgear Cost",                           "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "backUpGen",                      "Back up Diesel Generator Cost",                            "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "workSpace",                      "Offshore Substation Workspace & Accommodations Cost",      "$",                  "",                       "wobos",            "?=2000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "otherAncillary",                 "Other Ancillary Systems Costs",                            "$",                  "",                       "wobos",            "?=3000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mptCR",                          "Main Power Transformer Cost Rate",                         "$/MVA",              "",                       "wobos",            "?=12500",                 "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "arrCables",                      "Array Cables and Specifications",                          "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "expCables",                      "Export Cables and Specifications",                         "",                   "",                       "wobos",            "*",                       "",                              ""},

//Assembly & Installation
   { SSC_INPUT,        SSC_NUMBER,      "moorTimeFac",                    "Anchor & Mooring Water Depth Time Factor",                 "",                   "",                       "wobos",            "?=0.005",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorLoadout",                    "Anchor & Mooring Loadout Time",                            "hours",              "",                       "wobos",            "?=5",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "moorSurvey",                     "Survey Mooring Lines & Anchor Positions Time",             "hours",              "",                       "wobos",            "?=4",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepAA",                         "Prepare Inshore Assembly Area For Turbine Installation",   "hours",              "",                       "wobos",            "?=168",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepSpar",                       "Prepare Spar for Tow to Inshore Assembly Area",            "hours",              "",                       "wobos",            "?=18",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "upendSpar",                      "Upend and Ballast Spar",                                   "hours",              "",                       "wobos",            "?=36",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepSemi",                       "Prepare Semi-submersible for Turbine Installation",        "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "turbFasten",                     "Prepare and Fasten Turbine for Transport",                 "hours/turbine",      "",                       "wobos",            "?=8",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltTower",                      "Lift and Bolt Tower Section",                              "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltNacelle1",                   "Lift and Bolt Nacelle Individual Components Method",       "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltNacelle2",                   "Lift and Bolt Nacelle Bunny Ears Method",                  "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltNacelle3",                   "Lift and Bolt Nacelle Fully Assembled Rotor Method",       "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltBlade1",                     "Lift and Bolt Blade Individual Components Method",         "hours",              "",                       "wobos",            "?=3.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltBlade2",                     "Lift and Bolt Blade Bunny Ears Method",                    "hours",              "",                       "wobos",            "?=3.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "boltRotor",                      "Lift and Bolt Rotor Fully Assembled Rotor Method",         "hours",              "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "vesselPosTurb",                  "Vessel Positioning Time Turbine Installation",             "hours",              "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "vesselPosJack",                  "Vessel Positioning Time Jacket Installation",              "hours",              "",                       "wobos",            "?=8",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "vesselPosMono",                  "Vessel Positioning Time Monopile Installation",            "hours",              "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsVessPos",                    "Vessel Positioning Time Offshore Substation Installation", "hours",              "",                       "wobos",            "?=6",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "monoFasten",                     "Prepare and Fasten Monopile for Transport",                "hours/unit",         "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "jackFasten",                     "Prepare and Fasten Jacket for Transport",                  "hours/unit",         "",                       "wobos",            "?=20",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepGripperMono",                "Prepare Monopile Gripper and Upender",                     "hours",              "",                       "wobos",            "?=1.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepGripperJack",                "Prepare Jacket Gripper and Upender",                       "hours",              "",                       "wobos",            "?=8",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placePiles",                     "Place Jacket Piles",                                       "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepHamMono",                    "Prepare Hammer for Monopile Installation",                 "hours",              "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prephamJack",                    "Prepare Hammer for jacket Piles Installation",             "hours",              "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "removeHamMono",                  "Remove Hammer for Monopile Installation",                  "hours",              "",                       "wobos",            "?=4",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "removeHamJack",                  "Remove Hammer for Jacket Piles Installation",              "hours",              "",                       "wobos",            "?=4",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeTemplate",                  "Place Jacket Pile Template on Seabed",                     "hours",              "",                       "wobos",            "?=4",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeJack",                      "Place Jacket Main Lattice onto Piles",                     "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "levJack",                        "Level Jacket Main Lattice",                                "hours",              "",                       "wobos",            "?=24",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "hamRate",                        "Pile Hammer Rate",                                         "m/hour",             "",                       "wobos",            "?=20",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeMP",                        "Lift and Place Monopile for Hammering",                    "hours",              "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "instScour",                      "Install Scour Protection Around Monopile Base",            "hours",              "",                       "wobos",            "?=6",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeTP",                        "Place Transition Piece onto Monopile",                     "hours",              "",                       "wobos",            "?=3",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "groutTP",                        "Grout Transition Piece/Monopile Interface",                "hours",              "",                       "wobos",            "?=8",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "tpCover",                        "Install Transition Piece Cover",                           "hours",              "",                       "wobos",            "?=1.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "prepTow",                        "Prepare Floating Substructure for Tow to Site",            "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "spMoorCon",                      "Connect Mooring Lines to Spar",                            "hours",              "",                       "wobos",            "?=20",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssMoorCon",                      "Connect Mooring Lines to Semi-Submersible",                "hours",              "",                       "wobos",            "?=22",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "spMoorCheck",                    "Survey Spar Mooring Lines and Connections",                "hours",              "",                       "wobos",            "?=16",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssMoorCheck",                    "Survey Semi-submersible Mooing Lines and Connections",     "hours",              "",                       "wobos",            "?=12",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "ssBall",                         "Ballast Semi-submersible",                                 "hours",              "",                       "wobos",            "?=6",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "surflayRate",                    "Cable Surface Lay Rate",                                   "m/hour",             "",                       "wobos",            "?=375",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabPullIn",                      "Array Cable Pull in to Interfaces",                        "hours",              "",                       "wobos",            "?=5.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabTerm",                        "Cable Termination and Testing",                            "hours",              "",                       "wobos",            "?=5.5",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabLoadout",                     "Array Cable Loadout for Installation",                     "hours",              "",                       "wobos",            "?=14",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "buryRate",                       "Cable Burial Rate"                                         "m/hour",             "",                       "wobos",            "?=125",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsPullIn",                     "Cable Pull in to Offshore Substation",                     "hours",              "",                       "wobos",            "?=48",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "shorePullIn",                    "Cable Pull in to Onshore Infrastructure",                  "hours",              "",                       "wobos",            "?=96",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "landConstruct",                  "Onshore Infrastructure Construction",                      "days",               "",                       "wobos",            "?=7",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "expCabLoad",                     "Export Cable Loadout for Installation",                    "hours",              "",                       "wobos",            "?=24",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "subsLoad",                       "Offshore Substation Loadout for Installation",             "hours",              "",                       "wobos",            "?=60",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "placeTop",                       "Lift and Place Offshore Substation Topside",               "hours",              "",                       "wobos",            "?=24",                    "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nFeederBarge",                   "Number of Feeder Barges (Feeder Barge Strategy)",          "",                   "",                       "wobos",            "?=2",                     "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "pileSpreadDR"                    "Piling Spread Day Rate",                                   "$/day",              "",                       "wobos",            "?=2500",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "pileSpreadMob",                  "Piling Spread Mobilization Cost",                          "$",                  "",                       "wobos",            "?=750000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "groutSpreadDR",                  "Grouting Spread Day Rate",                                 "$/day",              "",                       "wobos",            "?=3000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "groutSpreadMob",                 "Grouting Spread Mobilization Cost",                        "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "seaSpreadDR",                    "Suction Pile Anchor Spread Day Rate",                      "$/day",              "",                       "wobos",            "?=165000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "seaSpreadMob",                   "Suction Pile Anchor Spread Mobilization Cost",             "$",                  "",                       "wobos",            "?=4500000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "compRacks",                      "Component Racks Cost",                                     "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabSurveyCR",                    "Cable Route Survey Cost",                                  "$/m",                "",                       "wobos",            "?=240",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabDrillDist",                   "Horizontal Drilling distance for Cable Landfall",          "m",                  "",                       "wobos",            "?=500",                   "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cabDrillCR",                     "Cost Rate for Horizontal Drilling",                        "$/m",                "",                       "wobos",            "?=3200",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "mpvRentalDR",                    "MPV Rental Day Rate",                                      "$/day",              "",                       "wobos",            "?=72000",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "diveTeamDR",                     "Cable Landfall Dive Team Day Rate",                        "$/day",              "",                       "wobos",            "?=3200",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "winchDR",                        "Cable Landfall Winch Day Rate",                            "$/day",              "",                       "wobos",            "?=1000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "civilWork",                      "Onshore Infrastructure Civil Work Cost",                   "$",                  "",                       "wobos",            "?=40000",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "elecWork",                       "Onshore Infrastructure Electrical Work Cost",              "$",                  "",                       "wobos",            "?=25000",                 "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "turbInstVessel",                 "Turbine Install Vessel Specifications",                    "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "turbFeederBarge",                "Turbine Install Feeder Barge Specifications",              "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "turbSupportVessels",             "Turbine Install Support Vessels",                          "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "subInstVessel",                  "Substructure Install Vessel Specifications",               "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "subFeederBarge",                 "Substructure Install Feeder Barge Specifications",         "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "subSupportVessels",              "Substructure Install Support Vessels",                     "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "arrCabInstVessel",               "Array Cable Install Vessel Specifications",                "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "expCabInstVessel",               "Export Cable Install Vessel Specifications",               "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "substaInstVessel",               "Offshore Substation Install Vessel Specifications",        "",                   "",                       "wobos",            "*",                       "",                              ""},
   { SSC_INPUT,        SSC_ARRAY,       "elecSupportVessels",             "Electrical Infrastructure Install Support Vessels",        "",                   "",                       "wobos",            "*",                       "",                              ""},

//Port & Staging
   { SSC_INPUT,        SSC_NUMBER,      "nCrane600",                      "Number of 600 tonne Crawler Cranes",                       "",                   "",                       "wobos",            "",                        "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nCrane1000",                     "Number of 1000 tonne Crawler Cranes",                      "",                   "",                       "wobos",            "",                        "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "crane600DR",                     "600 tonne Crawler Crane Day Rate",                         "$/day",              "",                       "wobos",            "?=5000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "crane1000DR",                    "1000 tonne Crawler Crane Day Rate",                        "$/day",              "",                       "wobos",            "?=8000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "craneMobDemob",                  "Port Crane Mobilization/Demobilization Cost",              "$",                  "",                       "wobos",            "?=150000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "entranceExitRate",               "Port Entrance and Exit Cost Rate",                         "$/occurrence",       "",                       "wobos",            "?=0.525",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "dockRate",                       "Quayside Docking Cost Rate",                               "$/day",              "",                       "wobos",            "?=3000",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "wharfRate",                      "Wharf Loading and Unloading Cost Rate",                    "$/tonne",            "",                       "wobos",            "?=2.75",                  "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "laydownCR",                      "Laydown and Storage Cost Rate",                            "$/m^2/day",          "",                       "wobos",            "?=0.25",                  "",                              ""},

//Engineering & Management
   { SSC_INPUT,        SSC_NUMBER,      "estEnMFac",                      "Estimated Engineering & Management Cost Factor",           "%",                  "",                       "wobos",            "?=0.04",                  "",                              ""},

//Development
   { SSC_INPUT,        SSC_NUMBER,      "preFEEDStudy",                   "Pre-FEED study Cost",                                      "$",                  "",                       "wobos",            "?=5000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "feedStudy",                      "FEED Study Cost",                                          "$",                  "",                       "wobos",            "?=10000000",              "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "stateLease",                     "State Leasing and Permitting Cost",                        "$",                  "",                       "wobos",            "?=250000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "outConShelfLease",               "Outer Continental Shelf Lease Cost",                       "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "saPlan",                         "Site Assessment Plan Cost",                                "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "conOpPlan",                      "Construction Operations Plan Cost",                        "$",                  "",                       "wobos",            "?=1000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nepaEisMet",                     "NEPA Environmental Impact Statement Met Tower Cost",       "$",                  "",                       "wobos",            "?=2000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "physResStudyMet",                "Physical Resource Study Met Tower Cost",                   "$",                  "",                       "wobos",            "?=1500000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "bioResStudyMet",                 "Biological Resource Study Met Tower Cost",                 "$",                  "",                       "wobos",            "?=1500000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "socEconStudyMet",                "Socioeconomic and Land use Study Met Tower Cost",          "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "navStudyMet",                    "Navigation and Transport Study Met Tower Cost",            "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "nepaEisProj",                    "NEPA Environmental Impact Study Project Cost",             "$",                  "",                       "wobos",            "?=5000000",               "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "physResStudyProj",               "Physical Resource Study Project Cost",                     "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "bioResStudyProj",                "Biological Resource Study Porject Cost",                   "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "socEconStudyProj",               "Socioeconomic and Land use Study Project Cost",            "$",                  "",                       "wobos",            "?=200000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "navStudyProj",                   "Navigation and Transport Study Project Cost",              "$",                  "",                       "wobos",            "?=250000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "coastZoneManAct",                "Coastal Zone Management Act Compliance Cost",              "$",                  "",                       "wobos",            "?=100000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "rivsnHarbsAct",                  "Rivers & Harbors Act, Section 10 Compliance Cost",         "$",                  "",                       "wobos",            "?=100000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cleanWatAct402",                 "Clean Water Act, Section 402 Compliance Cost",             "$",                  "",                       "wobos",            "?=100000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "cleanWatAct404",                 "Clean Water Act, Section 404 Compliance Cost",             "$",                  "",                       "wobos",            "?=100000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "faaPlan",                        "Federal Aviation Administration Plans & Mitigation Cost",  "$",                  "",                       "wobos",            "?=10000",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "endSpecAct",                     "Endangered Species Act Compliance Cost",                   "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "marMamProtAct",                  "Marine Mammal Protection Act Compliance Cost",             "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "migBirdAct",                     "Migratory Bird Treaty Compliance Cost",                    "$",                  "",                       "wobos",            "?=500000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "natHisPresAct",                  "National Historic Preservation Act Compliance Cost",       "$",                  "",                       "wobos",            "?=250000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "addLocPerm",                     "Additional State and Local Permitting Cost",               "$",                  "",                       "wobos",            "?=200000",                "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "metTowCR",                       "Meteorological (Met) Tower Fabrication & Install Cost",    "$/MW",               "",                       "wobos",            "?=11518",                 "",                              ""},
   { SSC_INPUT,        SSC_NUMBER,      "decomDiscRate",                  "Decommissioning Cost Discount Rate",                       "%",                  "",                       "wobos",            "?=0.03",                  "",                              ""},
   
   //OUTPUTS
    //General outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "hubD",                           "Hub Diameter",                                             "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "bladeL",                         "Blade Length",                                             "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "chord",                          "Blade Max Chord",                                          "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nacelleW",                       "Nacelle Width",                                            "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nacelleL",                       "Nacelle Length",                                           "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "rnaM",                           "Rotor-Nacelle Assembly Mass"                               "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "towerD",                         "Tower Diameter",                                           "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "towerM",                         "Tower Mass",                                               "tonne",              "",                       "wobos",            "",                        "",                              ""},

    //Substructure & Foundation outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "mpileM",                         "Monopile Pile Mass",                                       "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mtransM",                        "Monopile Transition Piece Mass",                           "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mPileCost",                      "Monopile Pile Cost",                                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mTransCost",                     "Monopile Transition Piece Cost",                           "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jlatticeM",                      "Jacket Main Lattice Mass",                                 "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jtransM",                        "Jacket Transtion Piece Mass",                              "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jpileM",                         "Jacket Piles Mass (total for 4 piles)",                    "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jLatticeCost",                   "Jacket Main Lattice Cost",                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jTransCost",                     "Jacket Transition Piece Cost",                             "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "jPileCost",                      "Jacket Piles Cost (total for 4 piles)",                    "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "spStifColM",                     "Spar Stiffened Column Mass",                               "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "spTapColM",                      "Spar Tapered Column Mass",                                 "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "spStifColCost",                  "Spar Stiffened Column Cost",                               "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "spTapColCost",                   "Spar Tapered Column Cost",                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ballM",                          "Spar Ballast Mass",                                        "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ballCost",                       "Spar Ballast Cost",                                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssStifColM",                     "Semi-submersible Stiffened Column Mass",                   "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssTrussM",                       "Semi-submersible Truss Mass",                              "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssHeaveM",                       "Semi-submersible Heave Plate Mass",                        "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssStifColCost",                  "Semi-submersible Stiffened Column Cost",                   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssTrussCost",                    "Semi-submersible Truss Cost",                              "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ssHeaveCost",                    "Semi-submersible Heave Plate Cost",                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "moorSysCost",                    "Mooring and Anchor System Cost",                           "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "sSteelM",                        "Secondary/Outfitting Steel Mass",                          "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "sSteelCost",                     "Secondary/Outfitting Steel Cost",                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subTotM",                        "Total Substructure Mass per Turbine",                      "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subTotCost",                     "Substructure & Foundation Total Cost",                     "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Electrical Infrastructure outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "systAngle",                      "Floating System Angle",                                    "degrees",            "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "freeCabLeng",                    "Free Hanging Cable Length",                                "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "fixCabLeng",                     "Fixed Cable Length",                                       "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nExpCab",                        "Number of Export Cables",                                  "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nSubstation",                    "Number Of Substations",                                    "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "fullStrings",                    "Full Array Strings",                                       "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbPS",                        "Number of Turbines per Partial Array String",              "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbCab1",                      "Number of Turbines per Array Cable #1",                    "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbCab2",                      "Number of Turbines per Array Cable #2",                    "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbInter1",                    "Number of Turbine Interfaces per Array Cable #1",          "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbInter2",                    "Number of Turbine Interfaces per Array Cable #2",          "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nSubsInter",                     "Number of Array Cable Substation Interfaces",              "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab1Leng",                       "Array Cable #1 Length",                                    "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab2Leng",                       "Array Cabel #2 Length",                                    "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expCabLeng",                     "Export Cable Length",                                      "m",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nMPT",                           "Number of Main Power Transformers (MPTs)",                 "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mptRating",                      "MPT Rating",                                               "MVA",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "mptCost",                        "MPT Cost",                                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsTopM",                       "Substation Topside Mass",                                  "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsTopCost",                    "Substation Topside Cost",                                  "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "arrCab1Cost",                    "Array Cable #1 and Ancillary Cost",                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "arrCab2Cost",                    "Array Cable #2 and Ancillary Cost",                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expCabCost",                     "Export Cable and Ancillary Cost",                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "shuntReactors",                  "Shunt Reactor Cost",                                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "switchGear",                     "Switchgear Cost",                                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "ancillarySys",                   "Offshore Substation Ancillary Systems Cost",               "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsSubM",                       "Offshore Substation Substructure Mass",                    "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsPileM",                      "Offshore Substation Jacket Piles Mass",                    "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsLandAssembly",               "Offshore Substation Land-based Assembly Cost",             "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsSubCost",                    "Offshore Substation Substructure Cost,"                    "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "switchYard",                     "Onshore Switch Yard Cost",                                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "onShoreSubs",                    "Onshore Substation Cost",                                  "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "onshoreMisc",                    "Onshore Misc. Costs",                                      "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "transLine",                      "Overhead Transmission Line Cost",                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subCabCost",                     "Total Subsea Cable and Ancillary Cost",                    "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "offSubsCost",                    "Total Offshore Substation Cost",                           "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "onShoreTransCost",               "Total Onshore Transmission System Cost",                   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totElecCost",                    "Total Electrical Infrastructure Cost"                      "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Assembly & Installation outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "moorTime",                       "Mooring and Anchor System Installation Time",              "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbDeckArea",                   "Deck Area Required per Turbine",                           "m^2",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nTurbPerTrip",                   "Maximum Number of Turbines per Vessel Trip",               "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbInstTime",                   "Turbine Installation Time",                                "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subDeckArea",                    "Deck Area Required per Substructure",                      "m^2",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "nSubPerTrip",                    "Maximum Number of Substructures per Vessel Trip",          "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subInstTime",                    "Substructure Installation Time",                           "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab1SecM",                       "Array Cable #1 Section Mass",                              "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab2SecM",                       "Array Cable #2 Section Mass",                              "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab1SecPerTrip",                 "Array Cable #1 Sections per Vessel Trip",                  "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "cab2SecPerTrip",                 "Array Cable #2 Sections per Vessel Trip",                  "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "arrInstTime",                    "Array Cable System Installation Time",                     "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expCabSecM",                     "Export Cable Section Mass",                                "tonne",              "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expCabSecPerTrip",               "Export Cable Sections per Vessel Trip",                    "",                   "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "expInstTime",                    "Export Cable Installation Time",                           "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subsInstTime",                   "Offshore Substation Installation Time",                    "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totInstTime",                    "Total Installation Time",                                  "days",               "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totAnICost",                     "Total Assembly & Installation Cost",                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_ARRAY,       "turbCostsByVessel",              "Turbine Installation Vessel Costs",                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_ARRAY,       "subCostsByVessel",               "Substructure Installation Vessel Costs",                   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_ARRAY,       "elecCostsByVessel",              "Electrical Infrastructure Installation Vessel Costs",      "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_ARRAY,       "mobDemobCostByVessel",           "Vessel Mobilization and Demobilization Cost",              "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Port & Staging outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "entrExitCost",                   "Port Entrance and Exit Cost",                              "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "wharfCost",                      "Port Wharf Cost",                                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "dockCost",                       "Port Docking Cost",                                        "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subLaydownA",                    "Substructure Laydown and Storage Area",                    "m^2",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "subLayCost",                     "Substructure Laydown and Storage Cost",                    "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbLaydownA",                   "Turbine Laydown and Storage Area",                         "m^2",                "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "turbLayCost",                    "Turbine Laydown and Storage Cost",                         "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "craneCost",                      "Port Craneage Cost",                                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totPortCost",                    "Total Port Cost",                                          "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totStageCost",                   "Total Staging Cost",                                       "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totPnSCost",                     "Total Port & Staging Cost",                                "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Engineering & Management outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "totEnMCost",                     "Total Engineering & Management Cost",                      "$",                  "",                       "wobos",            "",                        "",                              ""},

    //Development outputs
   {SSC_OUTPUT,        SSC_NUMBER,      "feedCost",                       "Front End Engineering Design (FEED) Cost",                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "permStudyComp",                  "Permitting, Studies, and Compliance Cost",                 "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "metFabCost",                     "Meteorological Tower Fabrication and Installation Cost",   "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "decomCost",                      "Decommissioning Expense",                                  "$",                  "",                       "wobos",            "",                        "",                              ""},
   {SSC_OUTPUT,        SSC_NUMBER,      "totDevCost",                     "Total Development Cost",                                   "$",                  "",                       "wobos",            "",                        "",                              ""},

   var_info_invalid};

class cm_wind_obos : public compute_module
{
public:
	cm_wind_obos()
	{
		add_var_info(_cm_vtab_wind_obos);
	}

	// BEGIN WRAPPER OF LIB_OBOS 
	// **********************************************************************

	void exec() throw(general_error)
	{
		//Create an instance of the wobos class********************************************************************************************************
		wobos obos;

		//Assign inputs***********************************************************************************************************************************
		//B asic inputs
		obos.rotorD = (double)as_number("rotorD");//rotor diameter (m)
		obos.turbR = (double)as_number("turbR");//turbine rating (MW)
		obos.hubH = (double)as_number("hubH");//hub height (m)
		obos.waterD = (double)as_number("waterD");// water depth (m)
		obos.distShore = (double)as_number("distShore");//distance to shore from install site (km)
		obos.distPort = (double)as_number("distPort");//distance to install site from install port (km)
		obos.distPtoA = (double)as_number("distPtoA");//distance from install port to inshore assembly area (km) (spar only)
		obos.distAtoS = (double)as_number("distAtoS");//distance from inshore assembly area to install site (km) (spar Only)
		obos.moorLines = (double)as_number("moorLines");//number of mooring lines for floating substructures
		obos.buryDepth = (double)as_number("buryDepth");//array and export cable burial depth (m)
		obos.arrayY = (double)as_number("arrayY");//turbine array spacing between turbines on same row (rotor diameters)
		obos.arrayX = (double)as_number("arrayX");// turbine array spacing between turbine rows (rotor diameters)
		obos.substructCont = (double)as_number("substructCont");//substructure install weather contingency
		obos.turbCont = (double)as_number("turbCont");//turbine install weather contingency
		obos.elecCont = (double)as_number("elecCont");//turbine install weather contingency
		obos.interConVolt = (double)as_number("interConVolt");//grid interconnect voltage (kV)
		obos.distInterCon = (double)as_number("distInterCon");//distance from onshore substation to grid interconnect (miles)
		obos.scrapVal = (double)as_number("scrapVal");//scrap value of decommissioned components ($)

		//detailed inputs
		//General
		obos.projLife = (double)as_number("projLife");//economic lifetime of the project (years)
		obos.inspectClear = (double)as_number("inspectClear");//inspection clearance for substructure and turbine components (m)

		//Substructure & Foundation
		obos.mpileCR = (double)as_number("mpileCR");//monopile pile cost rate ($/tonne)
		obos.mtransCR = (double)as_number("mtransCR");//monopile transition piece cost rate ($/tonne)
		obos.jlatticeCR = (double)as_number("jlatticeCR");//jacket lattice cost rate ($/tonne)
		obos.jtransCR = (double)as_number("jtransCR");//jacket transition piece cost rate ($/tonne)
		obos.jpileCR = (double)as_number("jpileCR");//jacket pile cost rate ($/tonne)
		obos.jlatticeA = (double)as_number("jlatticeA");//jacket lattice footprint area
		obos.jpileL = (double)as_number("jpileL");//jacket pile length
		obos.jpileD = (double)as_number("jpileD");//jacket pile diameter
		obos.spStifColCR = (double)as_number("spStifColCR");//spar stiffened column cost rate ($/tonne)
		obos.spTapColCR = (double)as_number("spTapColCR");//spar tapered column cost rate ($/tonne)
		obos.ballCR = (double)as_number("ballCR");//ballast cost rate ($/tonne)
		obos.deaFixLeng = (double)as_number("deaFixLeng");//drag embedment anchor fixed mooring line length
		obos.ssStifColCR = (double)as_number("ssStifColCR");//semisubmersible stiffened column cost rate ($/tonne)
		obos.ssTrussCR = (double)as_number("ssTrussCR");// semisubmersible truss cost rate ($/tonne)
		obos.ssHeaveCR = (double)as_number("ssHeaveCR");//semisubmersible heave plate cost rate ($/tonne)
		obos.sSteelCR = (double)as_number("sSteelCR");//secondary steel cost rate ($/tonne)
		obos.mpEmbedL = (double)as_number("mpEmbedL");//monopile embedment length (m)
		obos.scourMat = (double)as_number("scourMat");

		//Electrical Infrastructure
		obos.pwrFac = (double)as_number("pwrFac");//power factor to estimate losses
		obos.buryFac = (double)as_number("buryFac");//cable burial factor
		obos.catLengFac = (double)as_number("catLengFac");//free hanging or catenary cable length factor
		obos.exCabFac = (double)as_number("exCabFac");// excess cable factor
		obos.subsTopFab = (double)as_number("subsTopFab");//substation topside fabrication cost ($/tonne)
		obos.subsTopDes = (double)as_number("subsTopDes");//substation topside design cost ($)
		obos.topAssemblyFac = (double)as_number("topAssemblyFac");//land based substation topside assembly factor
		obos.subsJackCR = (double)as_number("subsJackCR");//substation jacket substructure cost rate ($/tonne)
		obos.subsPileCR = (double)as_number("subsPileCR");//substation jacket pile cost rate ($/tonne)
		obos.dynCabFac = (double)as_number("dynCabFac");//dynamic/free hanging cable cost premium
		obos.shuntCR = (double)as_number("shuntCR");//shunt reactor cost rate ($/MVA)
		obos.highVoltSG = (double)as_number("highVoltSG");//high voltage switchgear cost ($)
		obos.medVoltSG = (double)as_number("medVoltSG");//medium voltage switchgear cost ($)
		obos.backUpGen = (double)as_number("backUpGen");//back up generator cost ($)
		obos.workSpace = (double)as_number("workSpace");//substation workshop and accommodations cost ($)
		obos.otherAncillary = (double)as_number("otherAncillary");//substation other ancillary costs ($)
		obos.mptCR = (double)as_number("mptCR");//main power transformer cost rate ($/MVA)

		//Assembly & Installation
		obos.moorTimeFac = (double)as_number("moorTimeFac");//mooring installation timing factor (hrs/m)
		obos.moorLoadout = (double)as_number("moorLoadout");//mooring system loadout timing (hrs)
		obos.moorSurvey = (double)as_number("moorSurvey");//mooring system anchor position survey timing (hrs)
		obos.prepAA = (double)as_number("prepAA");//prep inshore assembly area timing (hrs)
		obos.prepSpar = (double)as_number("prepSpar");//prep spare for tow out to assembly area timing (hrs)
		obos.upendSpar = (double)as_number("upendSpar");//upend and ballast the spar timing (hrs)
		obos.prepSemi = (double)as_number("prepSemi");//prep semisubmersible for turbine install timing (hrs)
		obos.turbFasten = (double)as_number("turbFasten");//fasten turbine for transport timing (hrs)
		obos.boltTower = (double)as_number("boltTower");// bolt tower to substructure timing (hrs)
		obos.boltNacelle1 = (double)as_number("boltNacelle1");//bolt nacelle to tower timing individual components method (hrs)
		obos.boltNacelle2 = (double)as_number("boltNacelle2");//bolt nacelle to tower timing bunny ears method (hrs)
		obos.boltNacelle3 = (double)as_number("boltNacelle3");//bolt nacelle to tower timing assembled rotor method (hrs)
		obos.boltBlade1 = (double)as_number("boltBlade1");//bolt blade to rotor timing individual components method (hrs)
		obos.boltBlade2 = (double)as_number("boltBlade2");//bolt blade to rotor timing bunny ears method (hrs)
		obos.boltRotor = (double)as_number("boltRotor");//bolt rotor to nacelle timing assembled rotor method (hrs)
		obos.vesselPosTurb = (double)as_number("vesselPosTurb");//vessel positioning timing turbine install (hrs)
		obos.vesselPosJack = (double)as_number("vesselPosJack");//vessel positioning timing jacket install (hrs)
		obos.vesselPosMono = (double)as_number("vesselPosMono");//vessel positioning timing monopile install (hrs)
		obos.subsVessPos = (double)as_number("subsVessPos");//vessel positioning timing offshore substation install (hrs)
		obos.monoFasten = (double)as_number("monoFasten");//fasten monopile for transport timing (hrs)
		obos.jackFasten = (double)as_number("jackFasten");//fasten jacket for transport timing (hrs)
		obos.prepGripperMono = (double)as_number("prepGripperMono");//prepare pile gripper and upender timing monopile install (hrs)
		obos.prepGripperJack = (double)as_number("prepGripperJack");//prepare pile gripper and upender timing iacket install (hrs)
		obos.placePiles = (double)as_number("placePiles");//lift and place jacket piles timing (hrs)
		obos.prepHamMono = (double)as_number("prepHamMono");//prepare pile hammer timing monopile install (hrs)
		obos.removeHamMono = (double)as_number("removeHamMono");//remove hammer timing monopile install (hrs)
		obos.prepHamJack = (double)as_number("prepHamJack");//prepare pile hammer timing iacket install (hrs)
		obos.removeHamJack = (double)as_number("removeHamJack");//remove hammer timing iacket install (hrs)
		obos.placeJack = (double)as_number("placeJack");//place  jacket timing (hrs)
		obos.levJack = (double)as_number("levJack");//level jacket timing (hrs)
		obos.placeTemplate = (double)as_number("placeTemplate");//place jacket template timing (hrs)
		obos.hamRate = (double)as_number("hamRate");//pile hammer rate (m/hr)
		obos.placeMP = (double)as_number("placeMP");//place monopile pile timing (hrs)
		obos.instScour = (double)as_number("instScour");//install scour protection (hrs)
		obos.placeTP = (double)as_number("placeTP");//place transition piece on monopile timing (hrs)
		obos.groutTP = (double)as_number("groutTP");//grout transition piece (hrs)
		obos.tpCover = (double)as_number("tpCover");//install transition piece cover timing (hrs)
		obos.prepTow = (double)as_number("prepTow");//prep floating substructure for towing timing (hrs)
		obos.spMoorCon = (double)as_number("spMoorCon");//connect spar to mooring system timing (hrs)
		obos.ssMoorCon = (double)as_number("ssMoorCon");//connect semisubmersible to mooring system (hrs)
		obos.spMoorCheck = (double)as_number("spMoorCheck");//check mooring connections to spar timing (hrs)
		obos.ssMoorCheck = (double)as_number("ssMoorCheck");//check mooring connections to semisubmersible timing (hrs)
		obos.ssBall = (double)as_number("ssBall");//ballast semisubmersible timing (hrs)
		obos.surfLayRate = (double)as_number("surfLayRate");//electrical cable surface lay rate (m/hr)
		obos.cabPullIn = (double)as_number("cabPullIn");//array cable pull in to interfaces timing (hrs)
		obos.cabTerm = (double)as_number("cabTerm");//cable termination and testing timing (hrs)
		obos.cabLoadout = (double)as_number("cabLoadout");//array cable loadout timing (hrs)
		obos.buryRate = (double)as_number("buryRate");//cable bury rate (m/hr)
		obos.subsPullIn = (double)as_number("subsPullIn");//cable pull in to substation timing (hrs)
		obos.shorePullIn = (double)as_number("shorePullIn");//cable pull in to shore timing (hrs)
		obos.landConstruct = (double)as_number("landConstruct");//land construction of required onshore electrical systems timing (days)
		obos.expCabLoad = (double)as_number("expCabLoad");//export cable loadout timing (hrs)
		obos.subsLoad = (double)as_number("subsLoad");//substation loadout timing (hrs)
		obos.placeTop = (double)as_number("placeTop");//lift and place substation topside timing (hrs)
		obos.pileSpreadDR = (double)as_number("pileSpreadDR");//piling equipment spread day rate ($/day)
		obos.pileSpreadMob = (double)as_number("pileSpreadMob");//piling equipment spread mobilization/demobilization cost ($)
		obos.groutSpreadDR = (double)as_number("groutSpreadDR");//grouting equipment spread day rate ($/day)
		obos.groutSpreadMob = (double)as_number("groutSpreadMob");//grouting equipment spread mobilization/demobilization cost ($)
		obos.seaSpreadDR = (double)as_number("seaSpreadDR");//suction pile anchor vessel and equipment spread day rate ($/day)
		obos.seaSpreadMob = (double)as_number("seaSpreadMob");//suction pile anchor vessel and equipment spread mobilization/demobilization cost ($)
		obos.compRacks = (double)as_number("compRacks");//component racks cost ($)
		obos.cabSurveyCR = (double)as_number("cabSurveyCR");//cost rate of surveying and verifying electrical cable installation ($/)
		obos.cabDrillDist = (double)as_number("cabDrillDist");
		obos.cabDrillCR = (double)as_number("cabDrillCR");
		obos.mpvRentalDR = (double)as_number("mpvRentalDR");
		obos.diveTeamDR = (double)as_number("diveTeamDR");
		obos.winchDR = (double)as_number("winchDR");
		obos.civilWork = (double)as_number("civilWork");
		obos.elecWork = (double)as_number("elecWork");

		//Port & Staging
		obos.crane600DR = (double)as_number("crane600DR");//600 tonne capacity crawler crane day rate ($/day)
		obos.crane1000DR = (double)as_number("crane1000DR");//1000 tonne capacity crawler crane day rate ($/day)
		obos.craneMobDemob = (double)as_number("craneMobDemob");//crane mobilization and demobilization cost ($)
		obos.entranceExitRate = (double)as_number("entranceExitRate");//port entrance and exit cost ($/m^2/occurrence)
		obos.dockRate = (double)as_number("dockRate");//port docking cost ($/day)
		obos.wharfRate = (double)as_number("wharfRate");//port wharf loading and unloading cost ($/tonne)
		obos.laydownCR = (double)as_number("laydownCR");//port laydown and storage cost ($/m/day)

		//Engineering & Management
		obos.estEnMFac = (double)as_number("estEnMFac");//estimated engineering and management cost factor

		//Development
		obos.preFEEDStudy = (double)as_number("preFEEDStudy");//pre-fornt end engineering design (FEED) study cost ($)
		obos.feedStudy = (double)as_number("feedStudy");// FEED study cost ($)
		obos.stateLease = (double)as_number("stateLease");//state leasing cost ($)
		obos.outConShelfLease = (double)as_number("outConShelfLease");//outer continental shelf lease cost ($)
		obos.saPlan = (double)as_number("saPlan");//site assessment plan cost ($)
		obos.conOpPlan = (double)as_number("conOpPlan");//construction operations plan cost ($)
		obos.nepaEisMet = (double)as_number("nepaEisMet");//national environmental protection agency (NEPA) environmental impact (EIS) meteorological (met) tower study cost ($)
		obos.physResStudyMet = (double)as_number("physResStudyMet");//physical resource met tower study cost ($)
		obos.bioResStudyMet = (double)as_number("bioResStudyMet");//biological resource met tower study ($)
		obos.socEconStudyMet = (double)as_number("socEconStudyMet");//socioeconomic met tower study cost ($)
		obos.navStudyMet = (double)as_number("navStudyMet");//navigation met tower study ($)
		obos.nepaEisProj = (double)as_number("nepaEisProj");// NEPA EIS project site study cost ($)
		obos.physResStudyProj = (double)as_number("physResStudyProj");//physical resource project site study cost ($)
		obos.bioResStudyProj = (double)as_number("bioResStudyProj");//biological resource project site study cost ($)
		obos.socEconStudyProj = (double)as_number("socEconStudyProj");//socioeconomic project site study cost ($)
		obos.navStudyProj = (double)as_number("navStudyProj");//navigation project site study cost ($)
		obos.coastZoneManAct = (double)as_number("coastZoneManAct");//coastal zone management act compliance cost ($)
		obos.rivsnHarbsAct = (double)as_number("rivsnHarbsAct");//rivers & harbors act section 10 compliance cost ($)
		obos.cleanWatAct402 = (double)as_number("cleanWatAct402");//clean water act section 402 compliance cost ($)
		obos.cleanWatAct404 = (double)as_number("cleanWatAct404");//clean water act section 404 compliance cost ($)
		obos.faaPlan = (double)as_number("faaPlan");//federal aviation administration (FAA) plans and mitigation cost ($)
		obos.endSpecAct = (double)as_number("endSpecAct");//endangered species act compliance cost ($)
		obos.marMamProtAct = (double)as_number("marMamProtAct");//marine mammal protection act compliance cost ($)
		obos.migBirdAct = (double)as_number("migBirdAct");//migratory bird act compliance ($)
		obos.natHisPresAct = (double)as_number("natHisPresAct");//national historic preservation act compliance cost ($)
		obos.addLocPerm = (double)as_number("addLocPerm");//additional local and state permissions and compliance cost ($)
		obos.metTowCR = (double)as_number("metTowCR");//meteorological tower fabrication, design, and install cost rate ($/MW)
		obos.decomDiscRate = (double)as_number("decomDiscRate");//decommissioning expense discount rate

		//RUN COMPUTE MODULE***********************************************************************************************************************************
		obos.run();
		
		//Assign outputs***************************************************************************************************************************************
		assign("subTotCost", var_data(obos.subTotCost));

	}
};

DEFINE_MODULE_ENTRY(wind_obos, "Wind Offshore Balance of System cost model", 1)
