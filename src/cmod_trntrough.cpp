#include <math.h>

#include "lib_wfhrly.h"
#include "cmod_trnbase.h"


#ifndef M_PI
#define M_PI 3.1415926
#endif

static var_info _cm_vtab_trntrough[] = {
/*   VARTYPE           DATATYPE         NAME                                  LABEL                                UNITS      META                      GROUP          REQUIRED_IF                  CONSTRAINTS                      UI_HINTS*/

//   (2 variables)
{ SSC_INPUT,	SSC_NUMBER,	"case.financing",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"case.technology",	"",	"",	"",	"CSP Trough Empirical",	"?=3",	"",	"" },

//  Climate (1 variables)
{ SSC_INPUT,	SSC_STRING,	"climate.location",	"Location",	"",	"",	"CSP Trough Empirical",	"?=SAM/HI Kahului.tm2",	"",	"" },

//  Empirical Trough Parasitics (30 variables)
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.antifreeze.const",	"Antifreeze Pumping",	"fraction",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.bop.const",	"Balance of Plant",	"MWe/MWe",	"",	"CSP Trough Empirical",	"?=0.02467",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.bop.f0",	"",	"",	"",	"CSP Trough Empirical",	"?=0.483",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.bop.f1",	"",	"",	"",	"CSP Trough Empirical",	"?=0.517",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.bop.f2",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.bop.partload",	"Balance of Plant PF",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.ct0.const",	"Cooling Towers",	"MWe/MWe",	"",	"CSP Trough Empirical",	"?=0.017045",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.ct0.f0",	"",	"",	"",	"CSP Trough Empirical",	"?=-0.036",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.ct0.f1",	"",	"",	"",	"CSP Trough Empirical",	"?=0.242",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.ct0.f2",	"",	"",	"",	"CSP Trough Empirical",	"?=0.794",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.ct0.partload",	"Cooling Towers PF",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.fixedblock.const",	"Power Block Fixed",	"fraction",	"",	"CSP Trough Empirical",	"?=0.0055",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.hb.const",	"Heater & Boiler",	"MWe/MWe",	"",	"CSP Trough Empirical",	"?=0.02273",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.hb.f0",	"",	"",	"",	"CSP Trough Empirical",	"?=0.483",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.hb.f1",	"",	"",	"",	"CSP Trough Empirical",	"?=0.517",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.hb.f2",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.hb.partload",	"Heater & Boiler PF",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.htfpump.const",	"Solar Field HTF Pumps",	"MWe/m2",	"",	"CSP Trough Empirical",	"?=1.052e-005",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.htfpump.f0",	"",	"",	"",	"CSP Trough Empirical",	"?=-0.036",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.htfpump.f1",	"",	"",	"",	"CSP Trough Empirical",	"?=0.242",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.htfpump.f2",	"",	"",	"",	"CSP Trough Empirical",	"?=0.794",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.htfpump.partload",	"Solar Field HTF Pumps PF",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.operation_mode",	"Cooling Tower Operation Mode",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.sf.const",	"SCA Drives & Electronics",	"MWe/m2",	"",	"CSP Trough Empirical",	"?=2.66e-007",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.sf.partload",	"SCA Drives & Electronics PF",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.tes.const",	"TES Pumps",	"MWe/MWe",	"",	"CSP Trough Empirical",	"?=0.02",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.tes.f0",	"",	"",	"",	"CSP Trough Empirical",	"?=-0.036",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.tes.f1",	"",	"",	"",	"CSP Trough Empirical",	"?=0.242",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.tes.f2",	"",	"",	"",	"CSP Trough Empirical",	"?=0.794",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.par.tes.partload",	"TES Pumps PF",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },

//  Empirical Trough Power Block (24 variables)
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.boiler_lhv_eff",	"Boiler LHV Efficiency",	"",	"",	"CSP Trough Empirical",	"?=0.9",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.ctcf0",	"",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.ctcf1",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.ctcf2",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.ctcf3",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.ctcf4",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.design_gross_output",	"Design Gross Output",	"MWe",	"",	"CSP Trough Empirical",	"?=11",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.effdesign",	"Rated Cycle Conversion Efficiency",	"",	"",	"CSP Trough Empirical",	"?=0.3774",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.gross_net_conversion_factor",	"Estimated Gross to Net Conversion Factor",	"",	"",	"CSP Trough Empirical",	"?=0.88",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.maxoutput",	"Max turbine over design operation*",	"",	"",	"CSP Trough Empirical",	"?=1.05",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.minoutput",	"Min turbine operation*",	"",	"",	"CSP Trough Empirical",	"?=0.25",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.startup_energy",	"Frac of thermal power for startup",	"",	"",	"CSP Trough Empirical",	"?=0.2",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.temp_corr_mode",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tef0",	"",	"",	"",	"CSP Trough Empirical",	"?=-0.037726",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tef1",	"",	"",	"",	"CSP Trough Empirical",	"?=1.0062",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tef2",	"",	"",	"",	"CSP Trough Empirical",	"?=0.076316",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tef3",	"",	"",	"",	"CSP Trough Empirical",	"?=-0.044775",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tef4",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tff0",	"",	"",	"",	"CSP Trough Empirical",	"?=0.03737",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tff1",	"",	"",	"",	"CSP Trough Empirical",	"?=0.98823",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tff2",	"",	"",	"",	"CSP Trough Empirical",	"?=-0.064991",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tff3",	"",	"",	"",	"CSP Trough Empirical",	"?=0.039388",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.pwrb.tpl_tff4",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"csp.tr.pwrb.notify_text",	"",	"",	"",	"CSP Trough Empirical",	"?=empty",	"",	"" },

//  Empirical Trough SCA HCE (66 variables)
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a0_1",	"Heat Loss Coeff A0 Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=4.05",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a0_2",	"Heat Loss Coeff A0 Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=50.8",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a0_3",	"Heat Loss Coeff A0 Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=-9.95",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a0_4",	"Heat Loss Coeff A0 Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=11.8",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a1_1",	"Heat Loss Coeff A1 Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=0.247",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a1_2",	"Heat Loss Coeff A1 Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=0.904",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a1_3",	"Heat Loss Coeff A1 Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=0.465",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a1_4",	"Heat Loss Coeff A1 Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=1.35",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a2_1",	"Heat Loss Coeff A2 Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=-0.00146",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a2_2",	"Heat Loss Coeff A2 Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=0.000579",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a2_3",	"Heat Loss Coeff A2 Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=-0.000854",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a2_4",	"Heat Loss Coeff A2 Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=0.00075",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a3_1",	"Heat Loss Coeff A3 Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=5.65e-006",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a3_2",	"Heat Loss Coeff A3 Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=1.13e-005",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a3_3",	"Heat Loss Coeff A3 Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=1.85e-005",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a3_4",	"Heat Loss Coeff A3 Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=4.07e-006",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a4_1",	"Heat Loss Coeff A4 Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=7.62e-008",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a4_2",	"Heat Loss Coeff A4 Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=1.73e-007",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a4_3",	"Heat Loss Coeff A4 Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=6.89e-007",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a4_4",	"Heat Loss Coeff A4 Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=5.85e-008",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a5_1",	"Heat Loss Coeff A5 Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=-1.7",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a5_2",	"Heat Loss Coeff A5 Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=-43.2",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a5_3",	"Heat Loss Coeff A5 Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=24.7",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a5_4",	"Heat Loss Coeff A5 Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=4.48",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a6_1",	"Heat Loss Coeff A6 Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=0.0125",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a6_2",	"Heat Loss Coeff A6 Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=0.524",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a6_3",	"Heat Loss Coeff A6 Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=3.37",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.a6_4",	"Heat Loss Coeff A6 Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=0.285",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.absorption1",	"Absorption Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=0.96",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.absorption2",	"Absorption Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=0.96",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.absorption3",	"Absorption Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=0.8",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.absorption4",	"Absorption Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=0.96",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.bellows1",	"Bellows Shading Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=0.963",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.bellows2",	"Bellows Shading Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=0.963",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.bellows3",	"Bellows Shading Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=0.963",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.bellows4",	"Bellows Shading Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=0.963",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.envtrans1",	"Env. Transmissivity Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=0.963",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.envtrans2",	"Env. Transmissivity Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=0.963",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.envtrans3",	"Env. Transmissivity Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.envtrans4",	"Env. Transmissivity Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=0.963",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.heatlossf1",	"Heat Loss Factor Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.heatlossf2",	"Heat Loss Factor Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.heatlossf3",	"Heat Loss Factor Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.heatlossf4",	"Heat Loss Factor Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.unaccounted1",	"Unaccounted Rec. 1",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.unaccounted2",	"Unaccounted Rec. 2",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.unaccounted3",	"Unaccounted Rec. 3",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.unaccounted4",	"Unaccounted Rec. 4",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.unit1_frac",	"",	"",	"",	"CSP Trough Empirical",	"?=0.985",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.unit2_frac",	"",	"",	"",	"CSP Trough Empirical",	"?=0.01",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.unit3_frac",	"",	"",	"",	"CSP Trough Empirical",	"?=0.005",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.unit4_frac",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.aperture",	"SCA Aperture",	"m",	"",	"CSP Trough Empirical",	"?=5",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.aperture_area",	"SCA Aperture Area",	"m2",	"",	"CSP Trough Empirical",	"?=470.3",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.availability",	"Solar Field Availability",	"",	"",	"CSP Trough Empirical",	"?=0.99",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.avg_focal_length",	"Average Focal Length",	"m",	"",	"CSP Trough Empirical",	"?=1.8",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.cleanliness",	"Mirror Cleanliness Factor (avg)",	"",	"",	"CSP Trough Empirical",	"?=0.95",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.concentrator_factor",	"Concentrator Factor",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.dust_on_envelope",	"Dust on Envelope (avg)",	"",	"",	"CSP Trough Empirical",	"?=0.98",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.geometric_accuracy",	"Geometric Accuracy",	"",	"",	"CSP Trough Empirical",	"?=0.98",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.iamc1",	"Incident Angle Mod Coeff 1",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.iamc2",	"Incident Angle Mod Coeff 2",	"",	"",	"CSP Trough Empirical",	"?=0.0506",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.iamc3",	"Incident Angle Mod Coeff 3",	"",	"",	"CSP Trough Empirical",	"?=-0.1763",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.length",	"SCA Length",	"m",	"",	"CSP Trough Empirical",	"?=100",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.reflectivity",	"Mirror Reflectance",	"",	"",	"CSP Trough Empirical",	"?=0.935",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.sca.track_twist_error",	"Tracking Error and Twist",	"",	"",	"CSP Trough Empirical",	"?=0.994",	"",	"" },

//  Empirical Trough Solar Field (25 variables)
{ SSC_INPUT,	SSC_MATRIX,	"csp.tr.solf.custom_htf",	"Property table for user-defined HTF",	"",	"",	"CSP Trough Empirical",	"?=1,7,0,0,0,0,0,0,0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.azimuth",	"Collector Azimuth",	"deg",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.deployangle",	"Deploy Angle",	"deg",	"",	"CSP Trough Empirical",	"?=10",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.distrows",	"Row spacing, center-to-center",	"m",	"",	"CSP Trough Empirical",	"?=15",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.distscas",	"Distance between SCAs in Row",	"m",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.dp.ambienttemp",	"Ambient Temp.",	"'C",	"",	"CSP Trough Empirical",	"?=25",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.dp.dniradiation",	"Direct Normal Radiation",	"W/m2",	"",	"CSP Trough Empirical",	"?=950",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.dp.windvelocity",	"Wind Velocity",	"m/s",	"",	"CSP Trough Empirical",	"?=5",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.fieldarea",	"Solar Field Area",	"m2",	"",	"CSP Trough Empirical",	"?=887875",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.fieldhtftype",	"Solar Field HTF Type",	"",	"",	"CSP Trough Empirical",	"?=3",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.htfgallonsperarea",	"HTF Gallons per Area",	"gal/m2",	"",	"CSP Trough Empirical",	"?=0.614",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.htfinittemp",	"Solar Field Initial Temp.",	"'C",	"",	"CSP Trough Empirical",	"?=100",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.htfinlettemp",	"Solar Field Inlet Temp.",	"'C",	"",	"CSP Trough Empirical",	"?=293",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.htfmintemp",	"Minimum HTF Temp.",	"'C",	"",	"CSP Trough Empirical",	"?=50",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.htfoutlettemp",	"Solar Field Outlet Temp.",	"'C",	"",	"CSP Trough Empirical",	"?=391",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.land_overhead_factor",	"Non-Solar Field Land Area Multiplier",	"",	"",	"CSP Trough Empirical",	"?=1.4",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.nscasperloop",	"Number of SCAs per Row",	"",	"",	"CSP Trough Empirical",	"?=4",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.pipingheatlossatdesign",	"Piping Heat Losses @ Design Temp.",	"W/m2",	"",	"CSP Trough Empirical",	"?=10",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.pipingheatlosscoeff1",	"Piping Heat Loss Coeff 1",	"",	"",	"CSP Trough Empirical",	"?=0.001693",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.pipingheatlosscoeff2",	"Piping Heat Loss Coeff 2",	"",	"",	"CSP Trough Empirical",	"?=-1.683e-005",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.pipingheatlosscoeff3",	"Piping Heat Loss Coeff 3",	"",	"",	"CSP Trough Empirical",	"?=6.78e-008",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.solarmultiple",	"Solar Multiple",	"",	"",	"CSP Trough Empirical",	"?=1.7",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.solmultorarea",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.stowangle",	"Stow Angle",	"deg",	"",	"CSP Trough Empirical",	"?=170",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.solf.tilt",	"Collector Tilt",	"deg",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },

//  Empirical Trough Thermal Storage (39 variables)
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.adj_eff",	"Turbine TES Adj. - Efficiency",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.adj_output",	"Turbine TES Adj. - Gross Output",	"",	"",	"CSP Trough Empirical",	"?=0.998",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp1.fossil",	"Fossil Fill Fraction Period 1",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp1.nosolar",	"Dispatch Fraction Without Solar Period 1",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp1.solar",	"Dispatch Fraction With Solar Period 1",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp1.todf",	"TOD Factor Period 1",	"",	"",	"CSP Trough Empirical",	"?=2.064",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp1.turbout",	"Turbine Output Fraction Period 1",	"",	"",	"CSP Trough Empirical",	"?=1.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp2.fossil",	"Fossil Fill Fraction Period 2",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp2.nosolar",	"Dispatch Fraction Without Solar Period 2",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp2.solar",	"Dispatch Fraction With Solar Period 2",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp2.todf",	"TOD Factor Period 2",	"",	"",	"CSP Trough Empirical",	"?=1.2",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp2.turbout",	"Turbine Output Fraction Period 2",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp3.fossil",	"Fossil Fill Fraction Period 3",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp3.nosolar",	"Dispatch Fraction Without Solar Period 3",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp3.solar",	"Dispatch Fraction With Solar Period 3",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp3.todf",	"TOD Factor Period 3",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp3.turbout",	"Turbine Output Fraction Period 3",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp4.fossil",	"Fossil Fill Fraction Period 4",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp4.nosolar",	"Dispatch Fraction Without Solar Period 4",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp4.solar",	"Dispatch Fraction With Solar Period 4",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp4.todf",	"TOD Factor Period 4",	"",	"",	"CSP Trough Empirical",	"?=1.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp4.turbout",	"Turbine Output Fraction Period 4",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp5.fossil",	"Fossil Fill Fraction Period 5",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp5.nosolar",	"Dispatch Fraction Without Solar Period 5",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp5.solar",	"Dispatch Fraction With Solar Period 5",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp5.todf",	"TOD Factor Period 5",	"",	"",	"CSP Trough Empirical",	"?=0.8",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp5.turbout",	"Turbine Output Fraction Period 5",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp6.fossil",	"Fossil Fill Fraction Period 6",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp6.nosolar",	"Dispatch Fraction Without Solar Period 6",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp6.solar",	"Dispatch Fraction With Solar Period 6",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp6.todf",	"TOD Factor Period 6",	"",	"",	"CSP Trough Empirical",	"?=0.7",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp6.turbout",	"Turbine Output Fraction Period 6",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.fluid_type",	"Storage Fluid Type",	"",	"",	"CSP Trough Empirical",	"?=3",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.full_load_hours",	"Equiv. Full Load Hours of TES",	"hours",	"",	"CSP Trough Empirical",	"?=2",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.initial_ts",	"Initial Thermal Storage",	"MWht",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.system_type",	"Storage System Configuration",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.tank_heatloss",	"Tank Heat Losses",	"MWt",	"",	"CSP Trough Empirical",	"?=0.97",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"csp.tr.tes.sched.weekday",	"",	"",	"",	"CSP Trough Empirical",	"?=666666554444444444444555666666554444444444444555666666554444444444444555666666554444444444444555666666554444444444444555333333332222111111222333333333332222111111222333333333332222111111222333333333332222111111222333666666554444444444444555666666554444444444444555666666554444444444444555",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"csp.tr.tes.sched.weekend",	"",	"",	"",	"CSP Trough Empirical",	"?=666666555555555555555555666666555555555555555555666666555555555555555555666666555555555555555555666666555555555555555555333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333666666555555555555555555666666555555555555555555666666555555555555555555",	"",	"" },

//  Trough SCA HCE (4 variables)
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.brokenglass1",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.brokenglass2",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.brokenglass3",	"",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.hce.brokenglass4",	"",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },

//  Trough Storage (21 variables)
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp7.fossil",	"Fossil Fill Fraction Period 7",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp7.nosolar",	"Dispatch Fraction Without Solar Period 7",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp7.solar",	"Dispatch Fraction With Solar Period 7",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp7.todf",	"TOD Factor Period 7",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp7.turbout",	"Turbine Output Fraction Period 7",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp8.fossil",	"Fossil Fill Fraction Period 8",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp8.nosolar",	"Dispatch Fraction Without Solar Period 8",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp8.solar",	"Dispatch Fraction With Solar Period 8",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp8.todf",	"TOD Factor Period 8",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp8.turbout",	"Turbine Output Fraction Period 8",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp9.fossil",	"Fossil Fill Fraction Period 9",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp9.nosolar",	"Dispatch Fraction Without Solar Period 9",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp9.solar",	"Dispatch Fraction With Solar Period 9",	"",	"",	"CSP Trough Empirical",	"?=0.1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp9.todf",	"TOD Factor Period 9",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.disp9.turbout",	"Turbine Output Fraction Period 9",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.tmcline_adj_eff",	"Thermocline Eff. Adj for TES*",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.tmcline_adj_output",	"Thermocline Output Adj for TES*",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.tmcline_bed1",	"Primary Bed Material",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.tmcline_bed2",	"Secondary Bed Material",	"",	"",	"CSP Trough Empirical",	"?=1",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"csp.tr.tes.tmcline_temp_degradation",	"Thermocline Temp Degradation",	"'C",	"",	"CSP Trough Empirical",	"?=50",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"csp.tr.tes.notify_text",	"",	"",	"",	"CSP Trough Empirical",	"?=empty",	"",	"" },

//  User Variables (40 variables)
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.1",	"System Variable 1",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.10",	"System Variable 10",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.2",	"System Variable 2",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.3",	"System Variable 3",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.4",	"System Variable 4",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.5",	"System Variable 5",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.6",	"System Variable 6",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.7",	"System Variable 7",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.8",	"System Variable 8",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"sysudv.9",	"System Variable 9",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.1",	"User Variable 1",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.10",	"User Variable 10",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.2",	"User Variable 2",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.3",	"User Variable 3",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.4",	"User Variable 4",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.5",	"User Variable 5",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.6",	"User Variable 6",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.7",	"User Variable 7",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.8",	"User Variable 8",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_NUMBER,	"uservar.9",	"User Variable 9",	"",	"",	"CSP Trough Empirical",	"?=0",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.1",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.10",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.2",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.3",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.4",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.5",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.6",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.7",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.8",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"sysudv.label.9",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.1",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.10",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.2",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.3",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.4",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.5",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.6",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.7",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.8",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
{ SSC_INPUT,	SSC_STRING,	"uservar.label.9",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },

// 252 total variables

	// outputs
	{ SSC_OUTPUT,        SSC_ARRAY,       "dni",                              "DNI",                                  "kW/m2",  "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "windspd",                          "Wind speed",                           "m/s",    "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "ambtemp",                          "Ambient temp",                         "'C",     "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "sol_rad_inc_on_col",               "Incident solar radiation",             "kWh",    "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "thermal_energy_from_sf",           "Thermal energy from solar field",      "kWh",    "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "thermal_energy_to_powerblock",     "Thermal energy to power block",        "kWh",    "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "gross_electric_output",            "Gross electric output",                "kWh",    "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "e_net",                            "Net electric output",                  "kWh",    "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },
	{ SSC_OUTPUT,        SSC_ARRAY,       "water_flow",                       "Water flow",                           "m3/hr",  "",                      "CSPTrough",       "*",                        "LENGTH=8760",                         "" },

	{ SSC_OUTPUT,        SSC_NUMBER,      "fuel_usage",                       "Annual Fuel Usage (kWht)",             "kWht",   "",                      "CSPTrough",       "*",                        "",                         "" },
	{ SSC_OUTPUT,        SSC_NUMBER,      "gross_to_net_conversion_factor",   "Gross to Net Conv. Factor",            "frac",   "",                      "CSPTrough",       "*",                        "",                         "" },

var_info_invalid };


class cm_trntrough : public cm_trnbase
{
private:
public:
	cm_trntrough() : cm_trnbase()
	{
		add_var_info( _cm_vtab_trntrough );
	}

	virtual void pre_trnsys_call() throw( general_error )
	{
		std::string file;

		file = work_dir() + util::path_separator() + "touperiod.in";
		if (!write_tou_file( "dispatch_weekday", "dispatch_weekend", file.c_str()))
			throw general_error("failed to write tou dispatch input file: " + file);

		file = work_dir() + util::path_separator() + "fluidprop.dat";
		if ( 36 == translate_htf_type( as_integer("htf_type") )
			&& !write_htf_file( "custom_htf", file.c_str()))
			throw general_error("failed to write custom htf data file: " + file);
	}

	virtual void write_include_file( FILE *fp ) throw( general_error )
	{
		std::string annual_output_file = work_dir() + util::path_separator() + "trough.annual.out";
		std::string monthly_output_file = work_dir() + util::path_separator() + "trough.monthly.out";
		std::string hourly_output_file = work_dir() + util::path_separator() + "trough.hourly.out";
		std::string dview_output_file = data_file();
		std::string touperiod_file = work_dir() + util::path_separator() + "touperiod.in";
		std::string eff_array_file = work_dir() + util::path_separator() + "eff_array.dat";
		std::string flux_map_file = work_dir() + util::path_separator() + "flux_map.csv";
		std::string fluid_file = work_dir() + util::path_separator() + "fluidprop.dat";
		std::string warnings_file = work_dir() + util::path_separator() + "warnings.out";
		std::string pb_coeff_file = work_dir() + util::path_separator() + "pb_coeff.in";
		const char *weather_file = as_string("weather_file");


		wf_header hdr;
		if (!wf_read_header( weather_file, &hdr ))
			throw general_error("could not scan weather file header information: " + std::string(weather_file));


		FILE *fout;

		// clear the warnings file
		fout = fopen(warnings_file.c_str(),"w");
		if (fout) fclose(fout);

		// write the eff_array, array_view, and fluxmap files

		// ======= efficiency array file
		fout = fopen(eff_array_file.c_str(), "w");
		if (fout)
		{
			fputs( as_string("optieff_datablob"), fout);
			fclose(fout);
		}

		// ======= flux map file
		fout = fopen(flux_map_file.c_str(), "w");
		if (fout)
		{
			fputs( as_string("fluxmap_datablob"), fout);
			fclose(fout);
		}

		// ========= write actual include file

		// get field dimensions
		size_t nrad = 0, nazm = 0;
		as_matrix("heliostat_field", &nrad, &nazm);

		// write all include file lines

		// for now, create empty PB coeff file
		FILE *fpb = fopen(pb_coeff_file.c_str(), "w");
		if (fpb) fclose(fpb);

		fputs("\n", fp);
		fprintf(fp, "ASSIGN \"%s\" 30\n", weather_file);
		fprintf(fp, "ASSIGN \"%s\" 53\n", touperiod_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 10\n", eff_array_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 60\n", flux_map_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 58\n", annual_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 54\n", hourly_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 61\n", dview_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 57\n", monthly_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 95\n", fluid_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 96\n", warnings_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 65\n", pb_coeff_file.c_str());
		fputs("\n", fp);


		fprintf(fp, "CONSTANTS 2\n");
		fprintf(fp, "TRAKMODE=4\n");
		fprintf(fp, "WFType = %d\n", weather_file_type( weather_file ));

		fputs("\n", fp);
		fprintf(fp, "*********************** HELIOSTAT FIELD PAGE **********************\n");
		fprintf(fp, "CONSTANTS 9\n");
		fprintf(fp, "num_zen = %d\n", as_integer("optieff_zenith"));
		fprintf(fp, "num_azi = %d\n", as_integer("optieff_azimuth"));
		fprintf(fp, "num_helio = %lg\n", as_double("csp.pt.sf.num_heliostats"));
		fprintf(fp, "wind_stow_speed = %lg\n", as_double("wind_stow_speed"));
		fprintf(fp, "h_helio = %lg\n", as_double("heliostat_height"));
		fprintf(fp, "w_helio = %lg\n", as_double("heliostat_width"));
		fprintf(fp, "Hel_dens = %lg\n", as_double("ratio_reflect_to_profile"));
		fprintf(fp, "hel_stow_deploy = %lg\n", as_double("stow_deploy_angle"));
		fprintf(fp, "field_angle = %lg\n", as_double("field_span")/2.0);
		fputs("\n", fp);

		fprintf(fp, "*********************** RECEIVER/Trough FIELD PAGE **********************\n");
		fprintf(fp, "CONSTANTS 26\n");
		fprintf(fp, "is_north = %d\n", as_integer("receiver_type"));
		fprintf(fp, "num_panels = %d\n", as_integer("exter_num_panels"));
		fprintf(fp, "d_rec = %lg\n", as_double("exter_diameter"));

		if (as_integer("receiver_type")==0)
		{
			fprintf(fp, "h_rec = %lg\n", as_double("exter_height")); // EXTERNAL
			fprintf(fp, "Rec_d_spec = 0.0\n");
			fprintf(fp, "H_lip = 0.0\n");
		}
		else
		{
			fprintf(fp, "h_rec = %lg\n", as_double("cavity_panel_height")); // CAVITY
			fprintf(fp, "Rec_d_spec = %lg\n", as_double("cavity_aperture_width"));
			fprintf(fp, "H_lip = %lg\n", as_double("cavity_panel_height")*as_double("cavity_lip_height_ratio"));
		}

		fprintf(fp, "h_trough = %lg\n", as_double("trough_height"));
		fprintf(fp, "d_tube = %lg\n", as_double("tube_outer_diameter"));
		fprintf(fp, "th_tube = %lg\n", as_double("tube_wall_thickness"));
		fprintf(fp, "Material = 2\n"); // fixed to stainless steel
		fprintf(fp, "HTF = %d\n", translate_htf_type(as_integer("htf_type")));
		fprintf(fp, "flow_pattern = %d\n", as_integer("flow_pattern")+1);
		fprintf(fp, "HTF_rec_out = %lg\n", as_double("req_htf_outlet_temp"));
		fprintf(fp, "HTF_max_inlet = %lg\n", as_double("max_temp_to_receiver"));
		fprintf(fp, "Rec_HTF_max_flow = %lg\n", as_double("max_flow_to_receiver")*3600);
		fprintf(fp, "Rec_coating_abs = %lg\n", as_double("coating_absorptance"));
		fprintf(fp, "epsilon = %lg\n", as_double("exter_coating_emittance"));
		fprintf(fp, "night_recirc = %d\n", as_integer("enable_night_recirc"));
		fprintf(fp, "recirc_htr_eff = %lg\n", as_double("recirc_eff"));
		fprintf(fp, "nazm = %d\n", (int)nazm);
		fprintf(fp, "nrad = %d\n", (int)nrad);


		fprintf(fp, "Plant_lattitude = %lg\n", hdr.lat);

		fprintf(fp, "f_rec_min = %lg\n", as_double("min_turndown_frac"));
		fprintf(fp, "Q_rec_des = %lg\n", as_double("design_thermal_power"));
		fprintf(fp, "rec_su_delay = %lg\n", as_double("startup_delay_time"));
		fprintf(fp, "rec_qf_delay = %lg\n", as_double("startup_delay_energy_frac"));

		fputs("\n", fp);

		fprintf(fp, "****************Power Block page****************\n");
		fprintf(fp, "CONSTANTS 27\n");
		fprintf(fp, "tech_type = 1\n");
		fprintf(fp, "LU_pb = 65\n");
		fprintf(fp, "P_cycle_design = %lg\n", as_double("design_gross_output"));
		fprintf(fp, "Eff_cycle_design = %lg\n", as_double("cycle_eff"));
		fprintf(fp, "T_HTF_in_ref = %lg\n", as_double("design_htf_inlet_temp"));
		fprintf(fp, "T_HTF_out_ref = %lg\n", as_double("design_htf_outlet_temp"));
		fprintf(fp, "P_boiler = %lg\n", as_double("boiler_steam_pressure"));
		fprintf(fp, "Cycle_min_inlet_temp = %lg\n", as_double("min_temp_to_load"));
		fprintf(fp, "Turb_startup_t = %lg\n", as_double("startup_time"));
		fprintf(fp, "Turb_startup_f = %lg\n", as_double("startup_fraction"));
		fprintf(fp, "T_standby = %lg\n", as_double("standby_period"));
		fprintf(fp, "F_standby = %lg\n", as_double("standby_fraction"));
		fprintf(fp, "startup_time = %lg\n", as_double("startup_time"));
		fprintf(fp, "startup_frac = %lg\n", as_double("startup_fraction"));
		fprintf(fp, "hl_ffact = %lg\n", as_double("heat_loss_f"));
		fprintf(fp, "cycle_cutoff_frac = %lg\n", as_double("min_load"));
		fprintf(fp, "cycle_max_fraction = %lg\n", as_double("max_over_design"));
		fprintf(fp, "LHV_eff = %lg\n", as_double("lhv_eff"));

		fprintf(fp, "T_amb_des = %lg\n", as_double("design_ambient_temp"));
		fprintf(fp, "dT_cooling_ref = %lg\n", as_double("ref_condenser_dt"));
		fprintf(fp, "Cool_type = %d\n", as_integer("condenser_type")+1);
		fprintf(fp, "T_approach = %lg\n", as_double("approach_temp"));
		fprintf(fp, "T_ITD_des = %lg\n", as_double("itd_design"));
		fprintf(fp, "P_cond_ratio = %lg\n", as_double("condenser_pressure_ratio"));
		fprintf(fp, "pb_bd_frac = %lg\n", as_double("blowdown_frac"));
		fprintf(fp, "min_cond_pres = %lg\n", as_double("min_condenser_pressure"));
		fprintf(fp, "hr_pl_nlev = %d\n", as_integer("hr_pl_nlev"));

		fputs("\n", fp);


		fprintf(fp, "****************Thermal storage page****************\n");
		fprintf(fp, "CONSTANTS 20\n");
		fprintf(fp, "storage_bypass = %d\n", as_integer("storage_bypass"));
		fprintf(fp, "Is_two_tank = %d\n", as_integer("storage_type")+1);
		fprintf(fp, "V_tank_tot = %lg\n", as_double("storage_htf_vol"));
		fprintf(fp, "V_tank_min = %lg\n", as_double("min_fluid_vol"));
		fprintf(fp, "V_tank_max = %lg\n", as_double("max_fluid_vol"));
		fprintf(fp, "r_tank = %lg\n", as_double("tank_diameter")/2);
		double t_d = as_double("tank_diameter");
		fprintf(fp, "Circ_tank = %lg\n", M_PI*t_d);
		fprintf(fp, "A_c_tank = %lg\n", M_PI*pow(t_d/2,2));
		fprintf(fp, "h_tank_wetted = %lg\n", as_double("wetted_loss_coeff"));
		fprintf(fp, "h_tank_dry = %lg\n", as_double("dry_loss_coeff"));
		fprintf(fp, "T_store_hot_initial = %lg\n", as_double("init_hot_htf_temp"));
		fprintf(fp, "T_store_cold_initial = %lg\n", as_double("init_cold_htf_temp"));
		fprintf(fp, "V_store_hot_initial = %lg\n", as_double("init_hot_htf_vol"));
		fprintf(fp, "V_store_cold_initial = %lg\n", as_double("init_cold_htf_vol"));
		fprintf(fp, "tank_pairs = %d\n", as_integer("parallel_tank_pairs"));
		fprintf(fp, "T_htr_ctank= %lg\n", as_double("cold_heater_set_temp"));
		fprintf(fp, "T_htr_htank= %lg\n", as_double("hot_heater_set_temp"));
		fprintf(fp, "Q_max_ctank= %lg\n", as_double("cold_heater_max_load"));
		fprintf(fp, "Q_max_htank= %lg\n", as_double("hot_heater_max_load"));
		fprintf(fp, "eta_tank_htr= %lg\n", as_double("heater_eff"));

		fprintf(fp, "CONSTANTS 38\n");
		fprintf(fp, "TSHOURS = %lg\n", as_double("full_load_ts_hours"));
		fprintf(fp, "NUMTOU = 9\n");
		fprintf(fp, "TSLOGICT = %lg\n", as_double("dispatch1_solar"));
		fprintf(fp, "TSLOGIC1 = %lg\n", as_double("dispatch1_nosolar"));
		fprintf(fp, "TSLOGIC2 = %lg\n", as_double("dispatch1_turbout"));
		fprintf(fp, "TSLOGIC3 = %lg\n", as_double("dispatch2_solar"));
		fprintf(fp, "TSLOGIC4 = %lg\n", as_double("dispatch2_nosolar"));
		fprintf(fp, "TSLOGIC5 = %lg\n", as_double("dispatch2_turbout"));
		fprintf(fp, "TSLOGIC6 = %lg\n", as_double("dispatch3_solar"));
		fprintf(fp, "TSLOGIC7 = %lg\n", as_double("dispatch3_nosolar"));
		fprintf(fp, "TSLOGIC8 = %lg\n", as_double("dispatch3_turbout"));
		fprintf(fp, "TSLOGIC9 = %lg\n", as_double("dispatch4_solar"));
		fprintf(fp, "TSLOGI10 = %lg\n", as_double("dispatch4_nosolar"));
		fprintf(fp, "TSLOGI11 = %lg\n", as_double("dispatch4_turbout"));
		fprintf(fp, "TSLOGI12 = %lg\n", as_double("dispatch5_solar"));
		fprintf(fp, "TSLOGI13 = %lg\n", as_double("dispatch5_nosolar"));
		fprintf(fp, "TSLOGI14 = %lg\n", as_double("dispatch5_turbout"));
		fprintf(fp, "TSLOGI15 = %lg\n", as_double("dispatch6_solar"));
		fprintf(fp, "TSLOGI16 = %lg\n", as_double("dispatch6_nosolar"));
		fprintf(fp, "TSLOGI17 = %lg\n", as_double("dispatch6_turbout"));
		fprintf(fp, "TSLOGI18 = %lg\n", as_double("dispatch7_solar"));
		fprintf(fp, "TSLOGI19 = %lg\n", as_double("dispatch7_nosolar"));
		fprintf(fp, "TSLOGI20 = %lg\n", as_double("dispatch7_turbout"));
		fprintf(fp, "TSLOGI21 = %lg\n", as_double("dispatch8_solar"));
		fprintf(fp, "TSLOGI22 = %lg\n", as_double("dispatch8_nosolar"));
		fprintf(fp, "TSLOGI23 = %lg\n", as_double("dispatch8_turbout"));
		fprintf(fp, "TSLOGI24 = %lg\n", as_double("dispatch9_solar"));
		fprintf(fp, "TSLOGI25 = %lg\n", as_double("dispatch9_nosolar"));
		fprintf(fp, "TSLOGI26 = %lg\n", as_double("dispatch9_turbout"));
		fprintf(fp, "FOSSILFI = %lg\n", as_double("dispatch1_fossil"));
		fprintf(fp, "FOSSILF1 = %lg\n", as_double("dispatch2_fossil"));
		fprintf(fp, "FOSSILF2 = %lg\n", as_double("dispatch3_fossil"));
		fprintf(fp, "FOSSILF3 = %lg\n", as_double("dispatch4_fossil"));
		fprintf(fp, "FOSSILF4 = %lg\n", as_double("dispatch5_fossil"));
		fprintf(fp, "FOSSILF5 = %lg\n", as_double("dispatch6_fossil"));
		fprintf(fp, "FOSSILF6 = %lg\n", as_double("dispatch7_fossil"));
		fprintf(fp, "FOSSILF7 = %lg\n", as_double("dispatch8_fossil"));
		fprintf(fp, "FOSSILF8 = %lg\n", as_double("dispatch9_fossil"));
		fputs("\n", fp);

		fprintf(fp, "CONSTANTS 9\n");
		fprintf(fp, "HC_LOGIC0 = %lg\n", as_double("hc_ctl1"));
		fprintf(fp, "HC_LOGIC1 = %lg\n", as_double("hc_ctl2"));
		fprintf(fp, "HC_LOGIC2 = %lg\n", as_double("hc_ctl3"));
		fprintf(fp, "HC_LOGIC3 = %lg\n", as_double("hc_ctl4"));
		fprintf(fp, "HC_LOGIC4 = %lg\n", as_double("hc_ctl5"));
		fprintf(fp, "HC_LOGIC5 = %lg\n", as_double("hc_ctl6"));
		fprintf(fp, "HC_LOGIC6 = %lg\n", as_double("hc_ctl7"));
		fprintf(fp, "HC_LOGIC7 = %lg\n", as_double("hc_ctl8"));
		fprintf(fp, "HC_LOGIC8 = %lg\n", as_double("hc_ctl9"));

		fprintf(fp, "****************Parasitics page****************\n");
		fprintf(fp, "CONSTANTS 17\n");
		fprintf(fp, "P_hel_start = %lg\n", as_double("heliostat_startup_energy"));
		fprintf(fp, "P_hel_track = %lg\n", as_double("heliostat_tracking_power"));
		fprintf(fp, "eta_rec_pump = %lg\n", as_double("rec_htf_pump_eff"));
		fprintf(fp, "P_storage_pump = %lg\n", as_double("storage_pump_power"));
		fprintf(fp, "piping_loss = %lg\n", as_double("piping_loss_coeff"));
		fprintf(fp, "piping_length = %lg\n", as_double("total_piping_length"));


		fprintf(fp, "aux_par_0 = %lg\n", as_double("aux_c0"));
		fprintf(fp, "aux_par_1 = %lg\n", as_double("aux_c1"));
		fprintf(fp, "aux_par_2 = %lg\n", as_double("aux_c2"));
		fprintf(fp, "aux_par_f = %lg\n", as_double("aux_pf"));
		fprintf(fp, "aux_par = %lg\n", as_double("aux_val"));
		fprintf(fp, "bop_par_0 = %lg\n", as_double("bop_c0"));
		fprintf(fp, "bop_par_1 = %lg\n", as_double("bop_c1"));
		fprintf(fp, "bop_par_2 = %lg\n", as_double("bop_c2"));
		fprintf(fp, "bop_par_f = %lg\n", as_double("bop_pf"));
		fprintf(fp, "bop_par = %lg\n", as_double("bop_val"));
		fprintf(fp, "pb_fixed_par = %lg\n", as_double("pb_fixed_frac"));

	}

	virtual void process_outputs() throw( general_error )
	{
		update("Saving data...", 99.5);

		std::string hourly = work_dir() + util::path_separator() + "trough.hourly.out";

		output d;
		if (!d.read( hourly.c_str() )) throw general_error("could not read hourly output file: " + hourly);

		save_data(d, "DNI", "dni", 1.0, 8760);
		save_data(d, "V_wind", "windspd", 1.0, 8760);
		save_data(d, "Dry_bulb_temp", "ambtemp", 1.0, 8760);
		save_data(d, "Total_incident_power", "sol_rad_inc_on_col", 1000.0, 8760);
		save_data(d, "Power_from_receiver", "thermal_energy_from_sf", 1000.0, 8760);
		save_data(d, "Q_to_PB", "thermal_energy_to_powerblock", 1000.0, 8760);
		save_data(d, "Power_from_cycle_elec", "gross_electric_output", 1000.0, 8760);
		save_data(d, "Power_to_the_grid", "e_net", 1000.0, 8760);
		save_data(d, "water_makeup_flow", "water_flow", (ssc_number_t)(1/998.2), 8760); //Vol [m3] = mass [kg] * 1/998.2 [m3/kg]


		double net_total = 1.0, gross_total = 1.0;
		if (!accumulate_annual( d, "Power_from_cycle_elec", gross_total)
			|| ! accumulate_annual( d, "Power_to_the_grid", net_total))
			throw general_error("could not accumulate annual values for gross and net generation");

		std::string annual = work_dir() + util::path_separator() + "trough.annual.out";
		if (!d.read( annual.c_str() )) throw general_error("could not read annual output file: " + annual);

		save_data(d, "Fossil_energy", "fuel_usage", 1000.0, 1);
		assign("gross_to_net_conversion_factor", var_data( (ssc_number_t)( net_total / gross_total ) ));

		if (!util::remove_file( hourly.c_str() ))
			log("failed to delete hourly file: " + hourly);

	}

	virtual const char *deck_name() throw( general_error )
	{
		if (as_integer("receiver_type") == 1) return "csp_cavtrough";
		else return "csp_exttrough";
	}


	int translate_htf_type(int ui_number)
	{
		/*
	!    1.) Air
	!    2.) Stainless_AISI316
	!    3.) Water (liquid)
	!    4.) Steam
	!    5.) CO2
	!    6.) Salt (68% KCl, 32% MgCl2)
	!    7.) Salt (8% NaF, 92% NaBF4)
	!    8.) Salt (25% KF, 75% KBF4)
	!    9.) Salt (31% RbF, 69% RbBF4)
	!    10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
	!    11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
	!    12.) Salt (58% KF, 42% ZrF4)
	!    13.) Salt (58% LiCl, 42% RbCl)
	!    14.) Salt (58% NaCl, 42% MgCl2)
	!    15.) Salt (59.5% LiCl, 40.5% KCl)
	!    16.) Salt (59.5% NaF, 40.5% ZrF4)
	!    17.) Salt (60% NaNO3, 40% KNO3)
	!    18.) Nitrate Salt**
	!    19.) Caloria HT 43**
	!    20.) Hitec XL**
	!    21.) Therminol VP-1**
	!    22.) Hitec**
	!    23.) Dowtherm Q**
	!    24.) Dowtherm RP**
	!    25.) Salt XL**
	!   .....
	!    36+) User specified (lookup tables)
	*/
		switch(ui_number)
		{
		case 0: return 17;
		case 1: return 10;
		case 2: return 36; // user defined fluid #1
		default: return -1;
		}
	}
};

DEFINE_MODULE_ENTRY( trntrough, "Empirical trough simulator (TRNSYS)", 1 )
