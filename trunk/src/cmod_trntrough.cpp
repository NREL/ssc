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
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.1",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.10",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.2",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.3",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.4",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.5",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.6",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.7",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.8",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"sysudv.label.9",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.1",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.10",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.2",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.3",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.4",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.5",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.6",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.7",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.8",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },
//{ SSC_INPUT,	SSC_STRING,	"uservar.label.9",	"",	"",	"",	"CSP Trough Empirical",	"?=",	"",	"" },

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
		std::string fluid_file = work_dir() + util::path_separator() + "fluidprop.dat";
		std::string warnings_file = work_dir() + util::path_separator() + "warnings.out";
		const char *weather_file = as_string("weather_file");


		wf_header hdr;
		if (!wf_read_header( weather_file, &hdr ))
			throw general_error("could not scan weather file header information: " + std::string(weather_file));


		FILE *fout;

		// clear the warnings file
		fout = fopen(warnings_file.c_str(),"w");
		if (fout) fclose(fout);

		// write the eff_array, array_view, and fluxmap files

		// ========= write actual include file


		fputs("\n", fp);
		fprintf(fp, "ASSIGN \"%s\" 13\n", weather_file);
		fprintf(fp, "ASSIGN \"%s\" 39\n", touperiod_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 40\n", annual_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 36\n", hourly_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 35\n", dview_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 25\n", monthly_output_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 95\n", fluid_file.c_str());
		fprintf(fp, "ASSIGN \"%s\" 96\n", warnings_file.c_str());
		fputs("\n", fp);

	fprintf(fp, "EQUATIONS 6\n");
	fprintf(fp, "I_MONTH= 0\n");
	fprintf(fp, "I_DAY1= 1\n");
	fprintf(fp, "I_DAY=I_MONTH/24+I_DAY1\n");
	fprintf(fp, "I_LENGTH= 365\n");
	fprintf(fp, "WEEKS=I_LENGTH/7.\n");
/*	// cmod_trnbase
	fprintf(fp, "START=24*(I_DAY-1)+1\n");
	fprintf(fp, "STOP=START+I_LENGTH*24-1\n");
	fprintf(fp, "STEP=1\n");
//	fprintf(fp, "STEP=%lg\n", GetTimestep());
*/	
	fprintf(fp, "dispswch=-1\n\n");

	fprintf(fp, "CONSTANTS 1\n");
	fprintf(fp, "NUMTOU = 9\n\n");

	fprintf(fp, "CONSTANTS 9\n");
	fprintf(fp, "STOWANGL = %lg\n", as_double("csp.tr.solf.stowangle"));
	fprintf(fp, "DEPLYANG = %lg\n", as_double("csp.tr.solf.deployangle"));
	fprintf(fp, "DISTANCE = %lg\n", as_double("csp.tr.solf.distscas"));
	fprintf(fp, "DISTANC1 = %lg\n", as_double("csp.tr.solf.distrows"));
	fprintf(fp, "SCAAPER = %lg\n", as_double("csp.tr.sca.aperture"));
	fprintf(fp, "SFAVAIL = %lg\n", as_double("csp.tr.sca.availability"));
	fprintf(fp, "NUMSCAS= %lg\n", as_double("csp.tr.solf.nscasperloop"));

	fprintf(fp, "SOLARFIE = %lg\n", as_double("csp.tr.solf.dp.fieldarea"));
	fprintf(fp, "SOLFLDMT = %lg\n\n", as_double("csp.tr.solf.dp.solarmultiple"));
	
	fprintf(fp, "CONSTANTS 5\n");
	fprintf(fp, "TRAKMODE = 3\n");
	fprintf(fp, "WFType = %d\n", weather_file_type( weather_file ));
	fprintf(fp, "TILT = %lg\n", as_double("csp.tr.solf.tilt"));
	fprintf(fp, "AZIMUTHO = %lg\n", as_double("csp.tr.solf.azimuth"));
	fprintf(fp, "SFti_init = %lg\n\n", as_double("csp.tr.solf.htfinittemp"));

	fprintf(fp, "CONSTANTS 14\n");
	fprintf(fp, "NUMHCETY = 4\n");
	fprintf(fp, "IAMF = %lg\n", as_double("csp.tr.sca.iamc1"));
	fprintf(fp, "IAM1 = %lg\n", as_double("csp.tr.sca.iamc2"));
	fprintf(fp, "IAM2 = %lg\n", as_double("csp.tr.sca.iamc3"));
	fprintf(fp, "AVEFOCAL = %lg\n", as_double("csp.tr.sca.avg_focal_length"));
	fprintf(fp, "SCALEN = %lg\n", as_double("csp.tr.sca.length"));
	fprintf(fp, "SFPAR = %lg\n", as_double("csp.tr.par.sf.total"));
	fprintf(fp, "SFPARPF = %lg\n", as_double("csp.tr.par.sf.partload"));
	fprintf(fp, "COLTYPE = 1\n");
	fprintf(fp, "TRKTWST = %lg\n", as_double("csp.tr.sca.track_twist_error"));
	fprintf(fp, "GEOACC = %lg\n", as_double("csp.tr.sca.geometric_accuracy"));
	fprintf(fp, "MIRREF = %lg\n", as_double("csp.tr.sca.reflectivity"));
	fprintf(fp, "MIRCLN = %lg\n", as_double("csp.tr.sca.cleanliness"));
	fprintf(fp, "CNCTRATE = %lg\n\n", as_double("csp.tr.sca.concentrator_factor"));

	fprintf(fp, "CONSTANTS 64\n");
	fprintf(fp, "HCEtype1 = 1\n");
	fprintf(fp, "HCEFrac1 = %lg\n", as_double("csp.tr.hce.unit1_frac"));
	fprintf(fp, "HCEdust1 = %lg\n", as_double("csp.tr.sca.dust_on_envelope"));
	fprintf(fp, "HCEBels1 = %lg\n", as_double("csp.tr.hce.bellows1"));
	fprintf(fp, "HCEEnvt1 = %lg\n", as_double("csp.tr.hce.envtrans1"));
	fprintf(fp, "HCEabs1  = %lg\n", as_double("csp.tr.hce.absorption1"));
	fprintf(fp, "HCEmisc1 = %lg\n", as_double("csp.tr.hce.unaccounted1"));
	fprintf(fp, "PerfFac1 = %lg\n", as_double("csp.tr.hce.heatlossf1"));
	fprintf(fp, "RefMirA1 = %lg\n", as_double("csp.tr.sca.aperture"));
	fprintf(fp, "HCE_A01 = %lg\n", as_double("csp.tr.hce.a0_1"));
	fprintf(fp, "HCE_A11 = %lg\n", as_double("csp.tr.hce.a1_1"));
	fprintf(fp, "HCE_A21 = %lg\n", as_double("csp.tr.hce.a2_1"));
	fprintf(fp, "HCE_A31 = %lg\n", as_double("csp.tr.hce.a3_1"));
	fprintf(fp, "HCE_A41 = %lg\n", as_double("csp.tr.hce.a4_1"));
	fprintf(fp, "HCE_A51 = %lg\n", as_double("csp.tr.hce.a5_1"));
	fprintf(fp, "HCE_A61 = %lg\n", as_double("csp.tr.hce.a6_1"));

	fprintf(fp, "HCEtype2 = 1\n");
	fprintf(fp, "HCEFrac2 = %lg\n", as_double("csp.tr.hce.unit2_frac"));
	fprintf(fp, "HCEdust2 = %lg\n", as_double("csp.tr.sca.dust_on_envelope"));
	fprintf(fp, "HCEBels2 = %lg\n", as_double("csp.tr.hce.bellows2"));
	fprintf(fp, "HCEEnvt2 = %lg\n", as_double("csp.tr.hce.envtrans2"));
	fprintf(fp, "HCEabs2  = %lg\n", as_double("csp.tr.hce.absorption2"));
	fprintf(fp, "HCEmisc2 = %lg\n", as_double("csp.tr.hce.unaccounted2"));
	fprintf(fp, "PerfFac2 = %lg\n", as_double("csp.tr.hce.heatlossf2"));
	fprintf(fp, "RefMirA2 = %lg\n", as_double("csp.tr.sca.aperture"));
	fprintf(fp, "HCE_A02 = %lg\n", as_double("csp.tr.hce.a0_2"));
	fprintf(fp, "HCE_A12 = %lg\n", as_double("csp.tr.hce.a1_2"));
	fprintf(fp, "HCE_A22 = %lg\n", as_double("csp.tr.hce.a2_2"));
	fprintf(fp, "HCE_A32 = %lg\n", as_double("csp.tr.hce.a3_2"));
	fprintf(fp, "HCE_A42 = %lg\n", as_double("csp.tr.hce.a4_2"));
	fprintf(fp, "HCE_A52 = %lg\n", as_double("csp.tr.hce.a5_2"));
	fprintf(fp, "HCE_A62 = %lg\n", as_double("csp.tr.hce.a6_2"));

	fprintf(fp, "HCEtype3 = 1\n");
	fprintf(fp, "HCEFrac3 = %lg\n", as_double("csp.tr.hce.unit3_frac"));
	fprintf(fp, "HCEdust3 = %lg\n", as_double("csp.tr.sca.dust_on_envelope"));
	fprintf(fp, "HCEBels3 = %lg\n", as_double("csp.tr.hce.bellows3"));
	fprintf(fp, "HCEEnvt3 = %lg\n", as_double("csp.tr.hce.envtrans3"));
	fprintf(fp, "HCEabs3  = %lg\n", as_double("csp.tr.hce.absorption3"));
	fprintf(fp, "HCEmisc3 = %lg\n", as_double("csp.tr.hce.unaccounted3"));
	fprintf(fp, "PerfFac3 = %lg\n", as_double("csp.tr.hce.heatlossf3"));
	fprintf(fp, "RefMirA3 = %lg\n", as_double("csp.tr.sca.aperture"));
	fprintf(fp, "HCE_A03 = %lg\n", as_double("csp.tr.hce.a0_3"));
	fprintf(fp, "HCE_A13 = %lg\n", as_double("csp.tr.hce.a1_3"));
	fprintf(fp, "HCE_A23 = %lg\n", as_double("csp.tr.hce.a2_3"));
	fprintf(fp, "HCE_A33 = %lg\n", as_double("csp.tr.hce.a3_3"));
	fprintf(fp, "HCE_A43 = %lg\n", as_double("csp.tr.hce.a4_3"));
	fprintf(fp, "HCE_A53 = %lg\n", as_double("csp.tr.hce.a5_3"));
	fprintf(fp, "HCE_A63 = %lg\n", as_double("csp.tr.hce.a6_3"));

	fprintf(fp, "HCEtype4 = 1\n");
	fprintf(fp, "HCEFrac4 = %lg\n", as_double("csp.tr.hce.unit4_frac"));
	fprintf(fp, "HCEdust4 = %lg\n", as_double("csp.tr.sca.dust_on_envelope"));
	fprintf(fp, "HCEBels4 = %lg\n", as_double("csp.tr.hce.bellows4"));
	fprintf(fp, "HCEEnvt4 = %lg\n", as_double("csp.tr.hce.envtrans4"));
	fprintf(fp, "HCEabs4  = %lg\n", as_double("csp.tr.hce.absorption4"));
	fprintf(fp, "HCEmisc4 = %lg\n", as_double("csp.tr.hce.unaccounted4"));
	fprintf(fp, "PerfFac4 = %lg\n", as_double("csp.tr.hce.heatlossf4"));
	fprintf(fp, "RefMirA4 = %lg\n", as_double("csp.tr.sca.aperture"));
	fprintf(fp, "HCE_A04 = %lg\n", as_double("csp.tr.hce.a0_4"));
	fprintf(fp, "HCE_A14 = %lg\n", as_double("csp.tr.hce.a1_4"));
	fprintf(fp, "HCE_A24 = %lg\n", as_double("csp.tr.hce.a2_4"));
	fprintf(fp, "HCE_A34 = %lg\n", as_double("csp.tr.hce.a3_4"));
	fprintf(fp, "HCE_A44 = %lg\n", as_double("csp.tr.hce.a4_4"));
	fprintf(fp, "HCE_A54 = %lg\n", as_double("csp.tr.hce.a5_4"));
	fprintf(fp, "HCE_A64 = %lg\n\n", as_double("csp.tr.hce.a6_4"));

	fprintf(fp, "CONSTANTS 26\n");
	fprintf(fp, "TURBOUTG = %lg\n", as_double("csp.tr.pwrb.design_gross_output"));
	fprintf(fp, "TURBEFFG = %lg\n", as_double("csp.tr.pwrb.effdesign"));
	fprintf(fp, "PTTMAX = %lg\n", as_double("csp.tr.pwrb.maxoutput"));
	fprintf(fp, "PTTMIN = %lg\n", as_double("csp.tr.pwrb.minoutput"));
	fprintf(fp, "TURSUE = %lg\n", as_double("csp.tr.pwrb.startup_energy"));
	fprintf(fp, "TEPLF = %lg\n", as_double("csp.tr.pwrb.tpl_tef0"));
	fprintf(fp, "TEPL1 = %lg\n", as_double("csp.tr.pwrb.tpl_tef1"));
	fprintf(fp, "TEPL2 = %lg\n", as_double("csp.tr.pwrb.tpl_tef2"));
	fprintf(fp, "TEPL3 = %lg\n", as_double("csp.tr.pwrb.tpl_tef3"));
	fprintf(fp, "TEPL4 = %lg\n", as_double("csp.tr.pwrb.tpl_tef4"));
	fprintf(fp, "ETPLF = %lg\n", as_double("csp.tr.pwrb.tpl_tff0"));
	fprintf(fp, "ETPL1 = %lg\n", as_double("csp.tr.pwrb.tpl_tff1"));
	fprintf(fp, "ETPL2 = %lg\n", as_double("csp.tr.pwrb.tpl_tff2"));
	fprintf(fp, "ETPL3 = %lg\n", as_double("csp.tr.pwrb.tpl_tff3"));
	fprintf(fp, "ETPL4 = %lg\n", as_double("csp.tr.pwrb.tpl_tff4"));
	fprintf(fp, "TEMPCORR = %d\n", as_integer("csp.tr.pwrb.temp_corr_mode") + 1);
	fprintf(fp, "TEMPCOR1 = %lg\n", as_double("csp.tr.pwrb.ctcf0"));
	fprintf(fp, "TEMPCOR2 = %lg\n", as_double("csp.tr.pwrb.ctcf1"));
	fprintf(fp, "TEMPCOR3 = %lg\n", as_double("csp.tr.pwrb.ctcf2"));
	fprintf(fp, "TEMPCOR4 = %lg\n", as_double("csp.tr.pwrb.ctcf3"));
	fprintf(fp, "TEMPCOR5 = %lg\n", as_double("csp.tr.pwrb.ctcf4"));
	fprintf(fp, "TURTESEF = %lg\n", as_double("csp.tr.tes.adj_eff"));
	fprintf(fp, "TURTESOU = %lg\n", as_double("csp.tr.tes.adj_output"));
	fprintf(fp, "MINGROUT = %lg\n", as_double("csp.tr.pwrb.minoutput"));
	fprintf(fp, "MAXGROUT = %lg\n", as_double("csp.tr.pwrb.maxoutput"));
	fprintf(fp, "LHVBoilEff = %lg\n\n", as_double("csp.tr.pwrb.boiler_lhv_eff"));

	fprintf(fp, "CONSTANTS 5\n");
	fprintf(fp, "HTFFLUID = %d\n", translate_htf_type( as_integer("csp.tr.solf.fieldhtftype") ));
	fprintf(fp, "SFINTMPD = %lg\n", as_double("csp.tr.solf.htfinlettemp"));
	fprintf(fp, "SFOUTTMPD = %lg\n", as_double("csp.tr.solf.htfoutlettemp"));
	fprintf(fp, "MINHTFTE = %lg\n", as_double("csp.tr.solf.htfmintemp"));
	fprintf(fp, "HTFGALAR = %lg\n\n", as_double("csp.tr.solf.htfgallonsperarea"));

	fprintf(fp, "CONSTANTS 37\n");
	fprintf(fp, "TSHOURS = %lg\n", as_double("csp.tr.tes.full_load_hours"));
	fprintf(fp, "TSLOGICT = %lg\n", as_double("csp.tr.tes.disp1.solar"));
	fprintf(fp, "TSLOGIC1 = %lg\n", as_double("csp.tr.tes.disp1.nosolar"));
	fprintf(fp, "TSLOGIC2 = %lg\n", as_double("csp.tr.tes.disp1.turbout"));
	fprintf(fp, "TSLOGIC3 = %lg\n", as_double("csp.tr.tes.disp2.solar"));
	fprintf(fp, "TSLOGIC4 = %lg\n", as_double("csp.tr.tes.disp2.nosolar"));
	fprintf(fp, "TSLOGIC5 = %lg\n", as_double("csp.tr.tes.disp2.turbout"));
	fprintf(fp, "TSLOGIC6 = %lg\n", as_double("csp.tr.tes.disp3.solar"));
	fprintf(fp, "TSLOGIC7 = %lg\n", as_double("csp.tr.tes.disp3.nosolar"));
	fprintf(fp, "TSLOGIC8 = %lg\n", as_double("csp.tr.tes.disp3.turbout"));
	fprintf(fp, "TSLOGIC9 = %lg\n", as_double("csp.tr.tes.disp4.solar"));
	fprintf(fp, "TSLOGI10 = %lg\n", as_double("csp.tr.tes.disp4.nosolar"));
	fprintf(fp, "TSLOGI11 = %lg\n", as_double("csp.tr.tes.disp4.turbout"));	
	fprintf(fp, "TSLOGI12 = %lg\n", as_double("csp.tr.tes.disp5.solar"));
	fprintf(fp, "TSLOGI13 = %lg\n", as_double("csp.tr.tes.disp5.nosolar"));
	fprintf(fp, "TSLOGI14 = %lg\n", as_double("csp.tr.tes.disp5.turbout"));
	fprintf(fp, "TSLOGI15 = %lg\n", as_double("csp.tr.tes.disp6.solar"));
	fprintf(fp, "TSLOGI16 = %lg\n", as_double("csp.tr.tes.disp6.nosolar"));
	fprintf(fp, "TSLOGI17 = %lg\n", as_double("csp.tr.tes.disp6.turbout"));
	
	fprintf(fp, "TSLOGI18 = %lg\n", as_double("csp.tr.tes.disp7.solar"));
	fprintf(fp, "TSLOGI19 = %lg\n", as_double("csp.tr.tes.disp7.nosolar"));
	fprintf(fp, "TSLOGI20 = %lg\n", as_double("csp.tr.tes.disp7.turbout"));
	
	fprintf(fp, "TSLOGI21 = %lg\n", as_double("csp.tr.tes.disp8.solar"));
	fprintf(fp, "TSLOGI22 = %lg\n", as_double("csp.tr.tes.disp8.nosolar"));
	fprintf(fp, "TSLOGI23 = %lg\n", as_double("csp.tr.tes.disp8.turbout"));
	
	fprintf(fp, "TSLOGI24 = %lg\n", as_double("csp.tr.tes.disp9.solar"));
	fprintf(fp, "TSLOGI25 = %lg\n", as_double("csp.tr.tes.disp9.nosolar"));
	fprintf(fp, "TSLOGI26 = %lg\n", as_double("csp.tr.tes.disp9.turbout"));

	fprintf(fp, "FOSSILFI = %lg\n", as_double("csp.tr.tes.disp1.fossil"));
	fprintf(fp, "FOSSILF1 = %lg\n", as_double("csp.tr.tes.disp2.fossil"));
	fprintf(fp, "FOSSILF2 = %lg\n", as_double("csp.tr.tes.disp3.fossil"));
	fprintf(fp, "FOSSILF3 = %lg\n", as_double("csp.tr.tes.disp4.fossil"));
	fprintf(fp, "FOSSILF4 = %lg\n", as_double("csp.tr.tes.disp5.fossil"));
	fprintf(fp, "FOSSILF5 = %lg\n", as_double("csp.tr.tes.disp6.fossil"));
	fprintf(fp, "FOSSILF6 = %lg\n", as_double("csp.tr.tes.disp7.fossil"));
	fprintf(fp, "FOSSILF7 = %lg\n", as_double("csp.tr.tes.disp8.fossil"));
	fprintf(fp, "FOSSILF8 = %lg\n\n", as_double("csp.tr.tes.disp9.fossil"));


	fprintf(fp, "CONSTANTS 7\n");
	fprintf(fp, "TNKHL = %lg\n", as_double("csp.tr.tes.tank_heatloss"));
	fprintf(fp, "PTSMAX = %lg\n", as_double("csp.tr.tes.max_to_power"));
	fprintf(fp, "PFSMAX = %lg\n", as_double("csp.tr.tes.max_from_power"));
	fprintf(fp, "SfPipHLD = %lg\n", as_double("csp.tr.solf.pipingheatlossatdesign"));
	fprintf(fp, "SfPipHl1 = %lg\n", as_double("csp.tr.solf.pipingheatlosscoeff1"));
	fprintf(fp, "SfPipHl2 = %lg\n", as_double("csp.tr.solf.pipingheatlosscoeff2"));
	fprintf(fp, "SfPipHl3 = %lg\n\n", as_double("csp.tr.solf.pipingheatlosscoeff3"));

	fprintf(fp, "CONSTANTS 28\n");
	fprintf(fp, "CHTFPAR = %lg\n", as_double("csp.tr.par.htfpump.total"));
	fprintf(fp, "CHTFPARP = %lg\n", as_double("csp.tr.par.htfpump.partload"));
	fprintf(fp, "CHTFPARF = %lg\n", as_double("csp.tr.par.htfpump.f0"));
	fprintf(fp, "CHTFPAR1 = %lg\n", as_double("csp.tr.par.htfpump.f1"));
	fprintf(fp, "CHTFPAR2 = %lg\n", as_double("csp.tr.par.htfpump.f2"));

	fprintf(fp, "ANTIFRPA = %lg\n", as_double("csp.tr.par.antifreeze.total"));

	fprintf(fp, "BOPPAR = %lg\n", as_double("csp.tr.par.bop.total"));
	fprintf(fp, "BOPPARPF = %lg\n", as_double("csp.tr.par.bop.partload"));
	fprintf(fp, "BOPPARF = %lg\n", as_double("csp.tr.par.bop.f0"));
	fprintf(fp, "BOPPAR1 = %lg\n", as_double("csp.tr.par.bop.f1"));
	fprintf(fp, "BOPPAR2 = %lg\n", as_double("csp.tr.par.bop.f2"));

	fprintf(fp, "CTOPF = %d\n", as_integer("csp.tr.par.operation_mode"));
	fprintf(fp, "CTPAR = %lg\n", as_double("csp.tr.par.ct0.total"));
	fprintf(fp, "CTPARPF = %lg\n", as_double("csp.tr.par.ct0.partload"));
	fprintf(fp, "CTPARF = %lg\n", as_double("csp.tr.par.ct0.f0"));
	fprintf(fp, "CTPAR1 = %lg\n", as_double("csp.tr.par.ct0.f1"));
	fprintf(fp, "CTPAR2 = %lg\n", as_double("csp.tr.par.ct0.f2"));

	fprintf(fp, "HTRPAR = %lg\n", as_double("csp.tr.par.hb.total"));
	fprintf(fp, "HTRPARPF = %lg\n", as_double("csp.tr.par.hb.partload"));
	fprintf(fp, "HTRPARF = %lg\n", as_double("csp.tr.par.hb.f0"));
	fprintf(fp, "HTRPAR1 = %lg\n", as_double("csp.tr.par.hb.f1"));
	fprintf(fp, "HTRPAR2 = %lg\n", as_double("csp.tr.par.hb.f2"));

	fprintf(fp, "HHTFPAR = %lg\n", as_double("csp.tr.par.tes.total"));
	fprintf(fp, "HHTFPARP = %lg\n", as_double("csp.tr.par.tes.partload"));
	fprintf(fp, "HHTFPARF = %lg\n", as_double("csp.tr.par.tes.f0"));
	fprintf(fp, "HHTFPAR1 = %lg\n", as_double("csp.tr.par.tes.f1"));
	fprintf(fp, "HHTFPAR2 = %lg\n", as_double("csp.tr.par.tes.f2"));

	fprintf(fp, "PBFIXPAR = %lg\n", as_double("csp.tr.par.fixedblock.total"));


	}

	virtual void process_outputs() throw( general_error )
	{
		update("Saving data...", 99.5);

		std::string hourly = work_dir() + util::path_separator() + "trough.hourly.out";

		output d;
		if (!d.read( hourly.c_str() )) throw general_error("could not read hourly output file: " + hourly);

		save_data(d, "DNI", "dni", 1.0, 8760);
		save_data(d, "Q_nipCosTh", "dni_costh", 1.0, 8760);
		save_data(d, "V_wind", "windspd", 1.0, 8760);
		save_data(d, "T_dry_bulb", "ambtemp", 1.0, 8760);

		save_data(d, "Q_DNI_on_SF", "sol_rad_inc_on_col", 1000.0, 8760);
		save_data(d, "QSF_nipCosTh", "qsf_nip_costh", 1000.0, 8760);
		save_data(d, "SFAvailableEnergy", "sf_avail_energy", 1000.0, 8760);
		save_data(d, "QSF_Abs", "qsf_abs", 1000.0, 8760);
		save_data(d, "QSF_HCE_HL", "qsf_hce_hl", 1000.0, 8760);
		save_data(d, "QSF_Pipe_HL", "qsf_pipe_hl", 1000.0, 8760);
		save_data(d, "QSF", "qsf", 1000.0, 8760);
		save_data(d, "Q_TES_Full", "q_tes_full", 1000.0, 8760);
		save_data(d, "Q_TES_HL", "q_tes_hl", 1000.0, 8760);
		save_data(d, "Q_turb_SU", "q_turb_su", 1000.0, 8760);
		save_data(d, "Q_dump", "q_dump", 1000.0, 8760);
		save_data(d, "Q_to_PB", "q_to_pb", 1000.0, 8760);
		save_data(d, "E_Gross", "gross_electric_output", 1000.0, 8760);
		save_data(d, "E_Net", "e_net", 1000.0, 8760);


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
		return "csp_trough";
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
		case 0: return 18;
		case 1: return 19;
		case 2: return 20;
		case 3: return 21;
		case 4: return 22;
		case 5: return 23;
		case 6: return 24;
		case 7: return 36; // user defined HTF
		default: return -1;
		}
	}
};

DEFINE_MODULE_ENTRY( trntrough, "Empirical trough simulator (TRNSYS)", 1 )
