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
#include "tcstype.h"

TCS_DEFINE_TYPE( weatherreader )
TCS_DEFINE_TYPE( trnsys_weatherreader )
TCS_DEFINE_TYPE( solarcollector )
TCS_DEFINE_TYPE( pump )
TCS_DEFINE_TYPE( datatest )
TCS_DEFINE_TYPE( sumprod )
TCS_DEFINE_TYPE( sco2_test_type401 )
TCS_DEFINE_TYPE( sam_sco2_recomp_type424 )
TCS_DEFINE_TYPE( tc_test_type402 )
TCS_DEFINE_TYPE( sam_trough_model_type805 )
TCS_DEFINE_TYPE( sam_trough_plant_type807 )
TCS_DEFINE_TYPE( sam_trough_storage_type806 )
TCS_DEFINE_TYPE( sam_mw_pt_type224 )
TCS_DEFINE_TYPE( sam_mw_trough_type250 )
TCS_DEFINE_TYPE( sam_type250_input_generator )
TCS_DEFINE_TYPE( sam_mw_trough_type251 )
TCS_DEFINE_TYPE( sam_mw_csp_SumCalcs )
TCS_DEFINE_TYPE( sam_mw_pt_heliostatfield )
TCS_DEFINE_TYPE( sam_lf_st_pt_type232 )
TCS_DEFINE_TYPE( sam_mw_pt_type222 )
TCS_DEFINE_TYPE( sam_dsg_controller_type265 )
TCS_DEFINE_TYPE( sam_mw_gen_type260 )
TCS_DEFINE_TYPE( sam_mw_lf_type262 )
TCS_DEFINE_TYPE( sam_mw_type234 )
TCS_DEFINE_TYPE( sam_mw_pt_type228 )
TCS_DEFINE_TYPE( sam_pf_dish_collector_type295 )
TCS_DEFINE_TYPE( sam_pf_dish_receiver_type296 )
TCS_DEFINE_TYPE( sam_pf_dish_engine_type297 )
TCS_DEFINE_TYPE( sam_pf_dish_parasitics_type298 )
TCS_DEFINE_TYPE( sam_mw_lf_type261_steam )
TCS_DEFINE_TYPE( sam_iscc_powerblock )
TCS_DEFINE_TYPE( sam_iscc_parasitics )
TCS_DEFINE_TYPE( sam_mw_lf_type261_Wnet )
TCS_DEFINE_TYPE( tou_translator )
TCS_DEFINE_TYPE( atmospheric_aod )
TCS_DEFINE_TYPE( Heliostat3DInterp )
//TCS_DEFINE_TYPE( sam_geothermal_hybrid_pb )

TCS_BEGIN_EXPORT()
	TCS_EXPORT_TYPE( weatherreader )
	TCS_EXPORT_TYPE( trnsys_weatherreader )
	TCS_EXPORT_TYPE( solarcollector )
	TCS_EXPORT_TYPE( pump )
	TCS_EXPORT_TYPE( datatest )
	TCS_EXPORT_TYPE( sumprod )
	TCS_EXPORT_TYPE( sco2_test_type401 )
	TCS_EXPORT_TYPE( sam_sco2_recomp_type424 )
	TCS_EXPORT_TYPE( tc_test_type402 )
	TCS_EXPORT_TYPE( sam_trough_model_type805 )
	TCS_EXPORT_TYPE( sam_trough_plant_type807 )
	TCS_EXPORT_TYPE( sam_trough_storage_type806 )
	TCS_EXPORT_TYPE( sam_mw_pt_type224 )
	TCS_EXPORT_TYPE( sam_mw_trough_type250 )
	TCS_EXPORT_TYPE( sam_type250_input_generator )
	TCS_EXPORT_TYPE( sam_mw_trough_type251 )
	TCS_EXPORT_TYPE( sam_mw_csp_SumCalcs )
	TCS_EXPORT_TYPE( sam_mw_pt_heliostatfield )
	TCS_EXPORT_TYPE( sam_lf_st_pt_type232 )
	TCS_EXPORT_TYPE( sam_mw_pt_type222 )
	TCS_EXPORT_TYPE( sam_mw_gen_type260 )
	TCS_EXPORT_TYPE( sam_dsg_controller_type265 )
	TCS_EXPORT_TYPE( sam_mw_lf_type262 )
	TCS_EXPORT_TYPE( sam_mw_type234 )
	TCS_EXPORT_TYPE( sam_mw_pt_type228 )
	TCS_EXPORT_TYPE( sam_pf_dish_collector_type295 )
	TCS_EXPORT_TYPE( sam_pf_dish_receiver_type296 )
	TCS_EXPORT_TYPE( sam_pf_dish_engine_type297 )
	TCS_EXPORT_TYPE( sam_pf_dish_parasitics_type298 )
	TCS_EXPORT_TYPE( sam_mw_lf_type261_steam )
	TCS_EXPORT_TYPE( sam_iscc_powerblock )
	TCS_EXPORT_TYPE( sam_iscc_parasitics )
	TCS_EXPORT_TYPE( sam_mw_lf_type261_Wnet )
	TCS_EXPORT_TYPE( tou_translator )
	TCS_EXPORT_TYPE( atmospheric_aod )
	TCS_EXPORT_TYPE( Heliostat3DInterp )
	//TCS_EXPORT_TYPE( sam_geothermal_hybrid_pb )
TCS_END_EXPORT()





