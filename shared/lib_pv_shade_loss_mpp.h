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

#ifndef pv_shade_loss_mpp_h
#define pv_shade_loss_mpp_h
//extern const unsigned char vmpp[12091680];
//extern const unsigned char impp[12091680];
//extern const unsigned char vs[60458400];
//extern const unsigned char is[60458400];
#include <vector>
#include <stdlib.h>
#include <string>

extern const unsigned char pCmp_data[3133517];
// shading database with up to 8 strings
class ShadeDB8_mpp
{
public:
	enum db_type{VMPP, IMPP};
	ShadeDB8_mpp() {
		p_vmpp = NULL;
		p_impp=NULL ;
	};
	~ShadeDB8_mpp();
	void init();
	short vmpp(size_t ndx){
		return get_vmpp(ndx);
	};
	short impp(size_t ndx){
		return get_impp(ndx);
	};
	std::vector<double> get_vector(const size_t &N, const size_t &d, const size_t &t, const size_t &S, const db_type &DB_TYPE);
	size_t n_choose_k(size_t n, size_t k);
	bool get_index(const size_t &N, const size_t &d, const size_t &t, const size_t &S, const db_type &DB_TYPE, size_t* ret_ndx);

	double get_shade_loss(double &gpoa, double &dpoa, std::vector<double> &shade_frac, bool use_pv_cell_temp = false, double pv_cell_temp = 0, int mods_per_str = 0, double str_vmp_stc = 0, double mppt_lo = 0, double mppt_hi = 0);
	std::string get_warning() { return p_warning_msg; }
	std::string get_error() { return p_error_msg; }


private:
	unsigned char *p_vmpp;
	unsigned char *p_impp;
	short get_vmpp(size_t i);
	short get_impp(size_t i);
	bool decompress_file_to_uint8();
	size_t p_vmpp_uint8_size;
	size_t p_impp_uint8_size;
	size_t p_compressed_size;
	std::string p_warning_msg;
	std::string p_error_msg;
};

#endif