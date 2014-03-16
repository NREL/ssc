#ifndef __common_h
#define __common_h

#include <vector>
#include "core.h"


extern var_info vtab_standard_financial[];
extern var_info vtab_standard_loan[];
extern var_info vtab_oandm[];
extern var_info vtab_depreciation[];
extern var_info vtab_tax_credits[];
extern var_info vtab_payment_incentives[];

extern var_info vtab_adjustment_factors[];

class adjustment_factors
{
	compute_module *m_cm;
	std::vector<float> m_factors;
	std::string m_error;
public:
	adjustment_factors(compute_module *cm);
	bool setup();
	float operator()(size_t time);
	std::string error() { return m_error; }
};


#endif

