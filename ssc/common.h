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


class shading_losses
{
	bool m_setupOk;
	std::vector<std::string> m_errors;
	std::vector<double> m_beamFactors;
	util::matrix_t<double> m_azaltvals;
	bool m_enAzAlt;
	double m_diffFactor;

public:
	shading_losses( compute_module *cm, const std::string &prefix = "" );
	bool ok();
	std::string get_error(size_t i=0);
	
	// beam and diffuse loss factors (0: full loss, 1: no loss )
	double fbeam( size_t hour /* 0-8759 */, double solalt, double solazi );
	double fdiff();
};

#endif

