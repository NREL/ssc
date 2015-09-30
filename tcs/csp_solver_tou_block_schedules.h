#ifndef __csp_solver_tou_block_schedules_
#define __csp_solver_tou_block_schedules_

#include "csp_solver_core.h"
#include "csp_solver_util.h"

#include "sam_csp_util.h"

class C_block_schedule
{

protected:

	void size_vv(int n_arrays);
	
	void check_dimensions();

	void check_arrays_for_tous(int n_arrays);

	void set_hr_tou();

public:

	double m_hr_tou[8760];

	// member string for exception messages
	std::string m_error_msg;

	C_block_schedule(){};

	~C_block_schedule(){};

	util::matrix_t<double> mc_weekdays;
	util::matrix_t<double> mc_weekends;

	std::vector< std::vector<double> > mvv_tou_arrays;

	std::vector<string> mv_labels;

	void init(int n_arrays);
};

class C_block_schedule_csp_ops : public C_block_schedule
{

public:

	C_block_schedule_csp_ops();

	~C_block_schedule_csp_ops(){};

	enum 
	{
		TURB_FRAC,

		N_END	
	};

	
	
};

class C_block_schedule_pricing : public C_block_schedule
{

public:

	C_block_schedule_pricing();

	~C_block_schedule_pricing(){};

	enum
	{
		MULT_PRICE,

		N_END	
	};

	
};

class C_csp_tou_block_schedules : public C_csp_tou
{
	
private:
	double m_hr_csp_op_tou[8760];

public:

	// Class to save messages for up stream classes
	C_csp_messages mc_csp_messages;

	// member string for exception messages
	std::string m_error_msg;

	struct S_params
	{
		C_block_schedule_csp_ops mc_csp_ops;
		C_block_schedule_pricing mc_pricing;
	};

	S_params ms_params;

	C_csp_tou_block_schedules(){};

	~C_csp_tou_block_schedules(){};

	virtual void init();

	virtual void call(double time_s, C_csp_tou::S_csp_tou_outputs & tou_outputs);

};




#endif