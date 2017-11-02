#ifndef _INPUT_TOOLBOX_
#define _INPUT_TOOLBOX_

#include "vartab.h"
#include "core.h"
#include <string>

static void var(var_table* vt, std::string name, std::string value){
	var_data* vd = new var_data(value);
	vt->assign(name, *vd);
}

static void var(var_table* vt, std::string name, double value){
	var_data* vd = new var_data((ssc_number_t)value);
	vt->assign(name, *vd);
}

static void var(var_table* vt, std::string name, float* array, int length){
	var_data* vd = new var_data(array, length);
	vt->assign(name, *vd);
}

static void var(var_table* vt, std::string name, float* matrix, int nr, int nc){
	var_data* vd = new var_data(matrix, nr, nc);
	vt->assign(name, *vd);
}

static void var(var_table* vt, std::string name, var_data &vd){
	vt->assign(name, vd);
}

static void modify_var(var_table* vt, std::string name, double value){
	vt->unassign(name);
	vt->assign(name, value);
}

static void modify_var(var_table* vt, std::string name, std::string value){
	vt->unassign(name);
	vt->assign(name, value);
}

static void modify_var(var_table* vt, std::string name, var_data &vd){
	vt->unassign(name);
	vt->assign(name, vd);
}

class TestHandler : public handler_interface
{
public:
	compute_module *m_cm;
	TestHandler(compute_module *cm) : handler_interface(cm) {  }
	void on_log(const std::string &text, int type, float time){ } 
	bool on_update(const std::string &text, float percent_done, float time){ return true; }
};

#endif
