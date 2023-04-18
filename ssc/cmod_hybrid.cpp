/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/ssc/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#include "core.h"



static var_info _cm_vtab_hybrid[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT,         SSC_TABLE,      "input",               "Table input for one technology",             "","","",      "*",        "",      "" },
    { SSC_OUTPUT,        SSC_TABLE,      "output",               "Table output for one technology",           "","","",      "*",        "",      "" },


var_info_invalid };

class cm_hybrid : public compute_module
{
public:

	cm_hybrid()
	{
		add_var_info( _cm_vtab_hybrid );
	}

    void exec()
    {
        auto table = lookup("input");
        if (table->type != SSC_TABLE)
            throw exec_error("hybrid", util::format("No input table found."));


        // loop based on table of table inputs
        // loop for multiple hybrid compute modules starts here
        auto module = ssc_module_create(table->table.as_string("compute_module"));

        auto &input = table->table;
        ssc_module_exec(module, static_cast<ssc_data_t>(&input));
 
        auto outputs = ssc_data_create();

        int pidx = 0;
        while (const ssc_info_t p_inf = ssc_module_var_info(module, pidx++))
        {
            int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
            if (var_type == SSC_OUTPUT) { // maybe add INOUT
                auto var_name = ssc_info_name(p_inf);
                auto type = ssc_info_data_type(p_inf);
                auto var_value = input.lookup(var_name);
                ssc_data_set_var(outputs, var_name, var_value);
            }
        }

        // loop ends here
        // need to agregate some outputs potenitally here

        assign("output", var_data(*(static_cast<var_table*>(outputs))));

        ssc_module_free(module);
        ssc_data_free(outputs);
	}
};

DEFINE_MODULE_ENTRY( hybrid, "Hybrid processing", 1 )
