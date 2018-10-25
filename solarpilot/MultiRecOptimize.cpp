#include <set>

#include "SolarField.h"
#include "MultiRecOptimize.h"
#include <lp_lib.h>

void __WINAPI opt_logfunction(lprec *, void *handler, char *buf)
{
    static_cast<multi_rec_opt_helper*>(handler)->simlog << buf << "\n";
}

int __WINAPI opt_abortfunction(lprec *, void *userhandle)
{
    multi_rec_opt_helper* par = static_cast<multi_rec_opt_helper*>(userhandle);
    return par->is_abort_flag ? TRUE : FALSE;
}

void __WINAPI opt_iter_function(lprec *lp, void *userhandle, int msg)
{
    multi_rec_opt_helper* par = static_cast<multi_rec_opt_helper*>(userhandle);

    /*if( get_timeout(lp) > 0 )
        par->is_abort_flag = true;*/

    if (msg == MSG_LPBETTER)
    {
        par->objective = get_working_objective(lp);
    }
}

//----------------------------------------------------------------------------------
//----------------------------------------------------------------------------------

class optimization_vars
{
    int current_mem_pos;
    int alloc_mem_size;

    REAL *data;
public:
    struct opt_var
    {
        std::string name;
        int var_type;
        int var_dim;
        int var_dim_size;
        int var_dim_size2;
        int ind_start;
        int ind_end;
        REAL upper_bound;
        REAL lower_bound;
    };
private:
    std::vector<opt_var> var_objects;

    unordered_map<std::string, opt_var*> var_by_name;

public:
    struct VAR_TYPE { enum A { REAL_T, INT_T, BINARY_T }; };
    struct VAR_DIM { enum A { DIM_T, DIM_NT, DIM_T2, DIM_2T_TRI }; };

    optimization_vars();
    //~optimization_vars();

    void add_var(const std::string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, REAL lowbo = -DEF_INFINITE, REAL upbo = DEF_INFINITE);
    void add_var(const std::string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, int var_dim_size2, REAL lowbo = -DEF_INFINITE, REAL upbo = DEF_INFINITE);

    bool construct();

    int get_num_varobjs();
    int get_total_var_count();

    REAL &operator()(char *varname, int ind);    //Access for 1D var
    REAL &operator()(char *varname, int ind1, int ind2);     //Access for 2D var
    REAL &operator()(int varindex, int ind);
    REAL &operator()(int varindex, int ind1, int ind2);

    int column(const std::string &varname, int ind);
    int column(const std::string &varname, int ind1, int ind2);
    int column(int varindex, int ind);
    int column(int varindex, int ind1, int ind2);

    REAL *get_variable_array();

    opt_var *get_var(const std::string &varname);
    opt_var *get_var(int varindex);
};



//----------------------------------------------------------------------------------
//----------------------------------------------------------------------------------

int multi_rec_opt_helper::run(SolarField *SF)
{
    /*

    */

    Hvector *helios = SF->getHeliostats();
    std::vector<Receiver*> *recs = SF->getReceivers();
    int Nrec = recs->size();

    //calculate receiver maximum powers
    std::vector< double > rec_design_power;
    double rec_frac_total = 0.;

    for (int i = 0; i < Nrec; i++)
    {
        double f = recs->at(i)->getVarMap()->power_fraction.val;
        rec_frac_total += f;
        rec_design_power.push_back(recs->at(i)->getVarMap()->power_fraction.val);
    }
    for (int i = 0; i < Nrec; i++)
        rec_design_power[i] *= SF->getVarMap()->sf.q_des.val * 1.e6 / rec_frac_total;

    int Nh = helios->size();

    //heliostat to receiver power fractions
    unordered_map<int, std::vector<double> > power_allocs;
    unordered_map<int, std::vector<double> > costs;
    double max_rank = 0.;

    for (int i = 0; i < Nh; i++)
    {
        int id = helios->at(i)->getId();

        double rank_metric = helios->at(i)->getRankingMetricValue();

        unordered_map<Receiver*, double> rpa = helios->at(i)->getReceiverPowerAlloc();

        power_allocs[id] = std::vector<double>();
        costs[id] = std::vector<double>();

        for (std::vector<Receiver*>::iterator r = recs->begin(); r != recs->end(); r++)
        {
            double powalloc = rpa[*r];
            power_allocs[id].push_back(powalloc);
            costs[id].push_back(powalloc * rank_metric);

            max_rank = std::fmax(powalloc*rank_metric, max_rank);
        }
    }

    for (unordered_map<int, std::vector<double> >::iterator pair = costs.begin(); pair != costs.end(); pair++)
        for (std::vector<double>::iterator dit = pair->second.begin(); dit != pair->second.end(); dit++)
            *dit = max_rank / (*dit > 0. ? *dit : max_rank * 1e3);

    //---------------------------------------

    optimization_vars O;


    O.add_var("x", optimization_vars::VAR_TYPE::REAL_T, optimization_vars::VAR_DIM::DIM_NT, Nh, Nrec, 0., 1.);
    O.construct();

    lprec* lp = make_lp(0, Nh*Nrec);

    //set up objective
    int *col = new int[Nh*Nrec];
    REAL *row = new REAL[Nh*Nrec];
    for (int j = 0; j < Nrec; j++)
    {
        for (int i = 0; i < Nh; i++)
        {
            col[j*Nh + i] = O.column("x", i, j);
            row[j*Nh + i] = costs[helios->at(i)->getId()][j];  //cost of energy proxy for each heliostat
        }
    }

    set_obj_fnex(lp, Nh*Nrec, row, col);

    set_add_rowmode(lp, TRUE);

    /*
    --------------------------------------------------------------------------------
    set up the variable properties
    --------------------------------------------------------------------------------
    */
    //upper and lower variable bounds
    for (int j = 0; j < Nrec; j++)
    {
        for (int i = 0; i < Nh; i++)
        {
            set_lowbo(lp, j*Nh + i + 1, 0.);

            //set column name
            char s[40];
            sprintf(s, "%d:%d", helios->at(i)->getId(), j);
            set_col_name(lp, O.column("x", i, j), s);
        }
    }

    /*
    --------------------------------------------------------------------------------
    set up the constraints
    --------------------------------------------------------------------------------
    */
    //each receiver must meet a minimum power requirement
    for (int j = 0; j < Nrec; j++)
    {
        for (int i = 0; i < Nh; i++)
        {
            int id = helios->at(i)->getId();
            col[i] = O.column("x", i, j);
            row[i] = power_allocs[id].at(j);
        }
        add_constraintex(lp, Nh, row, col, GE, rec_design_power.at(j));
    }

    //total allocation can't exceed 1
    for (int i = 0; i < Nh; i++)
    {
        for (int j = 0; j < Nrec; j++)
        {
            col[j] = O.column("x", i, j);
            row[j] = 1.;
        }
        add_constraintex(lp, Nrec, row, col, LE, 1.);
    }

    delete[] row;
    delete[] col;

    //Set problem to maximize
    set_minim(lp);

    //reset the row mode
    set_add_rowmode(lp, FALSE);

    //optimization control parameters
    is_abort_flag = false;

    put_msgfunc(lp, opt_iter_function, (void*)(this), MSG_ITERATION | MSG_LPBETTER | MSG_LPFEASIBLE | MSG_LPOPTIMAL);
    put_abortfunc(lp, opt_abortfunction, (void*)(this));
    put_logfunc(lp, opt_logfunction, (void*)(this));
    set_verbose(lp, 1); //http://web.mit.edu/lpsolve/doc/set_verbose.htm
    set_presolve(lp, PRESOLVE_ROWS | PRESOLVE_COLS | PRESOLVE_LINDEP, get_presolveloops(lp));   //independent optimization
    set_timeout(lp, timeout_sec);  //max solution time
    set_scaling(lp, SCALE_CURTISREID | SCALE_LINEAR | SCALE_EQUILIBRATE);

    int ret = solve(lp);

    //Collect the dispatch profile and startup flags
    bool return_ok = ret == OPTIMAL || ret == SUBOPTIMAL;

    switch (ret)
    {
    case OPTIMAL:
        result_status = RESULT_STATUS::RS_OPTIMAL;
        break;
    case SUBOPTIMAL:
        result_status = RESULT_STATUS::RS_SUBOPTIMAL;
        break;
    case UNBOUNDED:
        result_status = RESULT_STATUS::RS_UNKNOWN_ERROR;
        break;
    case NUMFAILURE:
        result_status = RESULT_STATUS::RS_UNKNOWN_ERROR;
        break;
    case INFEASIBLE:
        result_status = RESULT_STATUS::RS_INFEASIBLE;
        break;
    }
    double objective = get_objective(lp);
    int ncols = get_Ncolumns(lp);

    REAL *vars = new REAL[ncols];
    get_variables(lp, vars);

    unordered_map<int, unordered_map<int, double> > results;

    for (int i = 0; i < ncols; i++)
    {
        char *colname = get_col_name(lp, i + 1);
        if (!colname) continue;
        int id, rec;
        try
        {
            std::vector<std::string> h_r = split(colname, ":");

            id = std::stoi(h_r.front());
            rec = std::stoi(h_r.back());
        }
        catch (...)
        {
            continue;
        }

        results[id][rec] = vars[i];
    }

    unordered_map<int, Heliostat*>* hmap = SF->getHeliostatsByID();
    std::set<Heliostat*> heliostat_set;

    for (unordered_map<int, unordered_map<int, double> >::iterator hres = results.begin(); hres != results.end(); hres++)
    {
        for (int j = 0; j < Nrec; j++)
        {
            if (hres->second.find(j) != hres->second.end())
            {
                Heliostat* h = hmap->at(hres->first);
                h->setPowerToReceiver(0.);
                double prec = hres->second[j];
                if (prec > 0.)
                {
                    h->setPowerToReceiver(h->getPowerToReceiver() + prec * power_allocs[hres->first][hres->second[j]]);
                    heliostat_set.insert(h);
                }
            }
        }
    }

    included_heliostats.clear();
    included_heliostats.reserve(heliostat_set.size());
    for (std::set<Heliostat*>::iterator h = heliostat_set.begin(); h != heliostat_set.end(); h++)
        included_heliostats.push_back(*h);

    return result_status;
}


//----------------------------------------------------------------------------------
//----------------------------------------------------------------------------------


optimization_vars::optimization_vars()
{
    current_mem_pos = 0;
    alloc_mem_size = 0;
}
void optimization_vars::add_var(const std::string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, REAL lobo, REAL upbo)
{
    if (var_dim == VAR_DIM::DIM_T2)
        add_var(vname, var_type, VAR_DIM::DIM_NT, var_dim_size, var_dim_size, lobo, upbo);
    else
        add_var(vname, var_type, var_dim, var_dim_size, 1, lobo, upbo);

}

void optimization_vars::add_var(const std::string &vname, int var_type /* VAR_TYPE enum */, int var_dim /* VAR_DIM enum */, int var_dim_size, int var_dim_size2, REAL lobo, REAL upbo)
{
    var_objects.push_back(optimization_vars::opt_var());
    optimization_vars::opt_var *v = &var_objects.back();
    v->name = vname;
    v->ind_start = current_mem_pos;
    v->var_type = var_type;
    v->var_dim = var_dim;
    v->var_dim_size = var_dim_size;
    v->var_dim_size2 = var_dim_size2;
    if (v->var_type == optimization_vars::VAR_TYPE::BINARY_T)
    {
        v->upper_bound = 1.;
        v->lower_bound = 0.;
    }
    else
    {
        v->upper_bound = upbo;
        v->lower_bound = lobo;
    }

    //calculate the required memory space for this type of variable
    int mem_size;
    switch (var_dim)
    {
    case optimization_vars::VAR_DIM::DIM_T:
        mem_size = var_dim_size;
        break;
    case optimization_vars::VAR_DIM::DIM_NT:
        mem_size = var_dim_size * var_dim_size2;
        break;
    case optimization_vars::VAR_DIM::DIM_T2:
        throw std::runtime_error("invalid var dimension in add_var");
    case optimization_vars::VAR_DIM::DIM_2T_TRI:
        mem_size = (var_dim_size + 1) * var_dim_size / 2;
        break;
    }

    v->ind_end = v->ind_start + mem_size;

    current_mem_pos += mem_size;


}

bool optimization_vars::construct()
{
    if (current_mem_pos < 0 || current_mem_pos > 1000000)
        throw std::runtime_error("Bad memory allocation when constructing variable table for dispatch optimization.");

    data = new REAL[current_mem_pos];

    alloc_mem_size = current_mem_pos;

    for (int i = 0; i < (int)var_objects.size(); i++)
        var_by_name[var_objects.at(i).name] = &var_objects.at(i);

    return true;
}

REAL &optimization_vars::operator()(char *varname, int ind)    //Access for 1D var
{
    return data[var_by_name[varname]->ind_start + ind];

}

REAL &optimization_vars::operator()(char *varname, int ind1, int)     //Access for 2D var
{
    return data[column(varname, ind1, ind1) - 1];
}

REAL &optimization_vars::operator()(int varind, int ind)    //Access for 1D var
{
    return data[var_objects.at(varind).ind_start + ind];

}

REAL &optimization_vars::operator()(int varind, int ind1, int ind2)     //Access for 2D var
{
    return data[column(varind, ind1, ind2) - 1];
}


int optimization_vars::column(const std::string &varname, int ind)
{
    return var_by_name[varname]->ind_start + ind + 1;
}

int optimization_vars::column(const std::string &varname, int ind1, int ind2)
{
    opt_var *v = var_by_name[std::string(varname)];
    switch (v->var_dim)
    {
    case VAR_DIM::DIM_T:
        throw std::runtime_error("Attempting to access optimization variable memory via 2D call when referenced variable is 1D.");
    case VAR_DIM::DIM_NT:
        return v->ind_start + v->var_dim_size2 * ind1 + ind2 + 1;
    default:
    {
        int ind = v->var_dim_size * ind1 + ind2 - ((ind1 - 1)*ind1 / 2);
        return v->ind_start + ind + 1;
    }
    break;
    }
}

int optimization_vars::column(int varindex, int ind)
{
    return var_objects[varindex].ind_start + ind + 1;
}

int optimization_vars::column(int varindex, int ind1, int ind2)
{
    opt_var *v = &var_objects[varindex];
    switch (v->var_dim)
    {
    case VAR_DIM::DIM_T:
        throw std::runtime_error("Attempting to access optimization variable memory via 2D call when referenced variable is 1D.");
    case VAR_DIM::DIM_NT:
        return v->ind_start + v->var_dim_size2 * ind1 + ind2 + 1;
    default:
    {
        int ind = v->var_dim_size * ind1 + ind2 - ((ind1 - 1)*ind1 / 2);
        return v->ind_start + ind + 1;
    } break;
    }
}

int optimization_vars::get_num_varobjs()
{
    return (int)var_objects.size();
}

int optimization_vars::get_total_var_count()
{
    return alloc_mem_size;
}

REAL *optimization_vars::get_variable_array()
{
    return data;
}

optimization_vars::opt_var *optimization_vars::get_var(const std::string &varname)
{
    return var_by_name[varname];
}

optimization_vars::opt_var *optimization_vars::get_var(int varindex)
{
    return &var_objects[varindex];
}