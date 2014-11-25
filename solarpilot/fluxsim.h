#ifndef _FLUXSIM_
#define _FLUXSIM_ 1

#include "mod_base.h"

class FluxSimData : public mod_base
{
public:
    bool _is_cloud_pattern;
    bool _is_cloud_symd;
    bool _is_cloud_symw;
    bool _is_cloudy;
    bool _is_optical_err;
    bool _is_sunshape_err;
    bool _save_data;
    bool _is_autoscale;
    double _cloud_depth;
    double _cloud_loc_x;
    double _cloud_loc_y;
    double _cloud_opacity;
    double _cloud_sep_depth;
    double _cloud_sep_width;
    double _cloud_skew;
    double _cloud_width;
    double _flux_hour;
    double _flux_solar_az;
    double _flux_solar_az_in;
    double _flux_solar_el;
    double _flux_solar_el_in;
    double _flux_dni;
    double _norm_dist_sigma;
    double _sigma_limit;
    double _plot_zmax;
    double _plot_zmin;
    int _aim_method;
    int _cloud_shape;
    int _flux_day;
    int _flux_dist;
    int _flux_model;
    int _flux_month;
    int _flux_time_type;
    int _max_rays;
    int _min_rays;
    int _seed;
    int _x_res;
    int _y_res;
    string _class_name;
    string _save_data_loc;
    string _flux_data;

    struct CLOUD_SHAPE { enum A {ELLIPTICAL,RECTANGULAR,FRONT}; };
    struct FLUX_DIST { enum A {TRIANGULAR, NORMAL, UNIFORM};};
    struct FLUX_MODEL { enum A {HERMITE, SOLTRACE}; };
    struct FLUX_TIME { enum A {POSITION, CALENDAR}; };

    void Create(var_map &V);


};


#endif