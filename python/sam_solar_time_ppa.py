'''
example script for running pvsamv1
runs 1 sub-array, cec 5 par module, snl inverter
'''

from math import ceil

# web service input variables
latitude = 33
longitude = 104

# array parameters
ac_derate = 0.99
num_modules = 80

# module parameters
cec_area = 1.244
cec_alpha_sc = 2.651e-003
cec_beta_oc = -1.423e-001
cec_gamma_r = -4.070e-001
cec_i_mp_ref = 5.25
cec_i_sc_ref = 5.75
cec_n_s = 72
cec_t_noct = 49.2
cec_v_mp_ref = 41
cec_v_oc_ref = 47.7
cec_standoff = 6
cec_height = 0
cec_r_s = 0.105
cec_r_sh_ref = 160.48
cec_i_o_ref = 1.919e-010
cec_i_l_ref = 5.754
cec_adjust = 20.8
cec_a_ref = 1.9816

# inverter parameters
inv_snl_c0 = -6.57929e-006
inv_snl_c1 = 4.72925e-005
inv_snl_c2 = 0.00202195
inv_snl_c3 = 0.000285321
inv_snl_paco = 4000
inv_snl_pdco = 4186
inv_snl_pnt = 0.17
inv_snl_pso = 19.7391
inv_snl_vdco = 310.67
inv_snl_vdcmax = 0
inv_snl_vmin = 250
inv_snl_vmax = 480

tilt = 30
azimuth = 180
track_mode = 0
soiling = 0.95
dc_derate = 0.95558

# shading derate table
shading_mxh = [ [ 0,0,0,0,0,0,0,0,0.475,0.95,1,1,0.7875,0.2375,0.25,0.3625,0,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0,0,0,0.4875,1,1,1,0.925,0.6375,0.6625,0.225,0,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0,0,0.15,0.925,1,1,1,1,1,0.75,0.2,0,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0,0,0.45,0.9125,1,1,1,1,1,0.625,0.375,0,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0.075,0.05,0.7875,1,1,1,1,1,1,0.625,0.4875,0.025,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0.15,0.075,0.9,1,1,1,1,1,1,0.675,0.5,0.05,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0.1,0.0625,0.8375,1,1,1,1,1,1,0.6375,0.4875,0.025,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0,0,0.6625,0.9625,1,1,1,1,1,0.6125,0.4,0,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0,0,0.2,0.9125,1,1,1,1,1,0.7375,0.2125,0,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0,0,0.0625,0.7,1,1,1,0.9375,0.8,0.7,0.1875,0,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0,0,0,0.45,0.95,1,1,0.8125,0.3625,0.3625,0.375,0,0,0,0,0,0,0,0 ],
				[ 0,0,0,0,0,0,0,0.0125,0.525,0.95,1,0.9875,0.75,0.175,0.2125,0.275,0,0,0,0,0,0,0,0 ] ]

# out-years system performance
	  	  
analysis_years = 30
availability = 100
degradation = 0.5

# financial
federal_tax_rate = 35
state_tax_rate = 7
property_tax_rate = 1
prop_tax_cost_assessed_percent = 100
prop_tax_assessed_decline = 0
sales_tax_rate = 5
real_discount_rate = 5.2
inflation_rate = 2.5
insurance_rate = 0.5
system_capacity = 17.22
system_heat_rate = 0
loan_term = 30
loan_rate = 7
loan_debt = 50
optimize_lcoe_wrt_debt_fraction = 0
optimize_lcoe_wrt_ppa_escalation = 0
min_irr_target = 15
ppa_escalation = 0
salvage_percentage = 0
constr_total_financing = 1601.36
total_installed_cost = 80068.2
om_fixed = 0
om_fixed_escal = 0
om_production = 0
om_production_escal = 0
om_capacity = 20
om_capacity_escal = 0
om_fuel_cost = 0
om_fuel_cost_escal = 0
depr_fed_macrs = 1
depr_sta_macrs = 1

# incentives

itc_fed_amount = 0
itc_fed_amount_deprbas_fed = 1
itc_fed_amount_deprbas_sta = 1
itc_sta_amount = 0
itc_sta_amount_deprbas_fed = 0
itc_sta_amount_deprbas_sta = 0
itc_fed_percent = 30
itc_fed_percent_maxvalue = 1e99
itc_fed_percent_deprbas_fed = 1
itc_fed_percent_deprbas_sta = 1
itc_sta_percent = 0
itc_sta_percent_maxvalue = 1e99
itc_sta_percent_deprbas_fed = 0
itc_sta_percent_deprbas_sta = 0
ptc_fed_amount = [0]
ptc_fed_term = 10
ptc_fed_escal = 2
ptc_sta_amount = [0]
ptc_sta_term = 10
ptc_sta_escal = 2
ibi_fed_amount = 0
ibi_fed_amount_tax_fed = 1
ibi_fed_amount_tax_sta = 1
ibi_fed_amount_deprbas_fed = 0
ibi_fed_amount_deprbas_sta = 0
ibi_sta_amount = 0
ibi_sta_amount_tax_fed = 1
ibi_sta_amount_tax_sta = 1
ibi_sta_amount_deprbas_fed = 0
ibi_sta_amount_deprbas_sta = 0
ibi_uti_amount = 0
ibi_uti_amount_tax_fed = 1
ibi_uti_amount_tax_sta = 1
ibi_uti_amount_deprbas_fed = 0
ibi_uti_amount_deprbas_sta = 0
ibi_oth_amount = 0
ibi_oth_amount_tax_fed = 1
ibi_oth_amount_tax_sta = 1
ibi_oth_amount_deprbas_fed = 0
ibi_oth_amount_deprbas_sta = 0
ibi_fed_percent = 0
ibi_fed_percent_maxvalue = 1e99
ibi_fed_percent_tax_fed = 1
ibi_fed_percent_tax_sta = 1
ibi_fed_percent_deprbas_fed = 0
ibi_fed_percent_deprbas_sta = 0
ibi_sta_percent = 0
ibi_sta_percent_maxvalue = 1e99
ibi_sta_percent_tax_fed = 1
ibi_sta_percent_tax_sta = 1
ibi_sta_percent_deprbas_fed = 0
ibi_sta_percent_deprbas_sta = 0
ibi_uti_percent = 0
ibi_uti_percent_maxvalue = 1e99
ibi_uti_percent_tax_fed = 1
ibi_uti_percent_tax_sta = 1
ibi_uti_percent_deprbas_fed = 0
ibi_uti_percent_deprbas_sta = 0
ibi_oth_percent = 0
ibi_oth_percent_maxvalue = 1e99
ibi_oth_percent_tax_fed = 1
ibi_oth_percent_tax_sta = 1
ibi_oth_percent_deprbas_fed = 0
ibi_oth_percent_deprbas_sta = 0
cbi_fed_amount = 0
cbi_fed_maxvalue = 1e99
cbi_fed_tax_fed = 1
cbi_fed_tax_sta = 1
cbi_fed_deprbas_fed = 0
cbi_fed_deprbas_sta = 0
cbi_sta_amount = 0
cbi_sta_maxvalue = 1e99
cbi_sta_tax_fed = 1
cbi_sta_tax_sta = 1
cbi_sta_deprbas_fed = 0
cbi_sta_deprbas_sta = 0
cbi_uti_amount = 0
cbi_uti_maxvalue = 1e99
cbi_uti_tax_fed = 1
cbi_uti_tax_sta = 1
cbi_uti_deprbas_fed = 0
cbi_uti_deprbas_sta = 0
cbi_oth_amount = 0
cbi_oth_maxvalue = 1e99
cbi_oth_tax_fed = 1
cbi_oth_tax_sta = 1
cbi_oth_deprbas_fed = 0
cbi_oth_deprbas_sta = 0
pbi_fed_amount = [0]
pbi_fed_term = 10
pbi_fed_escal = 0
pbi_fed_tax_fed = 1
pbi_fed_tax_sta = 1
pbi_sta_amount = [0]
pbi_sta_term = 10
pbi_sta_escal = 0
pbi_sta_tax_fed = 1
pbi_sta_tax_sta = 1
pbi_uti_amount = [0]
pbi_uti_term = 10
pbi_uti_escal = 0
pbi_uti_tax_fed = 1
pbi_uti_tax_sta = 1
pbi_oth_amount = [0]
pbi_oth_term = 10
pbi_oth_escal = 0
pbi_oth_tax_fed = 1
pbi_oth_tax_sta = 1


# internal calculations to determine array electrical wiring
mod_power = cec_v_mp_ref * cec_i_mp_ref
num_series = 0.5 * (inv_snl_vmin + inv_snl_vmax) / cec_v_mp_ref

if (inv_snl_vdcmax > 0):
	while ((num_series > 0) and ((num_series * cec_v_oc_ref) > inv_snl_vdcmax)):
		num_series -= 1
		
if (num_series < 1):
	num_series = 1

num_series = int(num_series)

num_parallel = num_modules / num_series
if (num_parallel < 1):
	num_parallel = 1

num_inverters = ceil(num_series * num_parallel * mod_power / inv_snl_paco)
if (num_inverters < 1):
	num_inverters = 1

num_parallel = int( num_parallel )
num_inverters = int( num_inverters )


print num_parallel
print num_series
print num_inverters



# set the weather file.  the web service should take a 
# lat-long and use the perez satellite data or tmy2/3 data
# in the same way that the PVWatts service specifies the weather data
# --> essentially, this service and PVWatts should use exactly the same
#     method to get weather data for a location request

import ssc

data = ssc.Data()

data.set_string( 'weather_file', '../../examples/daggett.tm2' )
data.set_number( 'ac_derate', ac_derate )
data.set_number( 'modules_per_string', num_series )
data.set_number( 'strings_in_parallel', num_parallel )
data.set_number( 'inverter_count', num_inverters )
data.set_number( 'subarray1_tilt', tilt )
data.set_number( 'subarray1_azimuth', azimuth )
data.set_number( 'subarray1_track_mode', track_mode )
data.set_matrix( 'subarray1_shading_mxh', shading_mxh )
data.set_array( 'subarray1_soiling', [soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling, soiling ] )
data.set_number( 'subarray1_derate', dc_derate )

# set up values for other sub arrays - not used (currently)
data.set_number( 'subarray2_tilt', 0 )
data.set_number( 'subarray3_tilt', 0 )
data.set_number( 'subarray4_tilt', 0 )

data.set_number( 'module_model', 1 )

data.set_number( 'cec_area', cec_area )
data.set_number( 'cec_a_ref', cec_a_ref )
data.set_number( 'cec_adjust', cec_adjust )
data.set_number( 'cec_alpha_sc', cec_alpha_sc )
data.set_number( 'cec_beta_oc', cec_beta_oc )
data.set_number( 'cec_gamma_r', cec_gamma_r )
data.set_number( 'cec_i_l_ref', cec_i_l_ref )
data.set_number( 'cec_i_mp_ref', cec_i_mp_ref )
data.set_number( 'cec_i_o_ref', cec_i_o_ref )
data.set_number( 'cec_i_sc_ref', cec_i_sc_ref )
data.set_number( 'cec_n_s', cec_n_s )
data.set_number( 'cec_r_s', cec_r_s )
data.set_number( 'cec_r_sh_ref', cec_r_sh_ref )
data.set_number( 'cec_t_noct', cec_t_noct )
data.set_number( 'cec_v_mp_ref', cec_v_mp_ref )
data.set_number( 'cec_v_oc_ref', cec_v_oc_ref )
data.set_number( 'cec_temp_corr_mode', 0 )
data.set_number( 'cec_standoff', cec_standoff )
data.set_number( 'cec_height', cec_height )

data.set_number( 'inverter_model', 1 )

data.set_number( 'inv_snl_c0', inv_snl_c0 )
data.set_number( 'inv_snl_c1', inv_snl_c1 )
data.set_number( 'inv_snl_c2', inv_snl_c2 )
data.set_number( 'inv_snl_c3', inv_snl_c3 )
data.set_number( 'inv_snl_paco', inv_snl_paco )
data.set_number( 'inv_snl_pdco', inv_snl_pdco )
data.set_number( 'inv_snl_pnt', inv_snl_pnt )
data.set_number( 'inv_snl_pso', inv_snl_pso )
data.set_number( 'inv_snl_vdco', inv_snl_vdco )
data.set_number( 'inv_snl_vdcmax', inv_snl_vdcmax )

data.set_number( 'inverter_model', 0 )
data.set_number( 'inv_spe_efficiency', 0.95 )
data.set_number( 'inv_spe_power_ac', 4000 )

# all variables have been set up for pvsamv1
# run the model

mod = ssc.Module('pvsamv1')
if (mod.exec_(data)):
	print "sam_solar_time_ppa example ok"
else:
	idx = 0
	msg = mod.log(idx)
	while (msg is not None):
		     print "Error [", idx," ]: " , msg
		     idx += 1
		     msg = mod.log(idx)
	print "sam_solar_time_ppa example failed"
	


# return the relevant outputs desired

# ac_hourly = var('hourly_ac_net')
# ac_monthly = var('monthly_ac_net')
# ac_annual = var('annual_ac_net')

# outln( 'ac_monthly (kWh) = ' + ac_monthly)
# outln( 'ac_annual (kWh) = ' + ac_annual)

# end of PV performance calculations
