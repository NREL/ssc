' based on version SAM 2010.11.9 defaults for PVWatts/Commercial Loan - updated to values below
'ssc_clear()
' update analysis period below to 30 to match with SAM defaults
energy_net   = array(398124,396134,394153,392182,390221,388270,386329,384397,382475,380563,378660,376767,374883,373009,371144,369288,367441,365604,363776,361957,360147,358347,356555,354772,352998,351233,349477,347730,345991,344261)
energy_value = array(95549.8,97924.3,100358,102852,105407,108027,110711,113462,116282,119172,122133,125168,128278,131466,134733,138081,141513,145029,148633,152327,156112,159991,163967,168042,172218,176497,180883,185378,189985,194706)

ssc_set("energy_value",energy_value)
ssc_set("energy_net", energy_net)
ssc_set("federal_tax_rate", 35.0)
ssc_set("state_tax_rate", 8.0)
ssc_set("real_discount_rate", 11.0)
ssc_set("insurance_rate", 0.5)
ssc_set("property_tax_rate", 2.0)
ssc_set("sales_tax_rate", 3.2)
ssc_set("inflation_rate", 3)
ssc_set("system_capacity", 250)
'ssc_set("total_cost", 937394.51)
ssc_set("total_hard_cost", 865100)
ssc_set("total_soft_cost", 71500)
ssc_set("percent_of_cost_subject_sales_tax", 2.87)
ssc_set("salvage_percentage", 50)


ssc_set("loan_term", 15)
ssc_set("loan_rate", 8)
ssc_set("loan_debt", 60)
'ssc_set("itc_fed_percent", array(30))
ssc_set("itc_fed_percent", 30)

ssc_set("depr_fed_type", 1) ' half year convention
ssc_set("depr_sta_type", 1) ' half year convention

ssc_set("om_fixed", array(3500))
ssc_set("om_capacity", array(20))


'ssc_set("itc_sta_percent", array(10,10,10,5))
ssc_set("itc_sta_percent", 10)
ssc_set("ptc_fed_amount", array(0.1))
ssc_set("ptc_fed_term", 10)
ssc_set("ptc_fed_escal", 1.5)

ssc_set("analysis_years", 30)

for (itc=30;itc<=30;itc=itc+10)
	outln("ITC=" + itc)
'	ssc_set("itc_fed_percent", array(itc))
	ssc_set("itc_fed_percent", itc)
	ssc_start()

	'outln("credit_basis_fed: " + ssc_get("credit_basis_fed"))
	'outln("credit_basis_sta: " + ssc_get("credit_basis_sta"))
	'outln("depr_basis_fed: " + ssc_get("depr_basis_fed"))
	'outln("depr_basis_sta: " + ssc_get("depr_basis_sta"))
	outln("\tnpv: " + ssc_get("npv"))
	outln("\tpayback: " + ssc_get("payback"))
	outln("\tlcoe_nom: " + ssc_get("lcoe_nom"))
	outln("\tlcoe_real: " + ssc_get("lcoe_real"))
end

'file = gethomedir() + "/cashloan_test.sscdat"
file = "cashloan_test.sscdat"
if (not ssc_save_state(file))	outln("failed to save ssc state") else outln("saved: " + file)

