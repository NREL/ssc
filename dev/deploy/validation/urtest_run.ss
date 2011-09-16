f1 = open("C:/Documents and Settings/adobos/Desktop/SVN/sscdev/deploy/validation/urtest_8760_e_sys.csv", "r")
f2 = open("C:/Documents and Settings/adobos/Desktop/SVN/sscdev/deploy/validation/urtest_8760_e_load.csv", "r")
if (not f1 or not f2)
	outln("could not open e_sys data")
	exit
end

declare buf,esys[8760],eload[8760]
readln(f1, buf)
readln(f2, buf)
e_annual=0
for (i=0;i<8760;i=i+1)
	readfmt(f1, "g", ", ", esys[i])
	readfmt(f2, "g", ", ", eload[i])
	eload[i] = -eload[i]
	e_annual = e_annual + esys[i]
end
close(f1)
close(f2)


ssc_clear()
ssc_set("e_with_system", esys)
ssc_set("e_without_system", eload)
ssc_set("system_degradation", array(0.5))
ssc_set("load_escalation", array(0.9))
ssc_set("rate_escalation", array(1.5))
ssc_set("ur_monthly_fixed_charge", 50)
ssc_set("analysis_years", 30)
ssc_set("ur_flat_buy_rate", 0.12)

ssc_set("ur_tou_enable",1)
ssc_set("ur_tou_p1_buy_rate", 0.12)
ssc_set("ur_tou_p2_buy_rate", 0.556)
ssc_set("ur_tou_p3_buy_rate", 0.75)
ssc_set("ur_tou_p4_buy_rate", 0.99)
ssc_set("ur_tou_sched_weekday", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111222222222222222222222222222222224444444442222222222222224444444442222222333333334444444443333333333333333333333333333333111111111111111111111111111111111111111111111111")
ssc_set("ur_tou_sched_weekend", "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111222222222222222222222222222222222222222222222222222222222222222222222222111111111111111111111111111111111111111111111111111111111111111111111111")

ssc_set("ur_dc_enable", 1)
ssc_set("ur_dc_sched_weekday", "444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444")
ssc_set("ur_dc_sched_weekend", "444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444")
ssc_set("ur_dc_fixed_m1", 10)
ssc_set("ur_dc_fixed_m2", 10)
ssc_set("ur_dc_fixed_m3", 10)
ssc_set("ur_dc_fixed_m4", 10)
ssc_set("ur_dc_fixed_m5", 20)
ssc_set("ur_dc_fixed_m6", 20)
ssc_set("ur_dc_fixed_m7", 30)
ssc_set("ur_dc_fixed_m8", 30)
ssc_set("ur_dc_fixed_m9", 30)
ssc_set("ur_dc_fixed_m10", 20)
ssc_set("ur_dc_fixed_m11", 10)
ssc_set("ur_dc_fixed_m12", 10)

ssc_set("ur_dc_p1", 11)
ssc_set("ur_dc_p2", 4)
ssc_set("ur_dc_p3", 3)
ssc_set("ur_dc_p4", 2)

ssc_set("ur_tr_enable", 1)
ssc_set("ur_tr_s1_energy_ub1", 100)
ssc_set("ur_tr_s1_energy_ub2", 300)
ssc_set("ur_tr_s1_rate1", 0.1)
ssc_set("ur_tr_s1_rate2", 0.2)
ssc_set("ur_tr_s1_rate3", 0.3)



ssc_start()
outln("e_annual (year 1)=" + e_annual)
outln("net energy   : ",ssc_get("energy_net"))
outln("net value   : ",ssc_get("energy_value"))
outln()
outln("with sys    : ",ssc_get("revenue_with_system"))
outln("without sys : ",ssc_get("revenue_without_system"))

outln("dc fixed wsys   : ",ssc_get("year1_monthly_dc_fixed_with_system"))
outln("dc fixed wosys  : ",ssc_get("year1_monthly_dc_fixed_without_system"))
outln("tr charge wsys  : ",ssc_get("year1_monthly_tr_charge_with_system"))
outln("tr charge wosys : ",ssc_get("year1_monthly_tr_charge_without_system"))
