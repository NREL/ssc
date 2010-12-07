' energy values used to match with DHF Team SAM Model Update - V3.xlsm
energy_net   = array(17000000,16915000,16830425,16746273,16662542,16579229,16496333,16413851,16331782,16250123,16168872,16088028,16007588,15927550,15847912,15768672,15689829,15611380,15533323,15455656,15378378,15301486,15224979,15148854,15073110,14997744,14922755,14848142,14773901,14700031,14626531,14553399,14480632,14408228,14336187,14264506,14193184,14122218,14051607,13981349)

ssc_set("energy_net", energy_net)
ssc_set("federal_tax_rate", 35.0)
ssc_set("state_tax_rate", 5.5)
' inflation = 1.5% and nominal = 8%
ssc_set("real_discount_rate", 6.403940887) 
ssc_set("insurance_rate", 0.5)
ssc_set("property_tax_rate", 0.75)
ssc_set("sales_tax_rate", 5.5)
ssc_set("inflation_rate", 1.5)
ssc_set("system_capacity", 10000)
ssc_set("ppa_price_input", 34.7539948813069)
ssc_set("ppa_escalation", 2)
ssc_set("salvage_percentage", 10)

' test equipment reserves
ssc_set("equip1_reserve_freq",12)
ssc_set("equip1_reserve_cost",0.25)
ssc_set("equip2_reserve_freq",15)
ssc_set("equip2_reserve_cost",0.5)
ssc_set("equip3_reserve_freq",3)
ssc_set("equip3_reserve_cost",0.01)


' test bonus depreciation
ssc_set("depr_bonus_sta", 5)
ssc_set("depr_bonus_sta_macrs_5",1)
ssc_set("depr_bonus_fed", 10)
ssc_set("depr_bonus_fed_macrs_5",1)

ssc_set("loan_term", 15)
ssc_set("loan_rate", 8)
ssc_set("loan_debt", 60)

ssc_set("depr_fed_type", 1) ' half year convention
ssc_set("depr_sta_type", 1) ' half year convention

ssc_set("om_fixed", array(10000))
ssc_set("om_fixed_escal", 1)
ssc_set("om_capacity", array(22))
ssc_set("om_capacity_escal", 0.75)
ssc_set("om_production", array(3.5))


ssc_set("itc_sta_amount", array(20000))
'ssc_set("itc_sta_percent", array(10))
ssc_set("itc_sta_percent", array(0))
ssc_set("ibi_sta_percent_itcbas_sta", 1)
ssc_set("ibi_sta_amount_itcbas_sta", 1)
ssc_set("itc_sta_percent_deprbas_sta",1)
ssc_set("itc_sta_amount_deprbas_sta",1)
ssc_set("itc_sta_percent_deprbas_fed",0)
ssc_set("itc_sta_amount_deprbas_fed",0)

ssc_set("itc_fed_percent", array(30))
ssc_set("itc_fed_amount", array(10000))
'ssc_set("itc_fed_percent", array(30))
ssc_set("itc_fed_percent", array(0))
ssc_set("ibi_fed_percent_itcbas_sta", 1)
ssc_set("ibi_fed_amount_itcbas_sta", 1)
ssc_set("itc_fed_percent_deprbas_sta",0)
ssc_set("itc_fed_amount_deprbas_sta",0)
ssc_set("itc_fed_percent_deprbas_fed",1)
ssc_set("itc_fed_amount_deprbas_fed",1)

ssc_set("ibi_sta_amount", array(2000))
ssc_set("ibi_sta_percent", array(10))
ssc_set("ibi_sta_amount_itcbas_sta", 1)
ssc_set("ibi_sta_amount_itcbas_fed", 1)
ssc_set("ibi_sta_percent_itcbas_fed", 0)
ssc_set("ibi_sta_percent_itcbas_sta", 0)
ssc_set("ibi_sta_amount_deprbas_sta", 0)
ssc_set("ibi_sta_amount_deprbas_fed", 0)
ssc_set("ibi_sta_percent_deprbas_fed", 0)
ssc_set("ibi_sta_percent_deprbas_sta", 0)

ssc_set("ibi_fed_amount", array(1000))
ssc_set("ibi_fed_percent", array(5))
ssc_set("ibi_fed_amount_itcbas_fed", 1)
ssc_set("ibi_fed_amount_itcbas_sta", 1)
ssc_set("ibi_fed_percent_itcbas_fed", 0)
ssc_set("ibi_fed_percent_itcbas_sta", 0)
ssc_set("ibi_fed_amount_deprbas_sta", 0)
ssc_set("ibi_fed_amount_deprbas_fed", 0)
ssc_set("ibi_fed_percent_deprbas_fed", 0)
ssc_set("ibi_fed_percent_deprbas_sta", 0)

ssc_set("ibi_uti_amount", array(500))
ssc_set("ibi_uti_percent", array(1))
ssc_set("ibi_uti_amount_deprbas_sta", 0)
ssc_set("ibi_uti_amount_deprbas_fed", 0)
ssc_set("ibi_uti_percent_deprbas_fed", 0)
ssc_set("ibi_uti_percent_deprbas_sta", 0)

ssc_set("ibi_oth_amount", array(250))
ssc_set("ibi_oth_percent", array(0.5))
ssc_set("ibi_oth_amount_deprbas_sta", 0)
ssc_set("ibi_oth_amount_deprbas_fed", 0)
ssc_set("ibi_oth_percent_deprbas_fed", 0)
ssc_set("ibi_oth_percent_deprbas_sta", 0)

ssc_set("cbi_fed_amount", array(0.5))
ssc_set("cbi_sta_amount", array(0.2))
ssc_set("cbi_uti_amount", array(0.01))
ssc_set("cbi_oth_amount", array(0.005))
ssc_set("cbi_fed_itcbas_fed", 1)
ssc_set("cbi_fed_itcbas_sta", 0)
ssc_set("cbi_fed_deprbas_fed", 0)
ssc_set("cbi_fed_deprbas_sta", 0)
ssc_set("cbi_sta_deprbas_fed", 0)
ssc_set("cbi_sta_deprbas_sta", 0)
ssc_set("cbi_uti_deprbas_fed", 0)
ssc_set("cbi_uti_deprbas_sta", 0)
ssc_set("cbi_oth_deprbas_fed", 0)
ssc_set("cbi_oth_deprbas_sta", 0)


ssc_set("ptc_fed_amount", array(0))
'ssc_set("ptc_fed_term", 10)
'ssc_set("ptc_fed_escal", 1.5)

ssc_set("analysis_years", 30)

ssc_start()
