// To duplicate functionality of GetShadeLoss.m from Sara for shade loss database
/* from GetShadeLoss.m:
GetShadeLoss(G,D,Tc,ModsPerString,StrShade,VMaxSTCStrUnshaded,VStrMPPT,ShadeDB )
% This searches the shade database and returns the %loss from partial shading
% G is the global POA irradiance, D is the diffuse irradiance, Tc is PV cell
% temperature, StrShade is a vector with each string's shaded fraction (like 24, 55, 12, etc preferably in terms of byp diode substrs),
%gammaPmp is the temperature coefficient of max power,
% reported in datasheet, VMaxSTCStrUnshaded is the unshaded Vmp of the string at STC,
% VStrMPPT is the lower and upper bounds of the inverter's MPPT range, and
% Shade DB is the database of shading losses (created by the DBX scripts at NREL)
*/
#include <functional>   // std::greater
#include <algorithm>    // std::sort
#include <math.h> // logarithm function

#include "core.h"
#include "lib_util.h"
#include "lib_pv_shade_loss_mpp.h"

static var_info _cm_vtab_pv_get_shade_loss_mpp[] = {
/*   VARTYPE           DATATYPE         NAME                           LABEL                                UNITS     META                      GROUP                      REQUIRED_IF                 CONSTRAINTS                      UI_HINTS*/
	{ SSC_INPUT, SSC_ARRAY, "global_poa_irrad", "Global POA irradiance", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_INPUT, SSC_ARRAY, "diffuse_irrad", "Diffuse irradiance", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_INPUT, SSC_MATRIX, "str_shade_fracs", "Shading fractions for each string", "", "", "PV Shade Loss DB", "*", "", "" },

	// testing indices from lookup
	{ SSC_OUTPUT, SSC_ARRAY, "N", "N", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "d", "d", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "t", "t", "", "", "PV Shade Loss DB", "*", "", "" },
	{ SSC_OUTPUT, SSC_ARRAY, "S", "S", "", "", "PV Shade Loss DB", "*", "", "" },


	/*  hourly or sub hourly shading for each string*/
	{ SSC_OUTPUT, SSC_ARRAY, "shade_loss", "Shade loss fraction", "", "", "PV Shade Loss DB", "*", "", "" },


var_info_invalid };

class cm_pv_get_shade_loss_mpp : public compute_module
{
public:

	cm_pv_get_shade_loss_mpp()
	{
		add_var_info(_cm_vtab_pv_get_shade_loss_mpp);
	}

	void exec() throw(general_error)
	{

		size_t nrec, count;
		ssc_number_t* global_poa_irrad = as_array("global_poa_irrad", &nrec);
		size_t step_per_hour = nrec / 8760;
		if (step_per_hour < 1 || step_per_hour > 60 || step_per_hour * 8760 != nrec)
			throw exec_error("pv_get_shade_loss_mpp", util::format("invalid number of global POA records (%d): must be an integer multiple of 8760", (int)nrec));
		double ts_hour = 1.0 / step_per_hour;

		ssc_number_t* diffuse_irrad = as_array("diffuse_irrad", &count);
		if (count != nrec)
			throw exec_error("pv_get_shade_loss_mpp", util::format("invalid number of diffuse records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));

		util::matrix_t<double> str_shade_fracs = as_matrix("str_shade_fracs");
		count = str_shade_fracs.nrows();
		size_t num_strings = str_shade_fracs.ncols();
		if (count != nrec)
			throw exec_error("pv_get_shade_loss_mpp", util::format("invalid number of mocules per string records (%d): must be equal to other input array sizes (%d)", (int)count, (int)nrec));

// start here
		ssc_number_t *N = allocate("N", nrec);
		ssc_number_t *d = allocate("d", nrec);
		ssc_number_t *t = allocate("t", nrec);
		ssc_number_t *S = allocate("S", nrec);

		ssc_number_t *shade_loss = allocate("shade_loss", nrec);


		if (num_strings > 0)
		{
			DB8_mpp db8;
			db8.init();

			for (size_t irec = 0; irec < nrec; irec++)
			{
//				shade_loss[irec] = 1;
				shade_loss[irec] = 0;
				//				if (global_poa_irrad[irec] > 0)
//				{
					// shading fractions for each string
					std::vector<double> dbl_str_shade;
					for (size_t ins = 0; ins < num_strings; ins++)
						dbl_str_shade.push_back(str_shade_fracs.at(irec,ins));

					//Sort in descending order of shading
					std::sort(dbl_str_shade.begin(), dbl_str_shade.end(), std::greater<double>());
					//Need to round them to 10s (note should be integer)
					for (size_t i = 0; i < num_strings; i++)
						dbl_str_shade[i] /= 10.0;
					std::vector<int> str_shade;
					for (size_t i = 0; i < num_strings; i++)
						str_shade.push_back((int)round(dbl_str_shade[i]));
					//			str_shade.push_back((int)dbl_str_shade[i]);
					int s_max = -1; // = str_shade[0]
					int s_sum = 0; // = str_shade[0] that is if first element zero then sum should be zero
					for (size_t i = 0; i < num_strings; i++)
					{
						if (str_shade[i] > s_max) s_max = str_shade[i];
						s_sum += str_shade[i];
					}
					//Now get the indices for the DB
//					if (s_sum > 0)
					if ((s_sum > 0) && (global_poa_irrad[irec] > 0))
					{
						int diffuse_frac = (int)round(diffuse_irrad[irec]*10.0 / global_poa_irrad[irec] );
						if (diffuse_frac < 1) diffuse_frac = 1;
						int counter = 1;
						bool found = false;
						if (num_strings > 1)
						{
							counter = 0;
							for (int i2 = 0; i2 <= s_max; i2++)
							{
								if (num_strings == 2)
								{
									counter++;
									std::vector<int> cur_case{ s_max, i2 };
									if (str_shade == cur_case)
										found = true;
								}
								else
								{
									for (int i3 = 0; i3 <= i2; i3++)
									{
										if (num_strings == 3)
										{
											counter++;
											std::vector<int> cur_case{ s_max, i2, i3 };
											if (str_shade == cur_case)
												found = true;
										}
										else
										{
											for (int i4 = 0; i4 <= i3; i4++)
											{
												if (num_strings == 4)
												{
													counter++;
													std::vector<int> cur_case{ s_max, i2, i3, i4 };
													if (str_shade == cur_case)
														found = true;
												}
												else
												{
													for (int i5 = 0; i5 <= i4; i5++)
													{
														if (num_strings == 5)
														{
															counter++;
															std::vector<int> cur_case{ s_max, i2, i3, i4, i5 };
															if (str_shade == cur_case)
																found = true;
														}
														else
														{
															for (int i6 = 0; i6 <= i5; i6++)
															{
																if (num_strings == 6)
																{
																	counter++;
																	std::vector<int> cur_case{ s_max, i2, i3, i4, i5, i6 };
																	if (str_shade == cur_case)
																		found = true;
																}
																else
																{
																	for (int i7 = 0; i7 <= i6; i7++)
																	{
																		if (num_strings == 7)
																		{
																			counter++;
																			std::vector<int> cur_case{ s_max, i2, i3, i4, i5, i6, i7 };
																			if (str_shade == cur_case)
																				found = true;
																		}
																		else
																		{
																			for (int i8 = 0; i8 <= i7; i8++)
																			{
																				if (num_strings == 8)
																				{
																					counter++;
																					std::vector<int> cur_case{ s_max, i2, i3, i4, i5, i6, i7, i8 };
																					if (str_shade == cur_case)
																						found = true;
																				}
																				else
																				{
																					// error message or throw error
																					counter = 0;
																				}
																			} // for i7
																			if (found) break;

																		}
																	} // for i7
																	if (found) break;
																}
															} // for i6
															if (found) break;
														}
													} // for i5
													if (found) break;
												}
											} // for i4
											if (found) break;
										}
									} // for i3
									if (found) break;
								}
								if (found) break;
							} // for i2
						} // (num_strings > 1)


						N[irec] = (ssc_number_t)num_strings;
						d[irec] = (ssc_number_t)diffuse_frac;
						t[irec] = (ssc_number_t)s_max;
						S[irec] = (ssc_number_t)counter;

						std::vector<double>vmpp = db8.get_vector(num_strings, diffuse_frac, s_max, counter, DB8_mpp::VMPP);
						std::vector<double>impp = db8.get_vector(num_strings, diffuse_frac, s_max, counter, DB8_mpp::IMPP);
						double p_max_frac = 0;
						int p_max_ind = 0;
						for (size_t i = 0; i < vmpp.size() && i < impp.size(); i++)
						{
							double pmp = vmpp[i] * impp[i];
							if (pmp > p_max_frac)
							{
								p_max_frac = pmp;
							}
						}
					// The global max power point is in range!
						shade_loss[irec] = (ssc_number_t)(1 - p_max_frac);

					} //(sum >0)
					else // either shade frac sum = 0 or global = 0
					{
						if (s_sum <=0 ) // to match with Matlab results
							shade_loss[irec] = 0;
						else
							shade_loss[irec] = 0;
//						shade_loss[irec] = 1;
					}
//				} //(global > 0)
			}// for irec
		} //  (num_strings > 0) 
		else
		{
			log(util::format("no DB loaded num strings = %d", num_strings), SSC_WARNING);
		}

		/*
		  
			Veemax = TcVmps(Pmaxind);
		if and(VStrMPPT(1) <= Veemax, VStrMPPT(2) >= Veemax)
			% The global max power point is in range!
			ShadeLoss = 1 - PmaxFrac;
		elseif isempty(PFracs(and(TcVs >= VStrMPPT(1), TcVs <= VStrMPPT(2))))
			ShadeLoss = 1;
		else
			%    %The global max power point is NOT in range
			ShadeLoss = 1 - max(PFracs(and(TcVs >= VStrMPPT(1), TcVs <= VStrMPPT(2))));
		end
		*/

	}
};

DEFINE_MODULE_ENTRY(pv_get_shade_loss_mpp, "PV get shade loss fraction for strings from Sara MacAlpine", 1)
