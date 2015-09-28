#include "lib_pv_shade_loss_mpp.h"
#include <functional>   // std::greater
#include <algorithm>    // std::sort
#include <math.h> // logarithm function

#include "lib_miniz.h" // decompression

#include "DB8_vmpp_impp_uint8_bin.h" // char* of binary compressed file


typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint;

short DB8_mpp::get_vmpp(size_t i)
{
	if (i >= 0 && i<6045840) // uint16 check
		return (short)((p_vmpp[2 * i + 1] << 8) | p_vmpp[2 * i]); 
	else 
		return -1;
};

short DB8_mpp::get_impp(size_t i)
{ 
	if (i >= 0 && i<6045840) // uint16 check
		return (short)((p_impp[2 * i + 1] << 8) | p_impp[2 * i]); 
	else 
		return -1; 
};


bool DB8_mpp::get_index(const size_t &N, const size_t &d, const  size_t &t, const size_t &S, const  db_type &DB_TYPE, size_t* ret_ndx)
{
	bool ret_val = false;
	//size_t ret_ndx=-1;
	size_t length=0, offset=0;
	size_t length_t =10, length_d=10;
	size_t iN = 0, id = 0, it = 0;

	// ret_ndx==0 is an error condition.
	// check N
	if ((N < 1) || (N>8)) return ret_val;
	// check d
	if ((d < 1) || (d>10)) return ret_val;
	// check t
	if ((t < 1) || (t>10)) return ret_val;

	// check S value for validity
	// find number of s vectors
	size_t size_s = n_choose_k(t + N - 1, t);
	if ((S < 1) || (S>size_s)) return ret_val;



	switch (DB_TYPE)
	{
		case VMPP:
			length = 8;
			offset = 0;
			break;
		case IMPP:
			length = 8;
			offset = p_vmpp_uint8_size / 2; // short offset
			break;
	}
	if (length == 0) return ret_val;
	*ret_ndx = 0; // independent vectors for vmpp,impp,vs and is so offset=0

	size_t t_ub = 11; // upper bound of t index for iteration
	size_t d_ub = 10; // upper bound of d index for iteration
	do
	{
		iN++;
		d_ub = ((iN == N) ? d : 10);
		id = 0;
		do
		{
			id++;
			t_ub = (((iN==N) && (id==d)) ? t : 11);
			for (it = 1; it < t_ub; it++)
			{
				// find number of s vectors
				size_s = n_choose_k(it + iN - 1, it);
				// multiply by length of each S vector
				*ret_ndx += size_s*length;
			}
		} while (id < d_ub);
	} while (iN < N);
	*ret_ndx += (S - 1)*length;
	ret_val = true;
	return ret_val;
}

size_t DB8_mpp::n_choose_k(size_t n, size_t k)
{
	if (k > n) return 0;
	if (k * 2 > n) k = n - k;
	if (k == 0) return 1;

	size_t result = n;
	for (size_t i = 2; i <= k; ++i) {
		result *= (n - i + 1);
		result /= i;
	}
	return result;
}

std::vector<double> DB8_mpp::get_vector(const size_t &N, const size_t &d, const size_t &t, const size_t &S, const db_type &DB_TYPE)
{
	std::vector<double> ret_vec;
	size_t length = 0;
	switch (DB_TYPE)
	{
	case VMPP:
		length = 8;
		break;
	case IMPP:
		length = 8;
		break;
	}
	if (length == 0) return ret_vec;
	size_t ndx;
	if (get_index(N, d, t, S, DB_TYPE, &ndx))
	{
		for (size_t i = 0; i < length; i++)
		{ // could replace with single get!
			if (DB_TYPE == VMPP)
				ret_vec.push_back((double)get_vmpp(ndx + i) / 1000.0);
			else if (DB_TYPE == IMPP)
				ret_vec.push_back((double)get_impp(ndx + i) / 1000.0);
		}
	}
	return ret_vec;
}

void DB8_mpp::init()
{
	p_vmpp_uint8_size = 12091680; // uint8 size from matlab
	p_impp_uint8_size = 12091680; // uint8 size from matlab
	p_vmpp = (unsigned char *)malloc(p_vmpp_uint8_size);//malloc(12091680); uint8 size
	p_impp = (unsigned char *)malloc(p_impp_uint8_size);//malloc(12091680); uint8 size
	p_compressed_size = 3133517; // from modified example5.c in miniz project
	decompress_file_to_uint8();
}

DB8_mpp::~DB8_mpp()
{
	if (p_vmpp)
		free(p_vmpp);
	if (p_impp)
		free(p_impp);
}



bool DB8_mpp::decompress_file_to_uint8()
{
	size_t status;
	uint8 *pTmp_data;

	size_t mem_size = p_vmpp_uint8_size + p_impp_uint8_size;

	pTmp_data = (uint8 *)malloc(mem_size);

	status = tinfl_decompress_mem_to_mem((void *)pTmp_data, mem_size, pCmp_data, p_compressed_size, TINFL_FLAG_PARSE_ZLIB_HEADER);

	memcpy(p_vmpp, pTmp_data, p_vmpp_uint8_size);
	memcpy(p_impp, pTmp_data + p_vmpp_uint8_size, p_impp_uint8_size);

	free(pTmp_data);

	if (status == TINFL_DECOMPRESS_MEM_TO_MEM_FAILED)
	{
		printf("tinfl_decompress_mem_to_mem() failed with status %i!\n", status);
		return EXIT_FAILURE;
	}

	return true;
};

double DB8_mpp::get_shade_loss(double &ghi, double &dhi, std::vector<double> &shade_frac)
{
	double shade_loss = 1;
	// shading fractions for each string
	size_t num_strings = shade_frac.size();
	// check for valid DB values
	if (dhi > ghi)
		dhi = ghi;
	if (num_strings > 0)
	{
		//Sort in descending order of shading
		std::sort(shade_frac.begin(), shade_frac.end(), std::greater<double>());
		//Need to round them to 10s (note should be integer)
		for (size_t i = 0; i < num_strings; i++)
			shade_frac[i] /= 10.0;
		std::vector<int> str_shade;
		for (size_t i = 0; i < num_strings; i++)
			str_shade.push_back((int)round(shade_frac[i]));
		int s_max = -1; // = str_shade[0]
		int s_sum = 0; // = str_shade[0] that is if first element zero then sum should be zero
		for (size_t i = 0; i < num_strings; i++)
		{
			if (str_shade[i] > s_max) s_max = str_shade[i];
			s_sum += str_shade[i];
		}
		//Now get the indices for the DB
		if ((s_sum > 0) && (ghi > 0))
		{
			int diffuse_frac = (int)round(dhi * 10.0 / ghi);
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



			std::vector<double>vmpp = get_vector(num_strings, diffuse_frac, s_max, counter, DB8_mpp::VMPP);
			std::vector<double>impp = get_vector(num_strings, diffuse_frac, s_max, counter, DB8_mpp::IMPP);
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
			shade_loss = (1.0 - p_max_frac);

		} //(sum >0)
		else // either shade frac sum = 0 or global = 0
		{
			if (s_sum <= 0) // to match with Matlab results
				shade_loss = 0.0;
			else
				shade_loss = 1.0;
		}
	}
	return shade_loss;
}
