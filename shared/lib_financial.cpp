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


#include <cmath>
#include <limits>
#include "lib_financial.h"

using namespace libfin;

/* financial code here */
///* ported from http://code.google.com/p/irr-newtonraphson-calculator/ */
//
//static bool is_valid_iter_bound(double estimatedReturnRate)
//{
//	return estimatedReturnRate != -1 && (estimatedReturnRate < std::numeric_limits<int>::max()) && (estimatedReturnRate > std::numeric_limits<int>::min());
//}
//
//static double irr_poly_sum(double estimatedReturnRate, const std::vector<double> &CashFlows, int Count)
//{
//    double sumOfPolynomial = 0;
//    if (is_valid_iter_bound(estimatedReturnRate))
//	{
//		for (int j = 0; j < Count && j<(int)CashFlows.size(); j++)
//        {
//			double val = (pow((1 + estimatedReturnRate), j));
//			if (val != 0.0)
//				sumOfPolynomial += CashFlows[j]/val;
//			else
//				break;
//        }
//	}
//    return sumOfPolynomial;
//}
//
//static double irr_derivative_sum(double estimatedReturnRate, const std::vector<double> &CashFlows, int Count)
//{
//    double sumOfDerivative = 0;
//    if (is_valid_iter_bound(estimatedReturnRate))
//		for (int i = 1; i < Count && i < (int)CashFlows.size(); i++)
//        {
//            sumOfDerivative += CashFlows[i]*(i)/pow((1 + estimatedReturnRate), i);
//        }
//    return sumOfDerivative*-1;
//}
//
//double libfin::irr(double tolerance, int maxIterations, const std::vector<double> &CashFlows, int Count)
//{
///* Validation check - can write to log if move to FinModel or include SimModel
//	if ((count < 2) || (CashFlows[0] > 0))
//    {
//		Messages.Add( "Cash flow for the first period  must be negative and there should");
//    }
//*/
//	int numberOfIterations=0;
//	double calculatedIRR=0;
//	double initialGuess = 0.1; // 10% is default used in Excel IRR function
//
//	if (CashFlows.size() < 3)
//		return initialGuess;
//
//    if ( (Count > 1) && (CashFlows[0] <= 0))
//    {
//		double deriv_sum = irr_derivative_sum(initialGuess,CashFlows,Count);
//		if (deriv_sum != 0)
//			calculatedIRR = initialGuess - irr_poly_sum(initialGuess,CashFlows,Count)/deriv_sum;
//		else
//			return initialGuess;
//
//		numberOfIterations++;
//		while (!(std::abs(irr_poly_sum(calculatedIRR,CashFlows,Count)) <= tolerance) && (numberOfIterations < maxIterations))
//		{
//			deriv_sum = irr_derivative_sum(initialGuess,CashFlows,Count);
//			if (deriv_sum != 0.0)
//				calculatedIRR = calculatedIRR - irr_poly_sum(calculatedIRR,CashFlows,Count)/deriv_sum;
//			else
//				break;
//
//			numberOfIterations++;
//		}
//	}
//    return calculatedIRR;
//}


// from singleowner

/* ported from http://code.google.com/p/irr-newtonraphson-calculator/ */
bool is_valid_iter_bound(double estimated_return_rate)
{
    return estimated_return_rate != -1 && (estimated_return_rate < std::numeric_limits<int>::max()) && (estimated_return_rate > std::numeric_limits<int>::min());
}

//double irr_poly_sum(double estimated_return_rate, int cf_line, int count)
double irr_poly_sum(double estimated_return_rate, const std::vector<double>& cf, int count)
{
    double sum_of_polynomial = 0;
    if (is_valid_iter_bound(estimated_return_rate))
    {
        for (int j = 0; j <= count; j++)
        {
            double val = (pow((1 + estimated_return_rate), j));
            if (val != 0.0)
                sum_of_polynomial += cf.at(j) / val;  //sum_of_polynomial += cf.at(cf_line, j) / val;
            else
                break;
        }
    }
    return sum_of_polynomial;
}

//double irr_derivative_sum(double estimated_return_rate, int cf_line, int count)
double irr_derivative_sum(double estimated_return_rate, const std::vector<double>& cf, int count)
{
    double sum_of_derivative = 0;
    if (is_valid_iter_bound(estimated_return_rate))
        for (int i = 1; i <= count; i++)
        {
            sum_of_derivative += cf.at(i) * (i) / pow((1 + estimated_return_rate), i + 1);  //sum_of_derivative += cf.at(cf_line, i) * (i) / pow((1 + estimated_return_rate), i + 1);
        }
    return sum_of_derivative * -1;
}

//double irr_scale_factor(int cf_unscaled, int count)
double irr_scale_factor(const std::vector<double>& cf_unscaled, int count)
{
    // scale to max value for better irr convergence
    if (count < 1) return 1.0;
    int i = 0;
    //double max = std::abs(cf.at(cf_unscaled, 0));
    double max = std::abs(cf_unscaled.at(0));
    for (i = 0; i <= count; i++)
        if (std::abs(cf_unscaled.at(i)) > max) max = std::abs(cf_unscaled.at(i));  //if (std::abs(cf.at(cf_unscaled, i)) > max) max = std::abs(cf.at(cf_unscaled, i));
    return (max > 0 ? max : 1);
}

//bool is_valid_irr(int cf_line, int count, double residual, double tolerance, int number_of_iterations, int max_iterations, double calculated_irr, double scale_factor)
bool is_valid_irr(const std::vector<double>& cf, int count, double residual, double tolerance, int number_of_iterations, int max_iterations, double calculated_irr, double scale_factor)
{
    //double npv_of_irr = npv(cf_line, count, calculated_irr) + cf.at(cf_line, 0);
    double npv_of_irr = npv(cf, count, calculated_irr) + cf.at(0);
    //double npv_of_irr_plus_delta = npv(cf_line, count, calculated_irr + 0.001) + cf.at(cf_line, 0);
    double npv_of_irr_plus_delta = npv(cf, count, calculated_irr + 0.001) + cf.at(0);
    bool is_valid = ((number_of_iterations < max_iterations) && (std::abs(residual) < tolerance) && (npv_of_irr > npv_of_irr_plus_delta) && (std::abs(npv_of_irr / scale_factor) < tolerance));
    //if (!is_valid)
    //{
    //std::stringstream outm;
    //outm <<  "cf_line=" << cf_line << "count=" << count << "residual=" << residual << "number_of_iterations=" << number_of_iterations << "calculated_irr=" << calculated_irr
    //	<< "npv of irr=" << npv_of_irr << "npv of irr plus delta=" << npv_of_irr_plus_delta;
    //log( outm.str() );
    //}
    return is_valid;
}

//double irr_calc(int cf_line, int count, double initial_guess, double tolerance, int max_iterations, double scale_factor, int& number_of_iterations, double& residual)
double irr_calc(const std::vector<double>& cf, int count, double initial_guess, double tolerance, int max_iterations, double scale_factor, int& number_of_iterations, double& residual) {
    //		double calculated_irr = 0;
    double calculated_irr = std::numeric_limits<double>::quiet_NaN();
    //		double calculated_irr = -999;
    double deriv_sum = irr_derivative_sum(initial_guess, cf, count); //double deriv_sum = irr_derivative_sum(initial_guess, cf_line, count);
    if (deriv_sum != 0.0)
        calculated_irr = initial_guess - irr_poly_sum(initial_guess, cf, count) / deriv_sum;  //calculated_irr = initial_guess - irr_poly_sum(initial_guess, cf_line, count) / deriv_sum;
    else
        return initial_guess;

    number_of_iterations++;


    residual = irr_poly_sum(calculated_irr, cf, count) / scale_factor;  //residual = irr_poly_sum(calculated_irr, cf_line, count) / scale_factor;

    while (!(fabs(residual) <= tolerance) && (number_of_iterations < max_iterations))
    {
        deriv_sum = irr_derivative_sum(initial_guess, cf, count); //deriv_sum = irr_derivative_sum(initial_guess, cf_line, count);
        if (deriv_sum != 0.0)
            calculated_irr = calculated_irr - irr_poly_sum(calculated_irr, cf, count) / deriv_sum; //calculated_irr = calculated_irr - irr_poly_sum(calculated_irr, cf_line, count) / deriv_sum;
        else
            break;

        number_of_iterations++;
        residual = irr_poly_sum(calculated_irr, cf, count) / scale_factor;  //residual = irr_poly_sum(calculated_irr, cf_line, count) / scale_factor;
    }
    return calculated_irr;
}


//double libfin::irr(int cf_line, int count, double initial_guess = -2, double tolerance = 1e-6, int max_iterations = 100)
double libfin::irr(const std::vector<double>& cf, int count, double initial_guess, double tolerance, int max_iterations)
{
    int number_of_iterations = 0;
    //		double calculated_irr = 0;
    double calculated_irr = std::numeric_limits<double>::quiet_NaN();
    //		double calculated_irr = -999;


    if (count < 1)
        return calculated_irr;

    // only possible for first value negative
    //if ((cf.at(cf_line, 0) <= 0))
    if ((cf.at(0) <= 0))
    {
        // initial guess from http://zainco.blogspot.com/2008/08/internal-rate-of-return-using-newton.html
        if ((initial_guess < -1) && (count > 1))// second order
        {
            //if (cf.at(cf_line, 0) != 0)
            if (cf.at(0) != 0)
            {
                /*double b = 2.0 + cf.at(cf_line, 1) / cf.at(cf_line, 0);
                double c = 1.0 + cf.at(cf_line, 1) / cf.at(cf_line, 0) + cf.at(cf_line, 2) / cf.at(cf_line, 0);*/
                double b = 2.0 + cf.at(1) / cf.at(0);
                double c = 1.0 + cf.at(1) / cf.at(0) + cf.at(2) / cf.at(0);
                initial_guess = -0.5 * b - 0.5 * sqrt(b * b - 4.0 * c);
                if ((initial_guess <= 0) || (initial_guess >= 1)) initial_guess = -0.5 * b + 0.5 * sqrt(b * b - 4.0 * c);
            }
        }
        else if (initial_guess < 0) // first order
        {
            if (cf.at(0) != 0) initial_guess = -(1.0 + cf.at(1) / cf.at(0));  //if (cf.at(cf_line, 0) != 0) initial_guess = -(1.0 + cf.at(cf_line, 1) / cf.at(cf_line, 0));
        }

        double scale_factor = irr_scale_factor(cf, count);  //double scale_factor = irr_scale_factor(cf_line, count);
        double residual = DBL_MAX;

        calculated_irr = irr_calc(cf, count, initial_guess, tolerance, max_iterations, scale_factor, number_of_iterations, residual);  //calculated_irr = irr_calc(cf_line, count, initial_guess, tolerance, max_iterations, scale_factor, number_of_iterations, residual);

        //if (!is_valid_irr(cf_line, count, residual, tolerance, number_of_iterations, max_iterations, calculated_irr, scale_factor)) // try 0.1 as initial guess
        if (!is_valid_irr(cf, count, residual, tolerance, number_of_iterations, max_iterations, calculated_irr, scale_factor)) // try 0.1 as initial guess
        {
            initial_guess = 0.1;
            number_of_iterations = 0;
            residual = 0;
            calculated_irr = irr_calc(cf, count, initial_guess, tolerance, max_iterations, scale_factor, number_of_iterations, residual); //calculated_irr = irr_calc(cf_line, count, initial_guess, tolerance, max_iterations, scale_factor, number_of_iterations, residual);
        }

        //if (!is_valid_irr(cf_line, count, residual, tolerance, number_of_iterations, max_iterations, calculated_irr, scale_factor)) // try -0.1 as initial guess
        if (!is_valid_irr(cf, count, residual, tolerance, number_of_iterations, max_iterations, calculated_irr, scale_factor)) // try -0.1 as initial guess
        {
            initial_guess = -0.1;
            number_of_iterations = 0;
            residual = 0;
            calculated_irr = irr_calc(cf, count, initial_guess, tolerance, max_iterations, scale_factor, number_of_iterations, residual);  //calculated_irr = irr_calc(cf_line, count, initial_guess, tolerance, max_iterations, scale_factor, number_of_iterations, residual);
        }
        //if (!is_valid_irr(cf_line, count, residual, tolerance, number_of_iterations, max_iterations, calculated_irr, scale_factor)) // try 0 as initial guess
        if (!is_valid_irr(cf, count, residual, tolerance, number_of_iterations, max_iterations, calculated_irr, scale_factor)) // try 0 as initial guess
        {
            initial_guess = 0;
            number_of_iterations = 0;
            residual = 0;
            calculated_irr = irr_calc(cf, count, initial_guess, tolerance, max_iterations, scale_factor, number_of_iterations, residual); //calculated_irr = irr_calc(cf_line, count, initial_guess, tolerance, max_iterations, scale_factor, number_of_iterations, residual);
        }

        //if (!is_valid_irr(cf_line, count, residual, tolerance, number_of_iterations, max_iterations, calculated_irr, scale_factor)) // try 0.1 as initial guess
        if (!is_valid_irr(cf, count, residual, tolerance, number_of_iterations, max_iterations, calculated_irr, scale_factor)) // try 0.1 as initial guess
        {
            //				calculated_irr = 0.0; // did not converge
            calculated_irr = std::numeric_limits<double>::quiet_NaN(); // did not converge
            //				double calculated_irr = -999;
        }

    }
    return calculated_irr;
}




/*ported directly from Delphi simple geometric sum*/
//double libfin::npv(double Rate, const std::vector<double> &CashFlows, int Count) //, PaymentTime: TPaymentTime)
//{
//	/*
//{ Caution: The sign of NPV is reversed from what would be expected for standard
//   cash flows!}
//*/
//	if (Rate <= -1.0)
//	{
//		// TODO - throw exception?
//		return -999;
//	}
//
//	if (Count > (int)CashFlows.size())
//		Count = (int)CashFlows.size();
//
//	double rr = 1/(1+Rate);
//	double result = 0;
//	for (int i=Count-1;i>0;i--)
//	{
//		result = rr * result + CashFlows[i];
//	}
////	if PaymentTime = ptEndOfPeriod then result := rr * result;
//	return result*rr; // assumes end of period payments!!
//}

//double libfin::npv(int cf_line, int nyears, double rate)
double libfin::npv(const std::vector<double>& cf, int nyears, double rate)
{
    //if (rate == -1.0) throw general_error("cannot calculate NPV with discount rate equal to -1.0");
    double rr = 1.0;
    if (rate != -1.0) rr = 1.0 / (1.0 + rate);
    double result = 0;
    for (int i = nyears; i > 0; i--)
        result = rr * result + cf.at(i);  //result = rr * result + cf.at(cf_line, i);

    return result * rr;
}

double libfin::payback(const std::vector<double> &CumulativePayback, const std::vector<double> &Payback, int Count)
{
/*
Return payback in years of inputs streams
Payback occures when cumulative stream is > 0
Find exact payback by subtracting cumulative / payback
*/
  double dPayback = 1e99; // report as > analysis period
  bool bolPayback = false;
  int iPayback = 0;
  int i = 1;
  while ((i<Count) && (!bolPayback))
  {
    if (CumulativePayback[i] > 0)
	{
      bolPayback = true;
      iPayback = i;
	}
	i++;
  }

  if (bolPayback)
  {
	  if (Payback[iPayback] !=0)
	  {
		  dPayback = iPayback - CumulativePayback[iPayback] /Payback[iPayback];
	  }
	  else
	  {
		  dPayback = iPayback;
	  }
  }
  return dPayback;
}


/* code source http://www.linkedin.com/answers/technology/software-development/TCH_SFT/445353-4527099?browseCategory=TCH_SFT
Returns the payment on the principal for a given period for an investment based on periodic, constant payments and a constant interest rate.

Syntax

PPMT(rate,per,nper,pv,fv,type)

For a more complete description of the arguments in PPMT, see PV.

Rate   is the interest rate per period.

Per   specifies the period and must be in the range 1 to nper.

Nper   is the total number of payment periods in an annuity.

Pv   is the present value � the total amount that a series of future payments is worth now.

Fv   is the future value, or a cash balance you want to attain after the last payment is made. If fv is omitted, it is assumed to be 0 (zero), that is, the future value of a loan is 0.

Type   is the number 0 or 1 and indicates when payments are due.

Set type equal to If payments are due
0 or omitted At the end of the period
1 At the beginning of the period

Remark
Make sure that you are consistent about the units you use for specifying rate and nper. If you make monthly payments on a four-year loan at 12 percent annual interest, use 12%/12 for rate and 4*12 for nper. If you make annual payments on the same loan, use 12% for rate and 4 for nper.


*/
double libfin::pow1pm1 (double x, double y)
{
	return (x <= -1) ? pow (1 + x, y) - 1 : exp(y * log(1.0 + x)) - 1;
}
double libfin::pow1p (double x, double y)
{
	return (fabs (x) > 0.5) ? pow (1 + x, y) : exp (y * log(1.0 + x));
}
double libfin::fvifa (double rate, double nper)
{
	return (rate == 0) ? nper : pow1pm1 (rate, nper) / rate;
}

double libfin::pvif (double rate, double nper)
{
	return pow1p (rate, nper);
}

double libfin::pmt (double rate, double nper, double pv, double fv, int type)
{
	return ((-pv * pvif (rate, nper) - fv ) / ((1.0 + rate * type) * fvifa (rate, nper)));
}

double libfin::ipmt (double rate, double per, double nper, double pv, double fv, int type)
{
	double p = pmt (rate, nper, pv, fv, 0);
	double ip = -(pv * pow1p (rate, per - 1) * rate + p * pow1pm1 (rate, per - 1));
	return (type == 0) ? ip : ip / (1 + rate);
}

double libfin::ppmt (double rate, double per, double nper, double pv, double fv, int type)
{
	// exception identified 1/26/2010 by Aron and J.MacKnick when term (nper = 0)
	if (nper == 0) return 0.0;
	double p = pmt (rate, nper, pv, fv, type);
	double ip = ipmt (rate, per, nper, pv, fv, type);
	return p - ip;
}

// from http://www.codeproject.com/Articles/58289/C-Round-Function.aspx
long libfin::round_irs(double number)
{
    return (number >= 0) ? (long)(number + 0.5) : (long)(number - 0.5);
}
