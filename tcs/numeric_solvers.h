#ifndef __NUMERIC_SOLVERS_
#define __NUMERIC_SOLVERS_

class C_monotonic_eq_solver
{
private:

	// Values set in solver
	bool m_is_pos_bound;
	bool m_is_neg_bound;
	bool m_is_pos_error;
	bool m_is_neg_error;
	bool m_is_pos_error_prev;
	bool m_is_neg_error_prev;

	double m_x_guess;
	double m_x_neg_err;
	double m_x_pos_err;
	double m_y_err_pos;
	double m_y_err_neg;

	double m_y_err;
	int m_iter;
	int m_iter_limit;

	double check_against_limits(double x);

	double calc_x_intercept(double x1, double y1, double x2, double y2);

protected:
	double m_func_x_lower;		// Lower limit of independent variable
	double m_func_x_upper;		// Upper limit of independent variable

	double m_tol;			// Convergence error
	int m_iter_max;			// Maximum number of iterations allowed

	bool m_is_err_rel;		// Should error be calculated relative (true) or absolute (false)?

	// Pointer to a 1D monotonic function
	// Returns a value
	double(*mf_monotonic_function)(double x);

public:

	C_monotonic_eq_solver();

	~C_monotonic_eq_solver(){}

	virtual void initialize(double(*monotonic_function)(double x), double tol, int iter_limit, double x_lower, double x_upper, bool is_err_rel);

	void solve(double x_guess_1, double x_guess_2, double y_target,
		bool &is_converged, bool &is_real_error, double &x_solved, double &tol_solved, int &iter_solved);

	double test_member_function(double x_guess);
};





#endif