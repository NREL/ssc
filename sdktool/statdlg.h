#ifndef __StatForm_h
#define __StatForm_h

#include <wx/wx.h>
#include "dllinvoke.h"

class wxExtGridCtrl;
class wxNumericCtrl;

class StatDialog: public wxDialog
{
public:
	StatDialog(wxWindow *parent, const wxString &title);

	void Compute( util::matrix_t<ssc_number_t> &val );

private:
	wxExtGridCtrl *grdMonthly;
	wxNumericCtrl *numSumOver1000;
	wxNumericCtrl *numSum;
	wxNumericCtrl *numMax;
	wxNumericCtrl *numMean;
	wxNumericCtrl *numMin;
};

#endif

