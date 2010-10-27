#ifndef __StatForm_h
#define __StatForm_h

#include <wx/wx.h>
#include "formalizer.h"

/*user.global.start*/
#include "dllinvoke.h"
/*user.global.end*/

class StatForm : public wxPanel
{
public:
	StatForm(wxWindow *parent, int id=-1);
	virtual ~StatForm();
	/* class members */

	wxButton *btnClose;
	wxStaticBox *GroupBox1;
	WFGridCtrl *grdMonthly;
	AFNumeric *numSumOver1000;
	AFLabel *Label11111;
	AFNumeric *numSum;
	AFLabel *Label1111;
	AFNumeric *numMax;
	AFNumeric *numMean;
	AFLabel *Label111;
	AFLabel *Label1;
	AFLabel *Label11;
	AFNumeric *numMin;

/*user.class.start*/
	void Compute( util::matrix_t<ssc_number_t> &val );
/*user.class.end*/
	DECLARE_EVENT_TABLE()
};

class StatFormDialog : public wxDialog
{
public:
	StatFormDialog(wxWindow *parent, const wxString &title, void *data = NULL);

	StatForm *GetPanel() { return mPanel; }
	void OnCommand(wxCommandEvent &evt);
	void OnClose(wxCloseEvent &evt);
/*user.dialog.start*/
	void Compute( util::matrix_t<ssc_number_t> &val ) { mPanel->Compute( val ); }
/*user.dialog.end*/
private:
	StatForm *mPanel;
	DECLARE_EVENT_TABLE()
};

#endif

