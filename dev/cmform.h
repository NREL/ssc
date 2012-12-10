#ifndef __CMForm_h
#define __CMForm_h

#include <wx/wx.h>
#include <wx/config.h>
#include <wx/grid.h>

#include "dllinvoke.h"
#include "sscdev.h"

class wxExtGridCtrl;

class CMForm : public wxDialog
{
private:
	wxButton *btnSendToExcel;
	wxExtGridCtrl *grdCMVars;
	wxButton *btnAccept;
	wxButton *btnClose;
	wxListBox *lstSelectedCMs;
	wxCheckListBox *cklCMList;

public:
	CMForm(wxWindow *parent);	
	
	void LoadCMs();
	void OnCMListSelect(wxCommandEvent &evt);
	void SetCMList( const wxArrayString & list ) {
		m_cmList = list; UpdateForm(); }
	wxArrayString GetCMList() { return m_cmList; }

	static wxArrayString GetAvailableCMs();

private:
	void UpdateForm();
	void OnSendToExcel(wxCommandEvent &);
	void OnCMListCheck(wxCommandEvent &evt);

	wxArrayString m_cmList;

	DECLARE_EVENT_TABLE()
};

#endif

