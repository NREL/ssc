#ifndef __CMForm_h
#define __CMForm_h

#include <wx/wx.h>
#include "formalizer.h"
#include <wx/config.h>
#include <wx/aui/aui.h>
#include "dllinvoke.h"
#include "sscdev.h"


class CMForm : public wxDialog
{
private:
	wxButton *btnSendToExcel;
	WFGridCtrl *grdCMVars;
	wxButton *btnAccept;
	wxButton *btnClose;
	AFLabel *Label6;
	wxListBox *lstSelectedCMs;
	AFLabel *Label3;
	AFLabel *Label2;
	wxCheckListBox *cklCMList;
	AFLabel *Label1;

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

