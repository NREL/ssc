#ifndef __CMForm_h
#define __CMForm_h

#include <wx/wx.h>
#include <wx/config.h>
#include <wx/grid.h>

#include "dllinvoke.h"
#include "sscdev.h"

class wxExtGridCtrl;

class CMForm : public wxPanel
{
private:
	wxExtGridCtrl *m_grid;
	wxChoice *m_currentCM;
	wxListBox *m_list;

public:
	CMForm( wxWindow *parent );	
	
	void LoadCMs();
	void SetCurrentCM( const wxString & cm ) { m_currentCM->SetStringSelection( cm ); }
	wxString GetCurrentCM() { return m_currentCM->GetStringSelection(); }

	wxArrayString GetAvailableCMs();

	void UpdateForm();
private:
	void OnRun( wxCommandEvent & );
	void OnCMListSelect(wxCommandEvent &evt);
	void OnCopyToClipboard(wxCommandEvent &);
	
	DECLARE_EVENT_TABLE()
};

#endif

