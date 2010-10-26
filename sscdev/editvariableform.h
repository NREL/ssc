#ifndef __EditVariableForm_h
#define __EditVariableForm_h

#include <wx/wx.h>
#include "formalizer.h"

/*user.global.start*/
#include "dllinvoke.h"
/*user.global.end*/

class EditVariableForm : public wxPanel
{
public:
	EditVariableForm(wxWindow *parent, int id=-1);
	virtual ~EditVariableForm();
	/* class members */

	wxButton *btnChooseFile;
	AFTextCtrl *txtValue;
	AFLabel *Label1;
	AFNumeric *numValue;
	AFLabel *Label11;
	AFRadioChoice *rbgVarType;
	wxButton *btnCancel;
	wxButton *btnAccept;
	WFGridCtrl *grdArrMat;
	AFNumeric *numCols;
	AFLabel *Label21;
	AFLabel *Label2;
	AFNumeric *numRows;

/*user.class.start*/
	void SetVarData( var_data &data ) { m_var = data; UpdateForm(); }
	void GetVarData( var_data &data ) { data = m_var; }
	void UpdateForm();

private:
	void OnTypeChange( wxCommandEvent &evt );
	void OnTextChange( wxCommandEvent &evt );
	void OnNumChange( wxCommandEvent &evt);
	void OnGridCellChange( wxGridEvent &evt );
	void OnRowsColsChange( wxCommandEvent &evt );
	void OnChooseFile( wxCommandEvent &evt );

	var_data m_var;
/*user.class.end*/
	DECLARE_EVENT_TABLE()
};

class EditVariableFormDialog : public wxDialog
{
public:
	EditVariableFormDialog(wxWindow *parent, const wxString &title, void *data = NULL);

	EditVariableForm *GetPanel() { return mPanel; }
	void OnCommand(wxCommandEvent &evt);
	void OnClose(wxCloseEvent &evt);
/*user.dialog.start*/
	void SetVarData( var_data &var ) { mPanel->SetVarData( var ); }
	void GetVarData( var_data &var ) { mPanel->GetVarData( var ); }
/*user.dialog.end*/
private:
	EditVariableForm *mPanel;
	DECLARE_EVENT_TABLE()
};

#endif

