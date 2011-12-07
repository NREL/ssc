#ifndef __CMForm_h
#define __CMForm_h

#include <wx/wx.h>
#include "formalizer.h"

/*user.global.start*/
#include <wx/config.h>
#include <wx/aui/aui.h>
#include "dllinvoke.h"
#include "sscdev.h"
/*user.global.end*/

class CMForm : public wxPanel
{
public:
	CMForm(wxWindow *parent, int id=-1);
	virtual ~CMForm();
	/* class members */

	wxButton *btnSendToExcel;
	wxButton *btnClose;
	wxButton *btnAccept;
	AFLabel *Label2;
	wxStaticBox *GroupBox2;
	AFLabel *Label6;
	wxListBox *lstSelectedCMs;
	WFGridCtrl *grdCMVars;
	wxTextCtrl *txtParams;
	wxCheckListBox *cklCMList;
	wxStaticBox *GroupBox1;
	AFLabel *Label3;
	wxButton *btnParamAdd;
	wxButton *btnParamFile;
	AFTextCtrl *txtParamValue;
	AFLabel *Label5;
	wxButton *btnParamDir;
	AFRadioChoice *rbgParamType;
	wxButton *btnParamReset;
	wxButton *btnParamDel;
	wxListBox *lstParams;
	AFLabel *Label4;
	AFLabel *Label1;

/*user.class.start*/
	void LoadCMs();
	void OnCMListSelect(wxCommandEvent &evt);
	void SetCMList( const Array<cmModule> & list ) {
		m_cmList = list; UpdateForm(); }
	Array<cmModule> GetCMList() { return m_cmList; }

	static wxArrayString GetAvailableCMs();

private:
	void UpdateForm();
	void UpdateParamList();
	void UpdateParam();

	void OnSendToExcel(wxCommandEvent &);
	void OnCMListCheck(wxCommandEvent &evt);
	void OnSelCMSelect(wxCommandEvent &evt);
	void OnParamCommand(wxCommandEvent &evt);

	Array<cmModule> m_cmList;
/*user.class.end*/
	DECLARE_EVENT_TABLE()
};

class CMFormDialog : public wxDialog
{
public:
	CMFormDialog(wxWindow *parent, const wxString &title, void *data = NULL);

	CMForm *GetPanel() { return mPanel; }
	void OnCommand(wxCommandEvent &evt);
	void OnClose(wxCloseEvent &evt);
/*user.dialog.start*/
/*user.dialog.end*/
private:
	CMForm *mPanel;
	DECLARE_EVENT_TABLE()
};

#endif

