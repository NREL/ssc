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

	AFTextCtrl *txtParamValue;
	AFLabel *Label5;
	wxButton *btnParamFile;
	wxButton *btnParamDir;
	wxStaticBox *GroupBox1;
	wxStaticBox *GroupBox2;
	AFLabel *Label6;
	wxListBox *lstSelectedCMs;
	AFRadioChoice *rbgParamType;
	wxButton *btnParamReset;
	wxButton *btnParamDel;
	wxButton *btnParamAdd;
	wxListBox *lstParams;
	wxButton *btnAccept;
	wxButton *btnClose;
	AFLabel *Label3;
	AFLabel *Label4;
	wxTextCtrl *txtParams;
	AFLabel *Label2;
	wxCheckListBox *cklCMList;
	WFGridCtrl *grdCMVars;
	AFLabel *Label1;

/*user.class.start*/
	void LoadCMs();
	void OnCMListSelect(wxCommandEvent &evt);
	void SetCMList( const Array<cmModule> & list ) {
		m_cmList = list; UpdateForm(); }
	Array<cmModule> GetCMList() { return m_cmList; }

private:
	void UpdateForm();
	void UpdateParamList();
	void UpdateParam();

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

