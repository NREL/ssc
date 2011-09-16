#ifndef __automation_h
#define __automation_h

#include <wx/wx.h>
#include <wx/scrolbar.h>
#include <wx/print.h>
#include <wx/listctrl.h>
#include <wx/printdlg.h>
#include <wx/snglinst.h>
#include <wx/config.h>
#include <wx/stc/stc.h>
#include <wx/aui/auibook.h>
#include <wx/aui/auibar.h>
#include <cml/codeedit.h>
#include <cml/afdirpanel.h>
#include <cml/afuiorgctrls.h>
#include <cml/sl_invoke.h>
#include <cml/mat2d.h>

class AutomationForm;

class SSEditorForm : public wxPanel
{
public:
	SSEditorForm( wxWindow *parent, AutomationForm *af );
	
	bool Load(const wxString &fn);
	bool Save();
	bool SaveAs();
	bool WriteToDisk(const wxString &fn, bool force=false);

	bool CanClose();
	wxString GetTitle();
	wxBitmap GetBitmapIcon();
	void BeforeDestroy();
	bool IsDirty();
	wxString GetFileName();
	CodeEdit *GetEditor() { return mEditor; }

private:

	AutomationForm *mAMForm;
	CodeEdit *mEditor;
	wxStatusBar *mStatusBar;
	wxString mFileName;

	void OnEditorModified(wxStyledTextEvent &evt);

	
	DECLARE_EVENT_TABLE()
};

class AutomationForm : public wxPanel
{
public:
	AutomationForm(wxWindow *parent);
	virtual ~AutomationForm();

	bool CloseEditors();

	SLInvokeLibrary *GetFuncLib() { return mFuncLib; }

	void New();
	void SaveAs();
	void Save();
	bool Load(const wxString &file=wxEmptyString);
	void Open();
	bool SaveAll();

	void Find();
	void FindNext();
	void Replace();

	void RunCurrent();
	void RunScript(const wxString &input, const wxString &filename=wxEmptyString);
	void UpdateTitle(SSEditorForm *ef);
	bool LocateLine(const wxString &file, long line);

	SSEditorForm *Active();
	int GetTabIdx(SSEditorForm *ef);
	Array<SSEditorForm*> GetEditorFormList();

	void OnFileSelect(wxCommandEvent &evt);
	void OnFileOpenApp(wxCommandEvent &evt);
	void OnCommand(wxCommandEvent &evt);
	void OnFuncInsert(wxCommandEvent &evt);
	void OnNotebookPageClose(wxAuiNotebookEvent& evt);
	void OnNotebookPageChange(wxAuiNotebookEvent& evt);

	bool ScriptRunning;
	bool AbortFlag;
	bool PauseFlag;
private:
	SLInvokeLibrary *mFuncLib;
	wxArrayString mFuncInsertList;

	wxAuiToolBar *ToolBar;
	wxAuiNotebook *Notebook;


	DECLARE_EVENT_TABLE();
};


#endif

