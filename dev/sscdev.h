#ifndef __SC_h
#define __SC_h

#include <wx/wx.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>

#include "dllinvoke.h"

class DataView;
class SCFrame;
class SCDocWin;
class wxNotebook;
class EditorWindow;
class CMForm;

extern SCFrame *app_frame;
extern wxConfig *app_config;

void applog(const wxString &s);
void applog(const char *fmt, ...);

extern int SC_major_ver;
extern int SC_minor_ver;
extern int SC_micro_ver;

class SCApp : public wxApp
{
public:
	virtual bool OnInit();
	virtual int OnExit();
};

DECLARE_APP(SCApp)

#define MAX_RECENT 9
class SCFrame : public wxFrame
{
public:
	SCFrame();
	virtual ~SCFrame();
		
	bool CloseDocument();
	bool LoadBdat( wxString fn = wxEmptyString );
	void SaveBdat();
	bool WriteBdatToDisk(const wxString &fn);
	
	void ChooseDynamicLibrary();
	void LoadUnloadLibrary();

	void Start();
	void ClearLog();
	void Log(const wxString &, bool wnl=true);
	
	void AddRecent(const wxString &fn);
	void RemoveRecent(const wxString &fn);
	void UpdateRecentMenu();
	wxArrayString GetRecentFiles();
	
	static void Copy( ssc_data_t p_data, var_table *vt, bool clear_first );
	static void Copy( var_table *vt,  ssc_data_t p_data, bool clear_first );

	void Progress(const wxString &text, float percent);

	wxString LastFileName() { return m_lastFile; }

	DataView *GetDataView() { return m_dataView; }
	var_table *GetVarTable() { return m_varTable; }

	void ClearCMs();
	bool AddCM( const wxString &name );
	void SetCMs( const wxArrayString &list );

private:	
	void WriteVarTable( wxDataOutputStream &o, var_table &vt );
	bool ReadVarTable( wxDataInputStream &o, var_table &vt, bool clear_first );

	void UpdateUI();

	void OnCommand(wxCommandEvent &evt);
	void OnRecent(wxCommandEvent &evt);
	void OnCloseFrame(wxCloseEvent &evt);

	wxMenu *m_recentMenu;
	wxString m_currentAppDir;
	wxString m_lastFile;
	
	wxString m_lastLoadTime;
	wxString m_dllPath;

	wxTextCtrl *m_txtOutput;

	wxMenu *m_fileMenu, *m_helpMenu;

	wxNotebook *m_notebook;
	CMForm *m_cmBrowser;
	DataView *m_dataView;
	EditorWindow *m_scriptWindow;

	wxArrayString m_cmList;
	var_table *m_varTable;

	int m_recentCount;
	wxString m_recentFiles[MAX_RECENT];
	
	DECLARE_EVENT_TABLE()
};

#endif
