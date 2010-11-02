#ifndef __SC_h
#define __SC_h

#include <wx/wx.h>
#include "dllinvoke.h"

class wxAuiToolBar;
class wxSingleInstanceChecker;
class AFLabel;
class DataView;

class SCFrame;
class SCDocWin;

extern SCFrame *app_frame;
extern wxArrayString app_args;
extern wxConfig *app_config;

void applog(const wxString &s);
void applog(const char *fmt, ...);

extern int SC_major_ver;
extern int SC_minor_ver;
extern int SC_micro_ver;

/***********************************************************/

class SCApp : public wxApp
{
public:
	SCApp();
	bool OnInit();
	int OnExit();	

	wxString GetInstanceName();
	void OnFatalException();
private:
	wxString m_inst_name;
	wxSingleInstanceChecker *m_inst_checker;
};

DECLARE_APP(SCApp)

/***********************************************************/

class SCAbout : public wxDialog
{
public:
	SCAbout(wxWindow *parent);
	void OnClose(wxCommandEvent &evt);
	void OnCloseFrame(wxCloseEvent &evt);

private:
	wxStaticText *mLblVersion;
	wxStaticBitmap *mBitmap;
	wxButton *mBtnClose;
	wxTextCtrl *mText;

	void OnCrash(wxCommandEvent &evt);

	DECLARE_EVENT_TABLE()
};


/***********************************************************/

// possible notifications to SCDocWin windows via 'virtual OnNotification(int)'
enum { NOTIFY_SAVE, NOTIFY_TABCHANGE };

#define MAX_RECENT 9

struct cmParam {
	wxString name;
	int type;
	wxString str;
	float num;
};

struct cmModule {
	wxString cm_mod_name;
	Array<cmParam> params;
};

class SCFrame : public wxFrame
{
public:
	SCFrame();
	virtual ~SCFrame();
	
	bool Load(const wxString &fn);
	bool WriteToDisk(const wxString &fn);
	bool CloseDocument();
	void Open();
	void Save();
	void SaveAs();
	void Exit();
	
	void Start();
	void Log(const wxString &);
	

	void AddRecent(const wxString &fn);
	void RemoveRecent(const wxString &fn);
	void UpdateRecentMenu();
	wxArrayString GetRecentFiles();
	
	static void Copy( ssc_data_t p_data, var_table *vt, bool clear_first );
	static void Copy( var_table *vt,  ssc_data_t p_data, bool clear_first );

	void Progress(const wxString &text, float percent);
private:	
	void UpdateUI();

	void OnCommand(wxCommandEvent &evt);
	void OnRecent(wxCommandEvent &evt);
	void OnRecentDropDownButton(wxAuiToolBarEvent &evt);
	void OnCloseFrame(wxCloseEvent &evt);

	wxMenu *m_recentMenu;
	wxAuiToolBar *m_toolBar;
	wxString m_currentAppDir;
	wxString m_lastFile;
	
	wxTextCtrl *m_txtDllPath;
	wxButton *m_btnChooseDll;
	AFLabel *m_lblDllStatus;
	wxTextCtrl *m_txtOutput;
	wxTextCtrl *m_txtSelectedCMs;
	wxTextCtrl *m_txtProgress;
	wxGauge *m_gauProgress;

	DataView *m_dataView;
	
	Array<cmModule> m_cmList;
	var_table *m_varTable;

	int m_recentCount;
	wxString m_recentFiles[MAX_RECENT];
	wxString m_loadedDllPath;
	wxString m_lastLoadTime;
	
	DECLARE_EVENT_TABLE()
};

#endif
