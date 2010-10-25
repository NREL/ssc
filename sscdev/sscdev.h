#ifndef __SC_h
#define __SC_h

#include <wx/wx.h>
#include <wx/scrolbar.h>
#include <wx/print.h>
#include <wx/listctrl.h>
#include <wx/printdlg.h>
#include <wx/aui/auibook.h>
#include <wx/snglinst.h>

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


class SCFrame : public wxFrame
{
public:
	SCFrame();
	virtual ~SCFrame();
	
	bool Load(const wxString &fn);
	bool CloseDocument();
	void Open();
	void Save();
	void SaveAs();
	void Exit();

	void AddRecent(const wxString &fn);
	void RemoveRecent(const wxString &fn);
	void UpdateRecentMenu();
	wxArrayString GetRecentFiles();

private:	

	void OnCommand(wxCommandEvent &evt);
	void OnRecent(wxCommandEvent &evt);

	void OnCloseFrame(wxCloseEvent &evt);

	wxMenu *mFileMenu, *mRecentMenu;
	wxToolBar *mToolBar;
	wxString mLastDir;
	
	int mRecentCount;
	wxString mRecentFiles[MAX_RECENT];

	DECLARE_EVENT_TABLE()
};

#endif
