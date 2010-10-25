#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>

#include <wx/config.h>
#include <wx/scrolbar.h>
#include <wx/print.h>
#include <wx/printdlg.h>
#include <wx/accel.h>
#include <wx/image.h>
#include <wx/fs_zip.h>
#include <wx/html/htmlwin.h>
#include <wx/snglinst.h>
#include <wx/progdlg.h>
#include <wx/busyinfo.h>
#include <wx/dir.h>
#include <wx/stdpaths.h>
#include <wx/generic/helpext.h>
#include <wx/clipbrd.h>
#include <wx/aui/aui.h>
#include <wx/snglinst.h>
#include <wx/filepicker.h>

#include <cml/util.h>
#include <cml/painter.h>
#include <cml/array.h>
#include <cml/afeditctrls.h>
#include <cml/afuiorgctrls.h>
#include <cml/afdialogs.h>


#include <cml/pixmaps/stock_save_24.xpm>
#include <cml/pixmaps/stock_save_as_24.xpm>
#include <cml/pixmaps/stock_open_24.xpm>
#include <cml/pixmaps/stock_text_indent_16.xpm>
#include <cml/pixmaps/stock_preferences_16.xpm>
#include <cml/pixmaps/stock_convert_16.xpm>
#include <cml/pixmaps/stock_exec_24.xpm>
#include <cml/pixmaps/stock_help_16.xpm>
#include <cml/pixmaps/stock_redo_24.xpm>
#include <cml/pixmaps/stock_undo_rtl_24.xpm>
#include <cml/pixmaps/stock_jump_to_24.xpm>
#include <cml/pixmaps/stock_search_24.xpm>
#include <cml/pixmaps/stock_refresh_24.xpm>
#include <cml/pixmaps/stock_about_24.xpm>
#include <cml/pixmaps/stock_preferences_24.xpm>
#include <cml/pixmaps/stock_trash_24.xpm>
#include <cml/pixmaps/stock_media_play_24.xpm>
#include <cml/pixmaps/stock_media_record_24.xpm>
#include <cml/pixmaps/stock_connect_24.xpm>
#include <cml/pixmaps/stock_disconnect_24.xpm>

#include "sscdev.h"
#include "splash.xpm"

/* exported application global variables */

int SC_major_ver = 0;
int SC_minor_ver = 1;
int SC_micro_ver = 0;

SCFrame *app_frame = NULL;
wxArrayString app_args;
wxConfig *app_config = NULL;

/* ************************************************************
   ************ SC Application (set up handlers/config) ******
   ************************************************************ */

IMPLEMENT_APP(SCApp)

wxString SCApp::GetInstanceName()
{
	return m_inst_name;
}

SCApp::SCApp()
{
}

bool SCApp::OnInit()
{
	// segmentation faults handled in SCApp::OnFatalException
	// works on MSW to check during development
#ifdef __WXMSW__
	if (!wxIsDebuggerRunning())
		wxHandleFatalExceptions();
#endif

	// generate a unique instance name
	int counter = 1;
	do
	{
		m_inst_name = "sscdev-" + wxGetUserId() + wxString::Format("-inst%d", counter++);
		m_inst_checker = new wxSingleInstanceChecker( m_inst_name );
		if (!m_inst_checker->IsAnotherRunning())
			counter = 0;
		else
			delete m_inst_checker;
	}
	while ( counter > 0 );

	// now we've found a unique instance name
	// and the app's instance checker object has been created
	// we will use the instance name as the temporary work directory
	SetAppName( "SSCdev" );
	
	// accumulate all the command line args
	for (int i=0;i<argc;i++)
		app_args.Add(argv[i]);

	// set the current working directory to locate .pdb on crash
	wxSetWorkingDirectory( wxPathOnly(app_args[0]) );

	app_config = new wxConfig( "sscdev", "WXAPPS" );
	
	/* needed for the html help viewer */
	wxImage::AddHandler(new wxBMPHandler);
    wxImage::AddHandler(new wxPNGHandler);
    wxImage::AddHandler(new wxJPEGHandler);
	wxImage::AddHandler(new wxXPMHandler);

    wxFileSystem::AddHandler(new wxZipFSHandler);

	app_frame = new SCFrame;
	SetTopWindow(app_frame);

	if ((int)app_args.Count() > 1)
		app_frame->Load(app_args[1]);

	bool first_load = true;
	wxString fl_key = Format("FirstLoad_%d",
		SC_major_ver*10000
		+SC_minor_ver*100
		+SC_micro_ver );

	app_config->Read(fl_key, &first_load, true);
	if (first_load)
	{
		// register the first load
		app_config->Write(fl_key, false);

		// on first load, maximize, and show help 'Getting Started'
		app_frame->SetPosition(wxPoint(10,10));
		app_frame->SetClientSize(700, 600);
	}

	return true;
}


int SCApp::OnExit()
{	
	if (app_config)
		delete app_config;

	if (m_inst_checker)
		delete m_inst_checker;
			
	return 0;
}

#ifdef __WXMSW__
/* simple class to get a stack trace */
#include <wx/stackwalk.h>

class TextMessageDialog : public wxDialog
{
private:
	wxTextCtrl *m_text;
public:
	TextMessageDialog(const wxString &text, const wxString &title="Notice") : wxDialog( NULL, -1, title, wxPoint(30,30), wxSize(500,400), wxRESIZE_BORDER|wxDEFAULT_DIALOG_STYLE )
	{
		m_text = new wxTextCtrl(this, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_DONTWRAP|wxTE_READONLY);
		m_text->SetFont( wxFont(10, wxMODERN, wxNORMAL, wxNORMAL) );
		m_text->ChangeValue(text);
		wxButton *btnclose = new wxButton(this, wxID_CANCEL, "Close");
		wxBoxSizer *btnsz = new wxBoxSizer(wxHORIZONTAL);
		btnsz->AddStretchSpacer(1);
		btnsz->Add(btnclose, 0, wxALL, 2);
		wxBoxSizer *vert = new wxBoxSizer(wxVERTICAL);
		vert->Add( m_text, 1, wxALL|wxEXPAND, 0 );
		vert->Add( btnsz, 0, wxALL|wxEXPAND, 2 );
		SetSizer(vert);
	}

	void SetText(const wxString &text) { m_text->ChangeValue(text);	}
};

void wxTextMessageDialog(const wxString &text, const wxString &title="Notice")
{
	TextMessageDialog dlg(text);
	dlg.ShowModal();	
}

class StackDump : public wxStackWalker
{
public:
    const wxString& GetStackTrace() const { return m_stackTrace; }
protected:
    virtual void OnStackFrame(const wxStackFrame& frame)
    {
		wxString line = wxString::Format("[%d] ", (int)frame.GetLevel());
        wxString name = frame.GetName();
        line += wxString::Format("%p %s", frame.GetAddress(), name.c_str());
        if ( frame.HasSourceLocation() ) line += " (" + frame.GetFileName() + "):" << frame.GetLine();
		m_stackTrace += line + "\n";
    }
private:
    wxString m_stackTrace;
};

#endif

void SCApp::OnFatalException()
{
	StackDump dump;
	dump.WalkFromException();

	wxString msgtext = TruncateLines( dump.GetStackTrace(), 9, "..." );

	wxString body;	
	body += "SYSTEM INFORMATION:\n\n";
	body += "User Name: " + wxGetUserName() + "\n";
	body += "Home Dir: " + wxGetHomeDir() + "\n";
	body += "Email Address: " + wxGetEmailAddress() + "\n";;
	body += "Full Host Name: " + wxGetFullHostName() + "\n";
	body += "OS: " + wxGetOsDescription() + "\n";
	body += "Little Endian?: " + wxString::Format("%s",wxIsPlatformLittleEndian()?"Yes":"No") + "\n";
	body += "64-bit?: " + wxString::Format("%s",wxIsPlatform64Bit()?"Yes":"No") + "\n";
	body += "Free Memory: " + wxString::Format("%lg kB",wxGetFreeMemory().ToDouble()/1024) + "\n";
	body += "\n";

	body += "\nCRASH TRACE [" + wxNow() + "]\n\n" + dump.GetStackTrace();

	wxTextMessageDialog(body);
}


/* ************************************************************
   ************ SC 'About' Dialog ****************************
   ************************************************************ */

enum { ID_ABOUT_CLOSE, ID_ABOUT_TEXT,ID_ABOUT_CRASH};

BEGIN_EVENT_TABLE(SCAbout, wxDialog)
	EVT_CLOSE( SCAbout::OnCloseFrame )
	EVT_BUTTON( ID_ABOUT_CLOSE,SCAbout::OnClose )
	EVT_BUTTON( ID_ABOUT_CRASH,SCAbout::OnCrash )
END_EVENT_TABLE()

static char *SC_about_text = "SSCdev - System Simulator Core\n\n"
"Developer interface for simulation, component development, validation, and basic visualization of SSC computation modules.";

SCAbout::SCAbout(wxWindow *parent)
: wxDialog(parent, -1, "SC", wxDefaultPosition, wxDefaultSize,wxBORDER_SIMPLE)
{
	wxBitmap pic;
	
	if (!pic.LoadFile( wxPathOnly(app_args[0]) + "/splash.bmp", wxBITMAP_TYPE_BMP ))
		pic = wxBitmap(splash_xpm);

	SetBackgroundColour( *wxWHITE );
	mBitmap = new wxStaticBitmap(this, -1, pic);
	mLblVersion = new wxStaticText(this, -1, "");
	mLblVersion->SetBackgroundColour( *wxWHITE );

	wxAcceleratorEntry entries[1];
	entries[0].Set(::wxACCEL_CTRL, WXK_F7, ID_ABOUT_CRASH);
	wxAcceleratorTable acceltab(1,entries);
	SetAcceleratorTable(acceltab);

	mLblVersion->SetLabel( wxString::Format(" Version %d.%d.%d", 
		SC_major_ver, SC_minor_ver, SC_micro_ver ));

	mBtnClose = new wxButton(this, ID_ABOUT_CLOSE, "Close");	

	mText = new wxTextCtrl(this, ID_ABOUT_TEXT, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY);
	mText->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "courier"));
	mText->SetForegroundColour( wxColour(90,90,90) );

	mText->SetSize(4,pic.GetHeight()+4,pic.GetWidth()-8,126);
	mText->SetValue(SC_about_text);
	mLblVersion->SetSize(3,pic.GetHeight()+134,pic.GetWidth()-135,17);
	mBtnClose->SetSize(pic.GetWidth()-74,pic.GetHeight()+134,70,21);

	
	/* hidden button that can be 'clicked' or 
	activated by the accelerator to cause a crash */
	wxButton *cb = new wxButton(this, ID_ABOUT_CRASH, "SegF");
	cb->SetSize( 10, 10,50,21);
	cb->Show(false);

#ifdef __WXMSW__
	this->SetClientSize( pic.GetWidth(), pic.GetHeight()+130+29 );
#else
	this->SetClientSize( pic.GetWidth()-1, pic.GetHeight()+130+29 );
#endif

	this->CenterOnScreen();
	SetEscapeId(ID_ABOUT_CLOSE);
}

void SCAbout::OnCloseFrame(wxCloseEvent &evt)
{
	EndModal(0);
}

void SCAbout::OnClose(wxCommandEvent &evt)
{
	EndModal(0);
}

void SCAbout::OnCrash(wxCommandEvent &evt)
{
	// force a segfault!
	wxBitmap *bit = NULL;
	wxBitmap bit2 = *bit;
}


/* ************************************************************
   ************ SC  Parent window class  ******************
   ************************************************************ */

enum{   ID_START, ID_STOP, 
		ID_LOAD_UNLOAD_DLL,
		ID_DLL_PATH,
		ID_CHOOSE_DLL,
			
		// up to 100 recent items can be accommodated
		ID_RECENT = 500,
		ID_RECENT_LAST = 600
};

BEGIN_EVENT_TABLE(SCFrame, wxFrame)

	EVT_TOOL( wxID_OPEN,                   SCFrame::OnCommand )
	EVT_TOOL( wxID_SAVE,                   SCFrame::OnCommand )
	EVT_TOOL( wxID_SAVEAS,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_PREFERENCES,            SCFrame::OnCommand )

	EVT_TOOL( wxID_EXIT,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_ABOUT,                 SCFrame::OnCommand )
	EVT_TOOL( wxID_HELP,                  SCFrame::OnCommand )

	EVT_TOOL( ID_LOAD_UNLOAD_DLL,              SCFrame::OnCommand )
	EVT_TEXT_ENTER( ID_DLL_PATH,                SCFrame::OnCommand )
	EVT_BUTTON( ID_CHOOSE_DLL,            SCFrame::OnCommand )
	

    EVT_AUITOOLBAR_TOOL_DROPDOWN(wxID_OPEN, SCFrame::OnRecentDropDownButton)
	
	EVT_CLOSE( SCFrame::OnCloseFrame )
	
	// For recent file menu
	EVT_MENU_RANGE( ID_RECENT, ID_RECENT+MAX_RECENT, SCFrame::OnRecent)
	
END_EVENT_TABLE()

SCFrame::SCFrame()
 : wxFrame(NULL, wxID_ANY, "SSCdev", wxDefaultPosition, wxSize(800,600))
{
	
	m_recentCount = 0;
	SetIcon( wxIcon("appicon") );

	m_toolBar = new wxAuiToolBar(this);
	
	m_toolBar->AddTool( ID_LOAD_UNLOAD_DLL,"Load/unload ssc32.dll", wxBitmap(stock_connect_24_xpm),"Load/unload ssc32.dll");
	m_toolBar->AddSeparator();
	m_toolBar->AddTool( ID_START ,"Start", wxBitmap(stock_media_play_24_xpm),"Start computations...");
	m_toolBar->AddTool( ID_STOP, "Stop", wxBitmap(stock_media_record_24_xpm), "Stop computations");
	m_toolBar->AddSeparator();
	m_toolBar->AddTool( wxID_OPEN ,"Open", wxBitmap(stock_open_24_xpm),"Open input file");
    m_toolBar->SetToolDropDown(wxID_OPEN, true);
	m_toolBar->AddTool( wxID_SAVE, "Save", wxBitmap(stock_save_24_xpm),"Save input file");
	m_toolBar->AddTool( wxID_SAVEAS, "Save as", wxBitmap(stock_save_as_24_xpm), "Save input file as...");
	m_toolBar->AddSeparator();
	m_toolBar->AddTool( wxID_PREFERENCES, "Options...", wxBitmap(stock_preferences_24_xpm), "Options...");
	m_toolBar->AddStretchSpacer();
	m_toolBar->AddTool( wxID_ABOUT, "About SSCdev", wxBitmap(stock_about_24_xpm), "About SSCdev...");
	m_toolBar->SetToolBitmapSize(wxSize(24,24));
	m_toolBar->Realize();

	m_txtDllPath = new wxTextCtrl( this, ID_DLL_PATH, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
	m_btnChooseDll = new wxButton( this, ID_CHOOSE_DLL, "...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT );
	m_lblDllStatus = new AFLabel( this, -1, "Status" );

	wxBoxSizer *sz_dll_info = new wxBoxSizer(wxHORIZONTAL);
	sz_dll_info->Add( m_lblDllStatus, 2, wxALL|wxEXPAND, 0 );
	sz_dll_info->Add( m_txtDllPath, 1, wxALL|wxEXPAND, 0 );
	sz_dll_info->Add( m_btnChooseDll, 0, wxALL|wxEXPAND, 0 );

	wxBoxSizer *sz_main = new wxBoxSizer(wxVERTICAL);
	sz_main->Add( m_toolBar, 0, wxALL|wxEXPAND, 0 );
	sz_main->Add( sz_dll_info, 0, wxALL|wxEXPAND, 0 );
	sz_main->AddStretchSpacer();

	SetSizer(sz_main );


	m_recentMenu = new wxMenu;	
	long ct = 0;
	if (app_config->Read("RecentCount", &ct))
		m_recentCount = (int)ct;

	if (m_recentCount > MAX_RECENT)
		m_recentCount = MAX_RECENT;

	for (int i=0;i<m_recentCount;i++)
	{
		wxString fn;
		if (app_config->Read(wxString::Format("RecentFile_%d", i), &fn))
		{
			fn.Replace("\\","/");
			m_recentFiles[i] = fn;
		}
	}

	app_config->Read("CurrentDirectory", &m_currentAppDir);

	wxString dll_path;
	app_config->Read("DllPath", &dll_path);
	if (wxFileExists(dll_path))
	{
		m_txtDllPath->ChangeValue( dll_path );
		m_loadedDllPath = dll_path;
		sscdll_load( dll_path.c_str() );
		m_lastLoadTime = wxNow();
	}

	UpdateRecentMenu();	

	

	wxAcceleratorEntry entries[6];
	entries[0].Set( wxACCEL_NORMAL, WXK_F1, ID_LOAD_UNLOAD_DLL );
	entries[1].Set( wxACCEL_NORMAL, WXK_F5, ID_START );
	SetAcceleratorTable( wxAcceleratorTable(2,entries) );
	


	/************ SHOW THE APPLICATION *************/
	
	this->Show();
	this->SetClientSize( 750, 600);	


	// restore window position

	bool b_maximize = false;
	int f_x,f_y,f_width,f_height;
	app_config->Read("FrameX", &f_x, -1);
	app_config->Read("FrameY", &f_y, -1);
	app_config->Read("FrameWidth", &f_width, -1);
	app_config->Read("FrameHeight", &f_height, -1);
	app_config->Read("FrameMaximized", &b_maximize, false);

	if (b_maximize)
	{
		this->Maximize();
	}
	else
	{
		if (f_width > 100 && f_height > 100)
			this->SetClientSize(f_width, f_height);

		if (f_x > 0 && f_y > 0)
			this->SetPosition(wxPoint(f_x,f_y));
	}

	UpdateUI();
	
}

void SCFrame::UpdateUI()
{
	wxString status;

	if (sscdll_isloaded())
	{
		int ver = 0;
		try {
			ver = ssc_version();
		} catch (sscdll_error e) {
			status = e.text + " ";
			ver = -999;
		}
		status += m_loadedDllPath + " ( " + m_lastLoadTime + " ) Version " + wxString::Format("%d", ver);
	}
	else
		status = "ssc32.dll not loaded.";

	m_lblDllStatus->SetCaption( status );

	m_toolBar->SetToolBitmap( ID_LOAD_UNLOAD_DLL, sscdll_isloaded() ? wxBitmap(stock_disconnect_24_xpm) : wxBitmap(stock_connect_24_xpm) );
	m_toolBar->SetToolShortHelp( ID_LOAD_UNLOAD_DLL, sscdll_isloaded() ? "Unload ssc32.dll" : "Load ssc32.dll" );

	m_toolBar->EnableTool( ID_START, sscdll_isloaded() );
	m_toolBar->EnableTool( ID_STOP, sscdll_isloaded() );
	m_toolBar->Refresh();
}
	
void SCFrame::UpdateRecentMenu()
{
	int i;
	for (i=0;i<MAX_RECENT;i++)
	{
		if (m_recentMenu->FindItem(ID_RECENT+i) != NULL)
			m_recentMenu->Destroy(ID_RECENT+i);
	}

	for (i=0;i<m_recentCount;i++)
	{
		wxString name;
		name.Printf("%d  %s", i+1, m_recentFiles[i].c_str());
		m_recentMenu->Append(ID_RECENT+i, name);
	}

}

void SCFrame::OnRecent(wxCommandEvent &evt)
{
	int id = evt.GetId() - ID_RECENT;
	if (id < 0 || id >= MAX_RECENT)
		return;

//	if (!CloseDocument())
//		return;

	wxString fn = m_recentFiles[id];
	if (fn != "")
		Load(fn);
}

void SCFrame::OnRecentDropDownButton(wxAuiToolBarEvent& evt)
{
	if (evt.IsDropDownClicked())
	{
		wxAuiToolBar* tb = static_cast<wxAuiToolBar*>(evt.GetEventObject());
		if (m_recentCount < 1)
		{
			wxMessageBox("No recent files.");
			tb->SetToolSticky(evt.GetId(), false);
			return;
		}

		tb->SetToolSticky(evt.GetId(), true);
		// line up our menu with the button
		wxRect rect = tb->GetToolRect(evt.GetId());
		wxPoint pt = tb->ClientToScreen(rect.GetBottomLeft());
		pt = ScreenToClient(pt);
		PopupMenu(m_recentMenu, pt);
		// make sure the button is "un-stuck"
		tb->SetToolSticky(evt.GetId(), false);
	}
}

wxArrayString SCFrame::GetRecentFiles()
{
	wxArrayString list;
	for (int i=0;i<m_recentCount;i++)
		list.Add( m_recentFiles[i] );
	return list;
}

void SCFrame::AddRecent(const wxString &fn)
{
	wxString norm_fn = fn;
	norm_fn.Replace("\\","/");

	int i;
	int index = -1;
	// find the file in the recent list
	for (i=0;i<m_recentCount;i++)
	{
		if (norm_fn == m_recentFiles[i])
		{
			index = i;
			break;
		}
	}

	if (index >= 0)
	{
		// bring this file to the front of the
		// recent file list

		for (i=index;i>0;i--)
			m_recentFiles[i] = m_recentFiles[i-1];
	}
	else // not found in recent list
	{
		// add this to the front of the recent list
		// and increment the recent count if its 
		// less than MAX_RECENT

		for (i=MAX_RECENT-1;i>0;i--)
			m_recentFiles[i] = m_recentFiles[i-1];

		if (m_recentCount < MAX_RECENT)
			m_recentCount++;
	}
	
	m_recentFiles[0] = norm_fn;
	UpdateRecentMenu();
}

void SCFrame::RemoveRecent(const wxString &fn)
{
	wxString norm_fn = fn;
	norm_fn.Replace("\\","/");

	int i;
	int index = -1;
	// find the file in the recent list
	for (i=0;i<m_recentCount;i++)
	{
		if (norm_fn == m_recentFiles[i])
		{
			index = i;
			break;
		}
	}

	if (index >= 0)
	{
		for (i=index;i<MAX_RECENT-1;i++)
			m_recentFiles[i] = m_recentFiles[i+1];

		m_recentCount--;
		UpdateRecentMenu();
	}
}


void SCFrame::OnCloseFrame( wxCloseEvent &evt )
{

	if (evt.CanVeto() && !CloseDocument())
	{
		evt.Veto();
		return;
	}	

	/* save window position */
	bool b_maximize = this->IsMaximized();
	int f_x,f_y,f_width,f_height;

	this->GetPosition(&f_x,&f_y);
	this->GetClientSize(&f_width, &f_height);
	
	app_config->Write("FrameX", f_x);
	app_config->Write("FrameY", f_y);
	app_config->Write("FrameWidth", f_width);
	app_config->Write("FrameHeight", f_height);
	app_config->Write("FrameMaximized", b_maximize);

	long ct = (long)m_recentCount;
	app_config->Write("RecentCount", ct);
	for (int i=0;i<m_recentCount;i++)
	{
		wxString key;
		key.Printf("RecentFile_%d", i);
		app_config->Write(key, m_recentFiles[i]);
	}
	app_config->Write("CurrentDirectory", m_currentAppDir);
	app_config->Write("DllPath", m_txtDllPath->GetValue());

	Destroy();
}


bool SCFrame::Load(const wxString &fn)
{
	return false;	
}

void SCFrame::Open()
{
}


void SCFrame::Save()
{
}

void SCFrame::SaveAs()
{
}

bool SCFrame::CloseDocument()
{
	return true;
}

void SCFrame::Exit()
{
	Close( false );
}

void SCFrame::OnCommand(wxCommandEvent &evt)
{	
	switch(evt.GetId())
	{
	case wxID_OPEN:
	case wxID_SAVE:
	case wxID_SAVEAS:
		wxMessageBox("open/save/saveas");
		break;
	case wxID_EXIT:
		Exit();
		break;
	case wxID_ABOUT:
		{		
			SCAbout dlg(this);
			dlg.ShowModal();	
		}
		break;
	case ID_LOAD_UNLOAD_DLL:
		{
			if (!sscdll_isloaded())
			{
				m_loadedDllPath = m_txtDllPath->GetValue();
				sscdll_load( m_loadedDllPath.c_str() );
				m_lastLoadTime = wxNow();
				m_currentAppDir = wxPathOnly(m_loadedDllPath);
			}
			else
				sscdll_unload();

			UpdateUI();
		}
		break;
	case ID_CHOOSE_DLL:
		{
			wxFileDialog fd(this, "Choose ssc32.dll", m_currentAppDir, "ssc32.dll", "DLL Files (*.dll)|*.dll", wxFD_OPEN);
			if (fd.ShowModal() != wxID_OK) return;
			wxString file = fd.GetPath();
			m_txtDllPath->ChangeValue(file);
			m_currentAppDir = wxPathOnly(file);
			m_loadedDllPath = file;
			sscdll_load(file.c_str());
			m_lastLoadTime = wxNow();
			UpdateUI();
		}
		break;
	case ID_DLL_PATH:
		{
			wxString file = m_txtDllPath->GetValue();
			m_currentAppDir = wxPathOnly( file );
			m_loadedDllPath = file;
			sscdll_load(file.c_str());
			m_lastLoadTime = wxNow();
			UpdateUI();
		}
		break;
	}
}