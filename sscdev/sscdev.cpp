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

#include <cml/util.h>
#include <cml/painter.h>
#include <cml/array.h>

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
	SetAppName( "sscdev" );
	
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

class StackDialog : public wxDialog
{
private:
	wxTextCtrl *m_text;
public:
	StackDialog() : wxDialog( NULL, -1, "Stack Trace", wxPoint(30,30), wxSize(500,400), wxRESIZE_BORDER )
	{
		m_text = new wxTextCtrl(this, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_DONTWRAP|wxTE_READONLY);
		m_text->SetFont( wxFont(10, wxMODERN, wxNORMAL, wxNORMAL) );
		wxButton *btnclose = new wxButton(this, wxID_CANCEL, "Close");
		wxBoxSizer *btnsz = new wxBoxSizer(wxHORIZONTAL);
		btnsz->AddStretchSpacer(1);
		btnsz->Add(btnclose, 0, wxALL, 2);
		wxBoxSizer *vert = new wxBoxSizer(wxVERTICAL);
		vert->Add( m_text, 1, wxALL|wxEXPAND, 0 );
		vert->Add( btnsz, 0, wxALL, 2 );
		SetSizer(vert);
	}

	void SetText(const wxString &text) { m_text->ChangeValue(text);	}
};

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

	StackDialog dlg;
	dlg.SetText( body );
	dlg.ShowModal();	
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

static char *SC_about_text = "sscdev - System Simulator Core\n\n"
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

enum{  ID_OPEN, ID_SAVE, ID_SAVEAS, ID_CLOSEFILE,

		ID_RECENT_FILES,		
			
		// up to 100 recent items can be accommodated
		ID_RECENT = 500,
		ID_RECENT_LAST = 600
};

BEGIN_EVENT_TABLE(SCFrame, wxFrame)

	EVT_MENU( ID_OPEN,                   SCFrame::OnCommand )
	EVT_MENU( ID_SAVE,                   SCFrame::OnCommand )
	EVT_MENU( ID_SAVEAS,                 SCFrame::OnCommand )
	EVT_MENU( ID_CLOSEFILE,              SCFrame::OnCommand )

	EVT_MENU( wxID_EXIT,                SCFrame::OnCommand )
	EVT_MENU( wxID_ABOUT,                 SCFrame::OnCommand )
	EVT_MENU( wxID_HELP,                  SCFrame::OnCommand )
	
	EVT_CLOSE( SCFrame::OnCloseFrame )
	
	// For recent file menu
	EVT_MENU_RANGE( ID_RECENT, ID_RECENT+MAX_RECENT, SCFrame::OnRecent)
	
END_EVENT_TABLE()

SCFrame::SCFrame()
 : wxFrame(NULL, wxID_ANY, "sscdev", wxDefaultPosition, wxSize(800,600))
{
	
	mRecentCount = 0;

	SetIcon( wxIcon("appicon") );

	wxMenuBar *menubar = new wxMenuBar;

	mRecentMenu = new wxMenu;

	mFileMenu = new wxMenu;
	mFileMenu->Append(ID_OPEN, "Open...\tCtrl-O");
	mFileMenu->Append(ID_SAVE, "Save\tCtrl-S");
	mFileMenu->Append(ID_SAVEAS, "Save As...");
	mFileMenu->AppendSeparator();
	mFileMenu->Append(ID_CLOSEFILE, "Close\tCtrl-W");
	mFileMenu->AppendSeparator();
	mFileMenu->Append(ID_RECENT_FILES, "Recent Files", mRecentMenu);
	
#ifndef __WXMAC__
	mFileMenu->AppendSeparator();
	mFileMenu->Append(wxID_EXIT);
#endif
	menubar->Append(mFileMenu, "&File");
	
	wxMenu *help_menu = new wxMenu;

	help_menu->Append(wxID_HELP, "Help Contents\tF1");
#ifndef __WXMAC__
	help_menu->AppendSeparator();
#endif
	help_menu->Append(wxID_ABOUT);
	menubar->Append(help_menu, "&Help");
	
	SetMenuBar( menubar );
				
	long ct = 0;
	if (app_config->Read("RecentCount", &ct))
		mRecentCount = (int)ct;

	if (mRecentCount > MAX_RECENT)
		mRecentCount = MAX_RECENT;

	for (int i=0;i<mRecentCount;i++)
	{
		wxString key;
		key.Printf("RecentFile_%d", i);
		wxString fn;
		if (app_config->Read(key, &fn))
		{
			fn.Replace("\\","/");
			mRecentFiles[i] = fn;
		}
	}

	app_config->Read("LastDirectory", &mLastDir);

	UpdateRecentMenu();	
	

	/*wxAcceleratorEntry entries[6];
	entries[0].Set( wxACCEL_SHIFT, WXK_F1, ID_HELPCONTEXT );
	SetAcceleratorTable( wxAcceleratorTable(1,entries) );
	*/


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
}

SCFrame::~SCFrame()
{	
	
	long ct = (long)mRecentCount;
	app_config->Write("RecentCount", ct);
	for (int i=0;i<mRecentCount;i++)
	{
		wxString key;
		key.Printf("RecentFile_%d", i);
		app_config->Write(key, mRecentFiles[i]);
	}

	app_config->Write("LastDirectory", mLastDir);

};
	
void SCFrame::UpdateRecentMenu()
{
	int i;
	for (i=0;i<MAX_RECENT;i++)
	{
		if (mRecentMenu->FindItem(ID_RECENT+i) != NULL)
			mRecentMenu->Destroy(ID_RECENT+i);
	}

	for (i=0;i<mRecentCount;i++)
	{
		wxString name;
		name.Printf("%d  %s", i+1, mRecentFiles[i].c_str());
		mRecentMenu->Append(ID_RECENT+i, name);
	}


	mFileMenu->Enable(ID_RECENT_FILES, mRecentCount > 0);
}

void SCFrame::OnRecent(wxCommandEvent &evt)
{
	int id = evt.GetId() - ID_RECENT;
	if (id < 0 || id >= MAX_RECENT)
		return;

//	if (!CloseDocument())
//		return;

	wxString fn = mRecentFiles[id];
	if (fn != "")
		Load(fn);
}

wxArrayString SCFrame::GetRecentFiles()
{
	wxArrayString list;
	for (int i=0;i<mRecentCount;i++)
		list.Add( mRecentFiles[i] );
	return list;
}

void SCFrame::AddRecent(const wxString &fn)
{
	wxString norm_fn = fn;
	norm_fn.Replace("\\","/");

	int i;
	int index = -1;
	// find the file in the recent list
	for (i=0;i<mRecentCount;i++)
	{
		if (norm_fn == mRecentFiles[i])
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
			mRecentFiles[i] = mRecentFiles[i-1];
	}
	else // not found in recent list
	{
		// add this to the front of the recent list
		// and increment the recent count if its 
		// less than MAX_RECENT

		for (i=MAX_RECENT-1;i>0;i--)
			mRecentFiles[i] = mRecentFiles[i-1];

		if (mRecentCount < MAX_RECENT)
			mRecentCount++;
	}
	
	mRecentFiles[0] = norm_fn;
	UpdateRecentMenu();
}

void SCFrame::RemoveRecent(const wxString &fn)
{
	wxString norm_fn = fn;
	norm_fn.Replace("\\","/");

	int i;
	int index = -1;
	// find the file in the recent list
	for (i=0;i<mRecentCount;i++)
	{
		if (norm_fn == mRecentFiles[i])
		{
			index = i;
			break;
		}
	}

	if (index >= 0)
	{
		for (i=index;i<MAX_RECENT-1;i++)
			mRecentFiles[i] = mRecentFiles[i+1];

		mRecentCount--;
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
	case wxID_EXIT:
		Exit();
		break;
	case wxID_ABOUT:
		{
		
			SCAbout dlg(this);
			dlg.ShowModal();	
		}
		break;
	}
}