/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

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
class wxConfig;

extern SCFrame *app_frame;
extern wxConfig *app_config;

void applog(const wxString &s);
void applog(const char *fmt, ...);

//extern int SC_major_ver;
//extern int SC_minor_ver;
//extern int SC_micro_ver;

class SCApp : public wxApp
{
public:
	virtual bool OnInit();
	virtual int OnExit();
};

DECLARE_APP(SCApp)

class wxMetroNotebook;
class wxExtGridCtrl;

class SCFrame : public wxFrame
{
public:
	SCFrame();
	virtual ~SCFrame();
		
	bool CloseDocument();
	bool LoadBdat( wxString fn = wxEmptyString );

	bool LoadScript(wxString fn = wxEmptyString);


	void SaveBdat();
	bool WriteBdatToDisk(const wxString &fn);
	
	void ChooseDynamicLibrary();
	void LoadUnloadLibrary();

	std::vector<bool> Start();
	void ClearLog();
	void Log(const wxString &, bool wnl=true);
	
	static void Copy(ssc_module_t p_mod, ssc_data_t p_data, var_table *vt, bool clear_first);
	static void Copy(var_table *vt, ssc_data_t p_data, bool clear_first);

	void Progress(const wxString &text, float percent);

	wxString LastFileName() { return m_lastFile; }

	DataView *GetDataView() { return m_dataView; }
	var_table *GetVarTable() { return m_varTable; }


	void SetProgress( int percent, const wxString &msg = wxEmptyString );
	
	wxArrayString GetAvailableCMs();
	void LoadCMs();
	void SetCurrentCM( const wxString & cm ) { m_currentCM->SetStringSelection( cm ); }
	wxString GetCurrentCM() { return m_currentCM->GetStringSelection(); }
	void UpdateCMForm();
	void OnRun( wxCommandEvent & );
	void OnCMListSelect(wxCommandEvent &evt);
	void OnCopyToClipboard(wxCommandEvent &);

	bool UpdateIsStopFlagSet();

private:	
	void WriteVarTable( wxDataOutputStream &o, var_table &vt );
	bool ReadVarTable( wxDataInputStream &o, var_table &vt, bool clear_first );

	void UpdateUI();

	void OnCommand(wxCommandEvent &evt);
	void OnCloseFrame(wxCloseEvent &evt);
	
	wxExtGridCtrl *m_gridCM;
	wxChoice *m_currentCM;
	wxListBox *m_listCM;

	wxStaticText *m_statusLabel;
	wxGauge *m_progressBar;

	wxString m_currentAppDir;
	wxString m_lastFile;
	wxString m_dllPath;

	wxTextCtrl *m_txtOutput;

	wxMetroNotebook *m_notebook;
	DataView *m_dataView;
	EditorWindow *m_scriptWindow;

	var_table *m_varTable;
		
	DECLARE_EVENT_TABLE()
};

#endif
