
#include <wx/dirdlg.h>
#include <wx/busyinfo.h>
#include <wx/stattext.h>

#include <wex/extgrid.h>
#include <wex/ole/excelauto.h>

#include "cmform.h"

enum {
  ID_btnSendToExcel,
  ID_grdCMVars,
  ID_btnAccept,
  ID_btnClose,
  ID_lstSelectedCMs,
  ID_cklCMList };

BEGIN_EVENT_TABLE( CMForm, wxPanel )
	EVT_CHECKLISTBOX(ID_cklCMList, CMForm::OnCMListCheck )
	EVT_LISTBOX(ID_cklCMList, CMForm::OnCMListSelect )
	EVT_BUTTON(ID_btnSendToExcel, CMForm::OnSendToExcel )
END_EVENT_TABLE()

CMForm::CMForm(wxWindow *parent)
	 : wxPanel( parent, wxID_ANY )
{
	cklCMList = new wxCheckListBox(this, ID_cklCMList);
	lstSelectedCMs = new wxListBox(this, ID_lstSelectedCMs);
	
	wxBoxSizer *szleft = new wxBoxSizer( wxVERTICAL );
	szleft->Add( cklCMList, 3, wxALL|wxEXPAND, 0 );
	szleft->Add( new wxStaticText( this, wxID_ANY, "Simulation sequence:" ), 0, wxALL|wxEXPAND, 2 );
	szleft->Add( lstSelectedCMs, 1, wxALL|wxEXPAND, 0 );

	wxBoxSizer *szbtns = new wxBoxSizer(wxHORIZONTAL );
#ifdef __WXMSW__
	szbtns->Add( new wxButton(this, ID_btnSendToExcel, "Send table to Excel..."), 0, wxALL|wxEXPAND, 2);
#endif
	szbtns->AddStretchSpacer();
	/*
	szbtns->Add( new wxButton(this, wxID_OK, "OK"), 0, wxALL|wxEXPAND, 2 );
	szbtns->Add( new wxButton(this, wxID_CANCEL, "Cancel"), 0, wxALL|wxEXPAND, 2 );
	*/
	m_lblStatus = new wxStaticText( this, wxID_ANY, wxEmptyString );

	szbtns->Add( m_lblStatus, 1, wxALL|wxEXPAND, 4 );
	
	grdCMVars = new wxExtGridCtrl(this, ID_grdCMVars);
	grdCMVars->CreateGrid(2,2);
	grdCMVars->EnableEditing(false);
	grdCMVars->DisableDragCell();
	grdCMVars->DisableDragColSize();
	grdCMVars->DisableDragRowSize();
	grdCMVars->DisableDragColMove();
	grdCMVars->DisableDragGridSize();
	grdCMVars->SetRowLabelSize(23);
	grdCMVars->SetColLabelSize(23);	
	grdCMVars->EnableDragColSize();

	wxBoxSizer *szcenter = new wxBoxSizer(wxHORIZONTAL );
	szcenter->Add( szleft, 1, wxALL|wxEXPAND, 0 );
	szcenter->Add( grdCMVars, 5, wxALL|wxEXPAND, 0 );

	wxBoxSizer *szmain = new wxBoxSizer(wxVERTICAL );
	szmain->Add( szcenter, 1, wxALL|wxEXPAND, 0 );
	szmain->Add( szbtns, 0, wxALL|wxEXPAND, 0 );

	SetSizer( szmain );
}

wxArrayString CMForm::GetAvailableCMs()
{
	wxArrayString list;
	try {

		int idx=0;
		while (const ssc_entry_t p_entry = ::ssc_module_entry(idx++))
			list.Add( ::ssc_entry_name(p_entry) );

	} catch(sscdll_error &e) {
		m_lblStatus->SetLabel("Dynamic library error: " + wxString(e.func.c_str()) + ": " + wxString(e.text.c_str()) );
	}

	m_lblStatus->SetLabel(wxEmptyString);

	return list;
}

void CMForm::LoadCMs()
{
	wxArrayString l = GetAvailableCMs();
	for (size_t i=0;i<l.Count();i++)
		cklCMList->Append( l[i] );
}

void CMForm::OnSendToExcel(wxCommandEvent &)
{
#ifdef __WXMSW__

	wxBusyInfo info("Sending data to excel...");
	grdCMVars->Copy(true);
	wxMilliSleep(150);

	wxExcelAutomation xl;
	if (!xl.StartExcel())
	{
		wxMessageBox("Could not start Excel.");
		return;
	}

	xl.Show(true);

	if (!xl.NewWorkbook())
	{
		wxMessageBox("Could not create a new Excel worksheet.");
		return;
	}
	xl.PasteClipboard();
	xl.AutoFitColumns();
#else
	wxMessageBox("Excel connection only available on Windows");
#endif

}

void CMForm::OnCMListCheck(wxCommandEvent &evt)
{
	int idx = evt.GetSelection();
	if (idx < 0) return;
	wxString name = cklCMList->GetString( idx );
	bool ischk = cklCMList->IsChecked( idx );

	int cmsel_idx = -1;
	for (size_t i=0;i<m_cmList.Count();i++)
		if (m_cmList[i] == name)
			cmsel_idx = i;

	if (ischk && cmsel_idx < 0)
	{
		m_cmList.Add( name );
		UpdateForm();
	}

	if (!ischk && cmsel_idx >= 0)
	{
		m_cmList.RemoveAt( cmsel_idx );
		UpdateForm();
	}
}

void CMForm::OnCMListSelect(wxCommandEvent &)
{
	try {
		wxString cm_name = cklCMList->GetStringSelection();
	
		ssc_module_t p_mod = ::ssc_module_create( (const char*)cm_name.c_str() );
		if ( p_mod == 0 )
		{
			m_lblStatus->SetLabel("Could not create module of type: " + cm_name);
			return;
		}

		std::vector<wxArrayString> vartab;

		int idx=0;
		while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, idx++ ) )
		{
			int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX
		

			wxArrayString row;

			switch( var_type )
			{
			case SSC_INPUT: row.Add("SSC_INPUT"); break;
			case SSC_OUTPUT: row.Add("SSC_OUTPUT"); break;
			case SSC_INOUT: row.Add("SSC_INOUT"); break;
			default: row.Add("<!unknown>"); break;
			}

			switch( data_type )
			{
			case SSC_STRING: row.Add("SSC_STRING"); break;
			case SSC_NUMBER: row.Add("SSC_NUMBER"); break;
			case SSC_ARRAY: row.Add("SSC_ARRAY"); break;
			case SSC_MATRIX: row.Add("SSC_MATRIX"); break;
			default: row.Add("<!unknown>"); break;
			}
			
			row.Add( ssc_info_name( p_inf ) );
			row.Add( ssc_info_label( p_inf ) );
			row.Add( ssc_info_units( p_inf ) );
			row.Add( ssc_info_meta( p_inf ) );
			row.Add( ssc_info_group( p_inf ) );
			row.Add( ssc_info_required( p_inf ) );
			row.Add( ssc_info_constraints( p_inf ) );

			vartab.push_back(row);
		}

		int nrows = (int)vartab.size();
		int ncols = 9;
		
		grdCMVars->Freeze();
		grdCMVars->ResizeGrid( nrows, ncols);
		grdCMVars->SetColLabelValue( 0, "TYPE" );
		grdCMVars->SetColLabelValue( 1, "DATA" );
		grdCMVars->SetColLabelValue( 2, "NAME" );
		grdCMVars->SetColLabelValue( 3, "LABEL" );
		grdCMVars->SetColLabelValue( 4, "UNITS" );
		grdCMVars->SetColLabelValue( 5, "META" );
		grdCMVars->SetColLabelValue( 6, "GROUP" );
		grdCMVars->SetColLabelValue( 7, "REQUIRE" );
		grdCMVars->SetColLabelValue( 8, "CONSTRAINT" );

		for (int r=0;r<nrows;r++)
			for (int c=0;c<ncols;c++)
				grdCMVars->SetCellValue( vartab[r][c], r, c );

		grdCMVars->AutoSizeColumns(false);
		grdCMVars->Thaw();

		::ssc_module_free( p_mod );
	
	} catch(sscdll_error &e) {
		m_lblStatus->SetLabel("DLL error: " + wxString(e.func.c_str()) + ": " + wxString(e.text.c_str()) );
	}

	m_lblStatus->SetLabel(wxEmptyString);
}


void CMForm::UpdateForm()
{
	wxArrayString list;
	lstSelectedCMs->Clear();
	for (size_t i=0;i<m_cmList.Count();i++)
	{
		lstSelectedCMs->Append( m_cmList[i] );
		list.Add( m_cmList[i] );
	}

	for (size_t i=0;i<cklCMList->GetCount();i++)
		cklCMList->Check( i, list.Index( cklCMList->GetString(i) ) != wxNOT_FOUND );

}
