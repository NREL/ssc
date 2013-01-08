
#include <wx/dirdlg.h>
#include <wx/busyinfo.h>
#include <wx/stattext.h>

#include <wex/extgrid.h>
#include <wex/ole/excelauto.h>

#include "cmform.h"

enum {
  ID_btnSendToExcel = wxID_HIGHEST+132,
  ID_list };

BEGIN_EVENT_TABLE( CMForm, wxPanel )
	EVT_LISTBOX(ID_list, CMForm::OnCMListSelect )
	EVT_BUTTON(ID_btnSendToExcel, CMForm::OnSendToExcel )
END_EVENT_TABLE()

CMForm::CMForm(wxWindow *parent)
	 : wxPanel( parent, wxID_ANY )
{
	m_currentCM = new wxChoice( this, wxID_ANY );	
	
	m_list = new wxListBox(this, ID_list);
		
	m_grid = new wxExtGridCtrl(this, wxID_ANY);
	m_grid->CreateGrid(2,2);
	m_grid->EnableEditing(false);
	m_grid->DisableDragCell();
	m_grid->DisableDragColSize();
	m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	m_grid->DisableDragGridSize();
	m_grid->SetRowLabelSize(23);
	m_grid->SetColLabelSize(23);	
	m_grid->EnableDragColSize();
	
	wxBoxSizer *szh_top = new wxBoxSizer( wxHORIZONTAL );
	szh_top->Add( new wxStaticText( this, wxID_ANY, " Run:" ),0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
	szh_top->Add( m_currentCM, 1, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 3 );

	wxBoxSizer *szleft = new wxBoxSizer( wxVERTICAL );
	szleft->Add( szh_top, 0, wxALL|wxEXPAND, 3 );
	szleft->Add( new wxStaticText( this, wxID_ANY, " Available modules:" ), 0, wxALL|wxEXPAND|wxALIGN_BOTTOM, 1 );
	szleft->Add( m_list, 1, wxALL|wxEXPAND, 3 );
#ifdef __WXMSW__
	szleft->Add( new wxButton(this, ID_btnSendToExcel, "Send table to Excel..."), 0, wxALL|wxEXPAND, 3);
#endif

	wxBoxSizer *szcenter = new wxBoxSizer(wxHORIZONTAL );
	szcenter->Add( szleft, 1, wxALL|wxEXPAND, 3 );
	szcenter->Add( m_grid, 5, wxALL|wxEXPAND, 3 );

	SetSizer( szcenter );
}

wxArrayString CMForm::GetAvailableCMs()
{
	wxArrayString list;
	try {

		int idx=0;
		while (const ssc_entry_t p_entry = ::ssc_module_entry(idx++))
			list.Add( ::ssc_entry_name(p_entry) );

	} catch(sscdll_error &e) {
	}

	return list;
}

void CMForm::LoadCMs()
{
	m_list->Clear();
	m_grid->ClearGrid();
	wxArrayString l = GetAvailableCMs();
	for (size_t i=0;i<l.Count();i++)
		m_list->Append( l[i] );
}

void CMForm::OnSendToExcel(wxCommandEvent &)
{
#ifdef __WXMSW__

	wxBusyInfo info("Sending data to excel...");
	m_grid->Copy(true);
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

void CMForm::OnCMListSelect(wxCommandEvent &)
{
	try {
		wxString cm_name = m_list->GetStringSelection();
	
		ssc_module_t p_mod = ::ssc_module_create( (const char*)cm_name.c_str() );
		if ( p_mod == 0 )
		{
			wxMessageBox("Could not create a module of type: " + cm_name );
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
		
		m_grid->Freeze();
		m_grid->ResizeGrid( nrows, ncols);
		m_grid->SetColLabelValue( 0, "TYPE" );
		m_grid->SetColLabelValue( 1, "DATA" );
		m_grid->SetColLabelValue( 2, "NAME" );
		m_grid->SetColLabelValue( 3, "LABEL" );
		m_grid->SetColLabelValue( 4, "UNITS" );
		m_grid->SetColLabelValue( 5, "META" );
		m_grid->SetColLabelValue( 6, "GROUP" );
		m_grid->SetColLabelValue( 7, "REQUIRE" );
		m_grid->SetColLabelValue( 8, "CONSTRAINT" );

		for (int r=0;r<nrows;r++)
			for (int c=0;c<ncols;c++)
				m_grid->SetCellValue( vartab[r][c], r, c );

		m_grid->AutoSizeColumns(false);
		m_grid->Thaw();

		::ssc_module_free( p_mod );
	
	} catch(sscdll_error &e) {
		wxMessageBox("Dynamic library error: " + wxString(e.func.c_str()) + ": " + wxString(e.text.c_str()) );
	}
}


void CMForm::UpdateForm()
{
	wxString sel = m_list->GetStringSelection();
	wxString run = m_currentCM->GetStringSelection();

	wxArrayString list = GetAvailableCMs();
	m_list->Clear();
	m_list->Append( list );
	m_currentCM->Clear();
	m_currentCM->Append( list );
	if (list.Index( sel ) != wxNOT_FOUND )
		m_list->SetStringSelection( sel );

	if (list.Index( run ) != wxNOT_FOUND )
		m_currentCM->SetStringSelection( run );
}
