#include "cmform.h"

/*user.global.start*/
#include <wx/dirdlg.h>
/*user.global.end*/
enum {
  ID_txtParamValue,
  ID_Label5,
  ID_btnParamFile,
  ID_btnParamDir,
  ID_GroupBox1,
  ID_GroupBox2,
  ID_Label6,
  ID_lstSelectedCMs,
  ID_rbgParamType,
  ID_btnParamReset,
  ID_btnParamDel,
  ID_btnParamAdd,
  ID_lstParams,
  ID_btnAccept,
  ID_btnClose,
  ID_Label3,
  ID_Label4,
  ID_txtParams,
  ID_Label2,
  ID_cklCMList,
  ID_grdCMVars,
  ID_Label1 };

BEGIN_EVENT_TABLE( CMForm, wxPanel )
/*user.eventtable.start*/
EVT_CHECKLISTBOX(ID_cklCMList, CMForm::OnCMListCheck )
EVT_LISTBOX(ID_cklCMList, CMForm::OnCMListSelect )
EVT_LISTBOX(ID_lstParams, CMForm::OnParamCommand )
EVT_LISTBOX(ID_lstSelectedCMs, CMForm::OnSelCMSelect )
EVT_BUTTON(ID_btnParamAdd, CMForm::OnParamCommand )
EVT_BUTTON(ID_btnParamDel, CMForm::OnParamCommand )
EVT_BUTTON(ID_btnParamReset, CMForm::OnParamCommand )
EVT_TEXT_ENTER(ID_txtParamValue, CMForm::OnParamCommand )
EVT_RADIOBUTTON(ID_rbgParamType, CMForm::OnParamCommand )
EVT_BUTTON(ID_btnParamDir, CMForm::OnParamCommand )
EVT_BUTTON(ID_btnParamFile, CMForm::OnParamCommand )
/*user.eventtable.end*/
END_EVENT_TABLE()

CMForm::CMForm(wxWindow *parent, int id)
	 : wxPanel( parent, id )
{
/*user.klsinit.start*/
/*user.klsinit.end*/
	SetClientSize( 818, 582 );
	GroupBox1 = new wxStaticBox(this, ID_GroupBox1, "Parameters", wxPoint(228,408), wxSize(356,158));
	GroupBox2 = new wxStaticBox(this, ID_GroupBox2, "Configure simulation sequence", wxPoint(6,390), wxSize(587,188));
	wxArrayString _data_cklCMList;
	cklCMList = new wxCheckListBox(this, ID_cklCMList, wxPoint(9,27), wxSize(179,219), _data_cklCMList, 0);
	txtParams = new wxTextCtrl(this, ID_txtParams, "", wxPoint(9,252), wxSize(180,132),wxTE_MULTILINE|wxTE_DONTWRAP|wxTE_PROCESS_TAB);
	txtParams->SetFont(wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "courier"));
	txtParams->ChangeValue("");
	txtParams->SetEditable( false );
	btnClose = new wxButton(this, ID_btnClose, "Close", wxPoint(729,555), wxSize(80,21));
	btnAccept = new wxButton(this, ID_btnAccept, "Accept", wxPoint(645,555), wxSize(80,21));
	wxArrayString _data_lstParams;
	lstParams = new wxListBox(this, ID_lstParams, wxPoint(237,450), wxSize(122,84), _data_lstParams, wxLB_SINGLE);
	btnParamAdd = new wxButton(this, ID_btnParamAdd, "Add..", wxPoint(237,537), wxSize(38,21));
	btnParamDel = new wxButton(this, ID_btnParamDel, "Del", wxPoint(279,537), wxSize(38,21));
	btnParamReset = new wxButton(this, ID_btnParamReset, "Rst", wxPoint(321,537), wxSize(38,21));
	wxArrayString _data_rbgParamType;
	_data_rbgParamType.Add("SSC_STRING");
	_data_rbgParamType.Add("SSC_NUMBER");
	rbgParamType = new AFRadioChoice(this, ID_rbgParamType, wxPoint(363,429), wxSize(110,44));
	rbgParamType->Add( _data_rbgParamType);
	wxArrayString _data_lstSelectedCMs;
	lstSelectedCMs = new wxListBox(this, ID_lstSelectedCMs, wxPoint(21,429), wxSize(182,135), _data_lstSelectedCMs, wxLB_SINGLE);
	btnParamDir = new wxButton(this, ID_btnParamDir, "dir..", wxPoint(507,537), wxSize(32,21));
	btnParamFile = new wxButton(this, ID_btnParamFile, "file..", wxPoint(543,537), wxSize(32,21));
	txtParamValue = new AFTextCtrl(this, ID_txtParamValue, wxPoint(363,513), wxSize(214,21));
	txtParamValue->ChangeValue("");
	txtParamValue->SetForegroundColour( wxColour(128, 0, 64) );
	txtParamValue->SetBackgroundColour( wxColour(255, 255, 255) );
	Label5 = new AFLabel(this, ID_Label5, "Value:", wxPoint(363,489), wxSize(110,21));
	Label5->SetColour(wxColour(0, 0, 0));
	Label5->SetRelativeSize(0);
	Label6 = new AFLabel(this, ID_Label6, "Use the check list box to selection compute modules for simluation.  Then, for each compute module, set the simulation parameters required.", wxPoint(600,393), wxSize(209,150));
	Label6->AlignTop();
	Label6->SetColour(wxColour(0, 0, 0));
	Label6->SetRelativeSize(0);
	Label6->SetWordWrap( true );
	Label3 = new AFLabel(this, ID_Label3, "Selected CMs for simulation:", wxPoint(21,408), wxSize(182,21));
	Label3->SetColour(wxColour(0, 0, 0));
	Label3->SetRelativeSize(0);
	Label4 = new AFLabel(this, ID_Label4, "Parameter names:", wxPoint(237,429), wxSize(110,21));
	Label4->SetColour(wxColour(0, 0, 0));
	Label4->SetRelativeSize(0);
	Label2 = new AFLabel(this, ID_Label2, "CM Variable Table", wxPoint(192,6), wxSize(146,21));
	Label2->SetColour(wxColour(0, 0, 0));
	Label2->SetRelativeSize(0);
	grdCMVars = new WFGridCtrl(this, ID_grdCMVars, wxPoint(192,27), wxSize(617,357));
	grdCMVars->CreateGrid(2,2);
	grdCMVars->EnableEditing(false);
	grdCMVars->DisableDragCell();
	grdCMVars->DisableDragColSize();
	grdCMVars->DisableDragRowSize();
	grdCMVars->DisableDragColMove();
	grdCMVars->DisableDragGridSize();
	grdCMVars->SetRowLabelSize(23);
	grdCMVars->SetColLabelSize(23);
	Label1 = new AFLabel(this, ID_Label1, "Compute Modules", wxPoint(9,6), wxSize(137,21));
	Label1->SetColour(wxColour(0, 0, 0));
	Label1->SetRelativeSize(0);
/*user.constructor.start*/
	grdCMVars->EnableDragColSize();
	LoadCMs();
	UpdateForm();
/*user.constructor.end*/
}
CMForm::~CMForm()
{
/*user.destructor.start*/
/*user.destructor.end*/
}
/*user.class.start*/

wxArrayString CMForm::GetAvailableCMs()
{
	wxArrayString list;
	try {

		int idx=0;
		while (const ssc_entry_t p_entry = ::ssc_module_entry(idx++))
			list.Add( ::ssc_entry_name(p_entry) );

	} catch(sscdll_error e) {
		wxMessageBox("DLL error: " + e.func + ": " + e.text );
	}

	return list;
}

void CMForm::LoadCMs()
{
	wxArrayString l = GetAvailableCMs();
	for (size_t i=0;i<l.Count();i++)
		cklCMList->Append( l[i] );
}

void CMForm::OnCMListCheck(wxCommandEvent &evt)
{
	int idx = evt.GetSelection();
	if (idx < 0) return;
	wxString name = cklCMList->GetString( idx );
	bool ischk = cklCMList->IsChecked( idx );

	int cmsel_idx = -1;
	for (int i=0;i<m_cmList.count();i++)
		if (m_cmList[i].cm_mod_name == name)
			cmsel_idx = i;

	if (ischk && cmsel_idx < 0)
	{
		cmModule x;
		x.cm_mod_name = name;
		m_cmList.append( x );
		UpdateForm();
	}

	if (!ischk && cmsel_idx >= 0)
	{
		m_cmList.removeat( cmsel_idx );
		UpdateForm();
	}
}

void CMForm::OnCMListSelect(wxCommandEvent &evt)
{
	try {
		wxString cm_name = cklCMList->GetStringSelection();
	
		ssc_module_t p_mod = ::ssc_module_create( (const char*)cm_name.c_str() );
		if ( p_mod == 0 )
		{
			wxMessageBox("Could not create module of type: " + cm_name);
			return;
		}

		Array<wxArrayString> vartab;

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

			vartab.append(row);
		}

		int nrows = vartab.count();
		int ncols = 7;

		grdCMVars->ResizeGrid( nrows, ncols);
		grdCMVars->SetColLabelValue( 0, "TYPE" );
		grdCMVars->SetColLabelValue( 1, "DATA" );
		grdCMVars->SetColLabelValue( 2, "NAME" );
		grdCMVars->SetColLabelValue( 3, "LABEL" );
		grdCMVars->SetColLabelValue( 4, "UNITS" );
		grdCMVars->SetColLabelValue( 5, "META" );
		grdCMVars->SetColLabelValue( 6, "GROUP" );

		for (int r=0;r<nrows;r++)
			for (int c=0;c<ncols;c++)
				grdCMVars->SetCellValue( vartab[r][c], r, c );

		txtParams->Clear();
		idx=0;
		while (const ssc_param_t p_param = ::ssc_module_parameter( p_mod, idx++ ))
		{
			const char *name = ::ssc_param_name(p_param);
			int type = ::ssc_param_type(p_param);
			const char *defval = ::ssc_param_default_value(p_param);
			wxString typ = type==SSC_STRING?"str":"num";
			txtParams->AppendText( "'" + wxString(name) + "' (" +typ+ ")={" + wxString(defval) + "}\n");		
		}

		if (idx==1) txtParams->AppendText("No parameters.");

		::ssc_module_free( p_mod );
	
	} catch(sscdll_error e) {
		wxMessageBox("DLL error: " + e.func + ": " + e.text );
	}
}


void CMForm::UpdateForm()
{
	wxArrayString list;
	lstSelectedCMs->Clear();
	for (int i=0;i<m_cmList.count();i++)
	{
		lstSelectedCMs->Append( m_cmList[i].cm_mod_name );
		list.Add( m_cmList[i].cm_mod_name );
	}

	for (int i=0;i<cklCMList->GetCount();i++)
		cklCMList->Check( i, list.Index( cklCMList->GetString(i) ) != wxNOT_FOUND );

	UpdateParamList();
}

void CMForm::UpdateParamList()
{
	lstParams->Clear();

	int idx = lstSelectedCMs->GetSelection();
	
	if (idx >= 0)
	{
		cmModule &cm = m_cmList[idx];
		
		for (int i=0;i<cm.params.count();i++)
			lstParams->Append( cm.params[i].name );
	}
	
	lstParams->Enable( idx >= 0 );

	UpdateParam();
}

void CMForm::UpdateParam()
{
	int cmidx = lstSelectedCMs->GetSelection();
	int pidx = lstParams->GetSelection();
	txtParamValue->Clear();
		
	if (cmidx >= 0 && cmidx < m_cmList.count()
		&& pidx >= 0 && pidx < m_cmList[cmidx].params.count())
	{
		cmParam &p = m_cmList[cmidx].params[pidx];

		rbgParamType->SetSelection( p.type==SSC_STRING ? 0 : 1 );
		txtParamValue->SetValue( p.type==SSC_STRING ? p.str : FloatToStr(p.num) );
	}
	
	txtParamValue->Enable( pidx >= 0 );
	rbgParamType->Enable( pidx >= 0 );
	btnParamFile->Enable( pidx >= 0 && rbgParamType->GetSelection()==0);
	btnParamDir->Enable( pidx >= 0  && rbgParamType->GetSelection()==0);
	btnParamAdd->Enable( cmidx >= 0 );
	btnParamDel->Enable( cmidx >= 0 );
	btnParamReset->Enable( cmidx >= 0 );
}

void CMForm::OnSelCMSelect(wxCommandEvent &evt)
{
	UpdateParamList();
}

void CMForm::OnParamCommand(wxCommandEvent &evt)
{
	int cmidx = lstSelectedCMs->GetSelection();
	int pidx = lstParams->GetSelection();

	switch (evt.GetId())
	{
	case ID_lstParams:
		UpdateParam();
		break;
	case ID_btnParamAdd:
		{
			if (cmidx >= 0 && cmidx < m_cmList.count())
			{
				wxString name = wxGetTextFromUser("enter parameter name:");
				if (name.IsEmpty()) return;
			
				cmParam x;
				x.name = name;
				x.type = SSC_NUMBER;
				x.num = 0.0;

				m_cmList[cmidx].params.append(x);
				UpdateParamList();
			}
		}
		break;
	case ID_btnParamDel:
		{
			int cmidx = lstSelectedCMs->GetSelection();
			int pidx = lstParams->GetSelection();
			if (cmidx >= 0 && cmidx < m_cmList.count()
				&& pidx >= 0 && pidx < m_cmList[cmidx].params.count())
			{
				m_cmList[cmidx].params.removeat( pidx );
				UpdateParamList();
			}
		}
		break;
	case ID_btnParamReset:
		{
		}
		break;
	case ID_txtParamValue:
		{
			if (cmidx >= 0 && cmidx < m_cmList.count()
				&& pidx >= 0 && pidx < m_cmList[cmidx].params.count())
			{
				cmParam &p = m_cmList[cmidx].params[pidx];

				if (p.type == SSC_STRING)
				{
					p.str = txtParamValue->GetValue();
					applog("set " + m_cmList[cmidx].cm_mod_name + " param: " + p.name + "=" + p.str);
				}
				else
				{
					p.num = atof( txtParamValue->GetValue().c_str() );
					txtParamValue->ChangeValue(wxString::Format("%lg", (double)p.num));
					
					applog("set " + m_cmList[cmidx].cm_mod_name + " param: " + p.name + "=" + FloatToStr(p.num));
				}
			}
		}
		break;
	case ID_rbgParamType:
			if (cmidx >= 0 && cmidx < m_cmList.count()
				&& pidx >= 0 && pidx < m_cmList[cmidx].params.count())
			{
				m_cmList[cmidx].params[pidx].type = rbgParamType->GetSelection()==0 ? SSC_STRING : SSC_NUMBER;

				btnParamFile->Enable( m_cmList[cmidx].params[pidx].type == SSC_STRING );
				btnParamDir->Enable( m_cmList[cmidx].params[pidx].type == SSC_STRING );
			}
		break;
	case ID_btnParamFile:
		{
			wxFileDialog fd( this, "Choose a file");
			if (fd.ShowModal() == wxID_OK)
			{
				wxString file = fd.GetPath();
				file.Replace("\\","/");
				txtParamValue->ChangeValue( file );

				if (cmidx >= 0 && cmidx < m_cmList.count()
					&& pidx >= 0 && pidx < m_cmList[cmidx].params.count())
					m_cmList[cmidx].params[pidx].str = file;
			}
		}
		break;
	case ID_btnParamDir:
		{
			wxDirDialog dd( this, "Choose a file");
			if (dd.ShowModal() == wxID_OK)
			{
				wxString dir = dd.GetPath();
				dir.Replace("\\","/");
				txtParamValue->ChangeValue( dir );
				
				if (cmidx >= 0 && cmidx < m_cmList.count()
					&& pidx >= 0 && pidx < m_cmList[cmidx].params.count())
					m_cmList[cmidx].params[pidx].str = dir;
			}			
		}
		break;
	}
}

/*user.class.end*/
BEGIN_EVENT_TABLE( CMFormDialog, wxDialog )
/*user.dialogevents.start*/
EVT_BUTTON(ID_btnAccept, CMFormDialog::OnCommand)
EVT_BUTTON(ID_btnClose, CMFormDialog::OnCommand)
/*user.dialogevents.end*/
EVT_CLOSE(CMFormDialog::OnClose)

END_EVENT_TABLE()

CMFormDialog::CMFormDialog(wxWindow *parent, const wxString &title, void *data)
	 : wxDialog( parent, -1, title 
/*user.dialogconstruct.start*/
/*user.dialogconstruct.end*/
	)
{
	mPanel = new CMForm(this);
	wxSize _sz = mPanel->GetClientSize();
	SetClientSize(_sz.GetWidth(), _sz.GetHeight());
/*user.dialoginit.start*/
	SetEscapeId(ID_btnClose);
/*user.dialoginit.end*/
}

/*user.dialog.start*/
/*user.dialog.end*/

void CMFormDialog::OnCommand(wxCommandEvent &evt)
{
/*user.oncommand.start*/
	if (evt.GetId() == ID_btnAccept)
		EndModal(wxID_OK);
	else
		EndModal(wxID_CANCEL);
/*user.oncommand.end*/
}

void CMFormDialog::OnClose(wxCloseEvent &evt)
{
/*user.onclose.start*/
	EndModal(wxID_CANCEL);
/*user.onclose.end*/
}

/* end of CMForm */

