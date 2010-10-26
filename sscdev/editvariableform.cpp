#include "editvariableform.h"

/*user.global.start*/
/*user.global.end*/
enum {
  ID_btnChooseFile,
  ID_txtValue,
  ID_Label1,
  ID_numValue,
  ID_Label11,
  ID_rbgVarType,
  ID_btnCancel,
  ID_btnAccept,
  ID_grdArrMat,
  ID_numCols,
  ID_Label21,
  ID_Label2,
  ID_numRows };

BEGIN_EVENT_TABLE( EditVariableForm, wxPanel )
/*user.eventtable.start*/
EVT_NUMERIC( ID_numRows, EditVariableForm::OnRowsColsChange )
EVT_NUMERIC( ID_numCols, EditVariableForm::OnRowsColsChange )
EVT_TEXT_ENTER( ID_txtValue, EditVariableForm::OnTextChange )
EVT_BUTTON( ID_btnChooseFile, EditVariableForm::OnChooseFile )
EVT_NUMERIC( ID_numValue, EditVariableForm::OnNumChange )
EVT_GRID_CMD_CELL_CHANGE( ID_grdArrMat, EditVariableForm::OnGridCellChange )
EVT_RADIOBUTTON( ID_rbgVarType, EditVariableForm::OnTypeChange )
/*user.eventtable.end*/
END_EVENT_TABLE()

EditVariableForm::EditVariableForm(wxWindow *parent, int id)
	 : wxPanel( parent, id )
{
/*user.klsinit.start*/
/*user.klsinit.end*/
	SetClientSize( 592, 452 );
	numRows = new AFNumeric(this, ID_numRows, 3, true, wxPoint(375,75), wxSize(70,21));
	numRows->SetFormat( "%d");
	numRows->SetInt( (int) 3 );
	numCols = new AFNumeric(this, ID_numCols, 4, true, wxPoint(507,75), wxSize(70,21));
	numCols->SetFormat( "%d");
	numCols->SetInt( (int) 4 );
	btnAccept = new wxButton(this, ID_btnAccept, "Accept", wxPoint(420,423), wxSize(80,21));
	btnCancel = new wxButton(this, ID_btnCancel, "Cancel", wxPoint(504,423), wxSize(80,21));
	wxArrayString _data_rbgVarType;
	_data_rbgVarType.Add("SSC_STRING");
	_data_rbgVarType.Add("SSC_NUMBER");
	_data_rbgVarType.Add("SSC_ARRAY");
	_data_rbgVarType.Add("SSC_MATRIX");
	rbgVarType = new AFRadioChoice(this, ID_rbgVarType, wxPoint(9,9), wxSize(137,86));
	rbgVarType->Add( _data_rbgVarType);
	numValue = new AFNumeric(this, ID_numValue, 0, false, wxPoint(270,33), wxSize(100,21));
	numValue->SetFormat( "%lg");
	numValue->SetDouble( 0 );
	txtValue = new AFTextCtrl(this, ID_txtValue, wxPoint(270,9), wxSize(262,21));
	txtValue->ChangeValue("");
	txtValue->SetForegroundColour( wxColour(0, 0, 0) );
	txtValue->SetBackgroundColour( wxColour(255, 255, 255) );
	btnChooseFile = new wxButton(this, ID_btnChooseFile, "file..", wxPoint(534,9), wxSize(50,21));
	Label1 = new AFLabel(this, ID_Label1, "String value:", wxPoint(156,9), wxSize(110,21));
	Label1->AlignRight();
	Label1->SetColour(wxColour(0, 0, 0));
	Label1->SetRelativeSize(0);
	Label11 = new AFLabel(this, ID_Label11, "Numeric value:", wxPoint(156,33), wxSize(110,21));
	Label11->AlignRight();
	Label11->SetColour(wxColour(0, 0, 0));
	Label11->SetRelativeSize(0);
	grdArrMat = new WFGridCtrl(this, ID_grdArrMat, wxPoint(9,99), wxSize(572,318));
	grdArrMat->CreateGrid(2,2);
	grdArrMat->EnableEditing(true);
	grdArrMat->DisableDragCell();
	grdArrMat->DisableDragColSize();
	grdArrMat->DisableDragRowSize();
	grdArrMat->DisableDragColMove();
	grdArrMat->DisableDragGridSize();
	grdArrMat->SetRowLabelSize(23);
	grdArrMat->SetColLabelSize(23);
	Label21 = new AFLabel(this, ID_Label21, "# cols:", wxPoint(447,75), wxSize(59,21));
	Label21->AlignRight();
	Label21->SetColour(wxColour(0, 0, 0));
	Label21->SetRelativeSize(0);
	Label2 = new AFLabel(this, ID_Label2, "# rows:", wxPoint(315,75), wxSize(59,21));
	Label2->AlignRight();
	Label2->SetColour(wxColour(0, 0, 0));
	Label2->SetRelativeSize(0);
/*user.constructor.start*/
/*user.constructor.end*/
}
EditVariableForm::~EditVariableForm()
{
/*user.destructor.start*/
/*user.destructor.end*/
}
/*user.class.start*/
void EditVariableForm::UpdateForm()	
{
	rbgVarType->SetSelection( m_var.type-1 );
	if (m_var.type == SSC_STRING)txtValue->ChangeValue( m_var.str );

	if (m_var.type == SSC_NUMBER)numValue->SetDouble( m_var.num );

	if (m_var.type == SSC_ARRAY)
	{
		grdArrMat->Freeze();
		grdArrMat->ResizeGrid( m_var.num.length(), 1 );
		numRows->SetInt( m_var.num.length() );
		numCols->SetInt( 1 );
		for (int i=0;i<m_var.num.length();i++)
			grdArrMat->SetCellValue( wxString::Format("%lg", (double) m_var.num[i]), i, 0 );

		grdArrMat->Thaw();
	}

	if (m_var.type == SSC_MATRIX)
	{
		grdArrMat->Freeze();
		grdArrMat->ResizeGrid( m_var.num.nrows(), m_var.num.ncols() );
		numRows->SetInt( m_var.num.nrows() );
		numCols->SetInt( m_var.num.ncols() );

		for (int r=0;r<m_var.num.nrows();r++)
			for (int c=0;c<m_var.num.ncols();c++)
				grdArrMat->SetCellValue( wxString::Format("%lg", (double) m_var.num.at(r,c)), r, c );

		grdArrMat->Thaw();
	}

	txtValue->Enable( m_var.type == SSC_STRING );
	numValue->Enable( m_var.type == SSC_NUMBER );
	grdArrMat->Enable( m_var.type == SSC_ARRAY || m_var.type == SSC_MATRIX );
	numRows->Enable( m_var.type == SSC_ARRAY || m_var.type == SSC_MATRIX );
	numCols->Enable( m_var.type == SSC_MATRIX );
}

void EditVariableForm::OnTypeChange( wxCommandEvent &evt )
{
	m_var.type = rbgVarType->GetSelection()+1;
	UpdateForm();	
}

void EditVariableForm::OnTextChange( wxCommandEvent &evt )
{
	m_var.str = (const char*)txtValue->GetValue().c_str();
}

void EditVariableForm::OnNumChange( wxCommandEvent &evt)
{
	m_var.num = (double) numValue->GetDouble();
}

void EditVariableForm::OnGridCellChange( wxGridEvent &evt )
{
	int r, c;
	r = evt.GetRow();
	c = evt.GetCol();

	if (r < 0 || c < 0) return;

	double val = atof( grdArrMat->GetCellValue(r,c).c_str() );

	if (m_var.type == SSC_MATRIX)
	{
		if (r < m_var.num.nrows() && c < m_var.num.ncols())
			m_var.num.at(r,c) = val;
	}
	else if (m_var.type == SSC_ARRAY)
	{
		if (r < m_var.num.length())
			m_var.num[r] = val;
	}

	grdArrMat->SetCellValue(wxString::Format("%lg",val),r,c);
}

void EditVariableForm::OnChooseFile( wxCommandEvent &evt )
{
	wxFileDialog fd( this, "Choose a file");
	if (fd.ShowModal() == wxID_OK)
	{
		wxString file = fd.GetPath();
		file.Replace("\\","/");
		txtValue->ChangeValue( file );
		m_var.str = (const char*)file.c_str();
	}
}

void EditVariableForm::OnRowsColsChange( wxCommandEvent &evt )
{
	size_t nr, nc;

	nr = (size_t) numRows->GetInt();
	nc = (size_t) numCols->GetInt();

	if (nr < 0 || nc < 0) return;

	if (m_var.type == SSC_ARRAY)
	{
		util::matrix_t<ssc_number_t> old;
		old.copy(m_var.num);
		m_var.num.resize_fill( nr, 0.0 );
		for (size_t i=0;i<m_var.num.length();i++)
			m_var.num[i] = (i<old.length()) ? old[i] : 0.0;
	}
	else
	{
		util::matrix_t<ssc_number_t> old;
		old.copy( m_var.num );

		m_var.num.resize_fill( nr, nc, 0.0 );	
		for (size_t r=0;r<nr;r++)
			for (size_t c=0;c<nc;c++)
				m_var.num.at(r,c) = (r<old.nrows()&&c<old.ncols()) ? old.at(r,c) : 0.0;
	}
	UpdateForm();
}

/*user.class.end*/
BEGIN_EVENT_TABLE( EditVariableFormDialog, wxDialog )
/*user.dialogevents.start*/
EVT_BUTTON(ID_btnAccept, EditVariableFormDialog::OnCommand )
EVT_BUTTON(ID_btnCancel, EditVariableFormDialog::OnCommand )
/*user.dialogevents.end*/
EVT_CLOSE(EditVariableFormDialog::OnClose)

END_EVENT_TABLE()

EditVariableFormDialog::EditVariableFormDialog(wxWindow *parent, const wxString &title, void *data)
	 : wxDialog( parent, -1, title 
/*user.dialogconstruct.start*/
/*user.dialogconstruct.end*/
	)
{
	mPanel = new EditVariableForm(this);
	wxSize _sz = mPanel->GetClientSize();
	SetClientSize(_sz.GetWidth(), _sz.GetHeight());
/*user.dialoginit.start*/
	SetEscapeId(ID_btnCancel);
/*user.dialoginit.end*/
}

/*user.dialog.start*/
/*user.dialog.end*/

void EditVariableFormDialog::OnCommand(wxCommandEvent &evt)
{
/*user.oncommand.start*/
	if (evt.GetId() == ID_btnAccept) EndModal(wxID_OK);
	else EndModal(wxID_CANCEL);
/*user.oncommand.end*/
}

void EditVariableFormDialog::OnClose(wxCloseEvent &evt)
{
/*user.onclose.start*/
	EndModal(wxID_CANCEL);
/*user.onclose.end*/
}

/* end of EditVariableForm */

