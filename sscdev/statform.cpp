#include "statform.h"

/*user.global.start*/
/*user.global.end*/
enum {
  ID_btnClose,
  ID_GroupBox1,
  ID_grdMonthly,
  ID_numSumOver1000,
  ID_Label11111,
  ID_numSum,
  ID_Label1111,
  ID_numMax,
  ID_numMean,
  ID_Label111,
  ID_Label1,
  ID_Label11,
  ID_numMin };

BEGIN_EVENT_TABLE( StatForm, wxPanel )
/*user.eventtable.start*/
/*user.eventtable.end*/
END_EVENT_TABLE()

StatForm::StatForm(wxWindow *parent, int id)
	 : wxPanel( parent, id )
{
/*user.klsinit.start*/
/*user.klsinit.end*/
	SetClientSize( 504, 402 );
	GroupBox1 = new wxStaticBox(this, ID_GroupBox1, "Monthly Values", wxPoint(12,60), wxSize(485,302));
	numMin = new AFNumeric(this, ID_numMin, 0, false, wxPoint(66,9), wxSize(100,21));
	numMin->SetFormat( "%lg");
	numMin->SetDouble( 0 );
	numMin->SetEditable( false );
	numMean = new AFNumeric(this, ID_numMean, 0, false, wxPoint(231,9), wxSize(100,21));
	numMean->SetFormat( "%lg");
	numMean->SetDouble( 0 );
	numMean->SetEditable( false );
	numMax = new AFNumeric(this, ID_numMax, 0, false, wxPoint(66,33), wxSize(100,21));
	numMax->SetFormat( "%lg");
	numMax->SetDouble( 0 );
	numMax->SetEditable( false );
	numSum = new AFNumeric(this, ID_numSum, 0, false, wxPoint(231,33), wxSize(100,21));
	numSum->SetFormat( "%lg");
	numSum->SetDouble( 0 );
	numSum->SetEditable( false );
	numSumOver1000 = new AFNumeric(this, ID_numSumOver1000, 0, false, wxPoint(396,33), wxSize(100,21));
	numSumOver1000->SetFormat( "%lg");
	numSumOver1000->SetDouble( 0 );
	numSumOver1000->SetEditable( false );
	btnClose = new wxButton(this, ID_btnClose, "Close", wxPoint(414,372), wxSize(80,21));
	grdMonthly = new WFGridCtrl(this, ID_grdMonthly, wxPoint(21,78), wxSize(467,267));
	grdMonthly->CreateGrid(12,4);
	grdMonthly->EnableEditing(false);
	grdMonthly->DisableDragCell();
	grdMonthly->DisableDragColSize();
	grdMonthly->DisableDragRowSize();
	grdMonthly->DisableDragColMove();
	grdMonthly->DisableDragGridSize();
	grdMonthly->SetRowLabelSize(23);
	grdMonthly->SetColLabelSize(23);
	Label11111 = new AFLabel(this, ID_Label11111, "Sum/1000", wxPoint(342,33), wxSize(53,21));
	Label11111->AlignRight();
	Label11111->SetColour(wxColour(0, 0, 0));
	Label11111->SetRelativeSize(0);
	Label1111 = new AFLabel(this, ID_Label1111, "Sum", wxPoint(177,33), wxSize(53,21));
	Label1111->AlignRight();
	Label1111->SetColour(wxColour(0, 0, 0));
	Label1111->SetRelativeSize(0);
	Label111 = new AFLabel(this, ID_Label111, "Mean", wxPoint(177,9), wxSize(53,21));
	Label111->AlignRight();
	Label111->SetColour(wxColour(0, 0, 0));
	Label111->SetRelativeSize(0);
	Label1 = new AFLabel(this, ID_Label1, "Min", wxPoint(12,9), wxSize(53,21));
	Label1->AlignRight();
	Label1->SetColour(wxColour(0, 0, 0));
	Label1->SetRelativeSize(0);
	Label11 = new AFLabel(this, ID_Label11, "Max", wxPoint(12,33), wxSize(53,21));
	Label11->AlignRight();
	Label11->SetColour(wxColour(0, 0, 0));
	Label11->SetRelativeSize(0);
/*user.constructor.start*/
/*user.constructor.end*/
}
StatForm::~StatForm()
{
/*user.destructor.start*/
/*user.destructor.end*/
}
/*user.class.start*/

void StatForm::Compute( util::matrix_t<ssc_number_t> &val )
{
static int nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31}; 

	size_t len = val.length();
	ssc_number_t *pvals = val.data();

	ssc_number_t min, max, mean, sum;
	ssc_number_t mmin[12],mmax[12],mmean[12],msum[12];

	int i,j;
	min = (ssc_number_t)1e99;
	max = (ssc_number_t)-1e99;
	mean=sum=0.0;
	for (i=0;i<12;i++)
	{
		mmin[i]=(ssc_number_t)1e99;
		mmax[i]=(ssc_number_t)-1e99;
		mmean[i]=msum[i] = (ssc_number_t)-999.9;
	}


	for (i=0;i<len;i++)
	{
		if (pvals[i] < min) min = pvals[i];
		if (pvals[i] > max) max = pvals[i];
		sum += pvals[i];
	}

	mean = sum/((ssc_number_t)len);

	numMin->SetDouble( min );
	numMax->SetDouble( max );
	numMean->SetDouble( mean );
	numSum->SetDouble( sum );
	numSumOver1000->SetDouble( sum/1000.0f );

	int multiple = len / 8760;
	if ( multiple*8760 == len )
	{
		i=0;
		for (int m=0;m<12;m++)
		{
			for (int d=0;d<nday[m];d++)
			{
				for (int h=0;h<24;h++)
				{
					ssc_number_t val = 0.0;
					for (j=0;j<multiple;j++)
						val += pvals[i*multiple+j];


					if (val < mmin[m]) mmin[m] = val;
					if (val > mmax[m]) mmax[m] = val;
					msum[m] += val;

					i++;
				}
			}

			mmean[m] = msum[m] / ( nday[m]*24 );
		}
	}

	grdMonthly->ResizeGrid(12,5);
	for (i=0;i<12;i++)
	{
		grdMonthly->SetCellValue( FloatToStr( mmin[i] ), i, 0 );
		grdMonthly->SetCellValue( FloatToStr( mmax[i] ), i, 1 );
		grdMonthly->SetCellValue( FloatToStr( mmean[i] ), i, 2 );
		grdMonthly->SetCellValue( FloatToStr( msum[i] ), i, 3 );
		grdMonthly->SetCellValue( FloatToStr( msum[i]/1000.0f ), i, 4 );
	}
	
	grdMonthly->SetRowLabelValue(0, "Jan");
	grdMonthly->SetRowLabelValue(1, "Feb");
	grdMonthly->SetRowLabelValue(2, "Mar");
	grdMonthly->SetRowLabelValue(3, "Apr");
	grdMonthly->SetRowLabelValue(4, "May");
	grdMonthly->SetRowLabelValue(5, "Jun");
	grdMonthly->SetRowLabelValue(6, "Jul");
	grdMonthly->SetRowLabelValue(7, "Aug");
	grdMonthly->SetRowLabelValue(8, "Sep");
	grdMonthly->SetRowLabelValue(9, "Oct");
	grdMonthly->SetRowLabelValue(10, "Nov");
	grdMonthly->SetRowLabelValue(11, "Dec");

	grdMonthly->SetColLabelValue(0, "Min");
	grdMonthly->SetColLabelValue(1, "Max");
	grdMonthly->SetColLabelValue(2, "Mean");
	grdMonthly->SetColLabelValue(3, "Sum");
	grdMonthly->SetColLabelValue(4, "Sum/1000");

	grdMonthly->SetRowLabelSize( 40 );
	grdMonthly->SetColLabelSize( wxGRID_AUTOSIZE );

}

/*user.class.end*/
BEGIN_EVENT_TABLE( StatFormDialog, wxDialog )
/*user.dialogevents.start*/
EVT_BUTTON(ID_btnClose, StatFormDialog::OnCommand)
/*user.dialogevents.end*/
EVT_CLOSE(StatFormDialog::OnClose)

END_EVENT_TABLE()

StatFormDialog::StatFormDialog(wxWindow *parent, const wxString &title, void *data)
	 : wxDialog( parent, -1, title 
/*user.dialogconstruct.start*/
/*user.dialogconstruct.end*/
	)
{
	mPanel = new StatForm(this);
	wxSize _sz = mPanel->GetClientSize();
	SetClientSize(_sz.GetWidth(), _sz.GetHeight());
/*user.dialoginit.start*/
	SetEscapeId(ID_btnClose);
/*user.dialoginit.end*/
}

/*user.dialog.start*/
/*user.dialog.end*/

void StatFormDialog::OnCommand(wxCommandEvent &evt)
{
/*user.oncommand.start*/
	EndModal(wxID_OK);
/*user.oncommand.end*/
}

void StatFormDialog::OnClose(wxCloseEvent &evt)
{
/*user.onclose.start*/
	EndModal(wxID_OK);
/*user.onclose.end*/
}

/* end of StatForm */

