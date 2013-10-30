#include "statdlg.h"

#include <wex/numeric.h>
#include <wex/extgrid.h>

StatDialog::StatDialog( wxWindow *parent, const wxString &title )
	 : wxDialog( parent, wxID_ANY, title, 
		wxDefaultPosition, wxDefaultSize, 
		wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{
	wxBoxSizer *sz_h1 = new wxBoxSizer( wxHORIZONTAL );
	
	sz_h1->Add( new wxStaticText( this, wxID_ANY, "Mean:" ), 0, wxLEFT|wxALIGN_CENTER_VERTICAL, 5 );
	sz_h1->Add( numMean = new wxNumericCtrl(this) );
	sz_h1->Add( new wxStaticText( this, wxID_ANY, "Min:" ), 0, wxLEFT|wxALIGN_CENTER_VERTICAL, 5 );
	sz_h1->Add( numMin = new wxNumericCtrl(this) );
	sz_h1->Add( new wxStaticText( this, wxID_ANY, "Max:" ), 0, wxLEFT|wxALIGN_CENTER_VERTICAL, 5 );
	sz_h1->Add( numMax = new wxNumericCtrl(this) );

	wxBoxSizer *sz_h2 = new wxBoxSizer( wxHORIZONTAL );

	sz_h2->Add( new wxStaticText( this, wxID_ANY, "Sum:" ), 0, wxLEFT|wxALIGN_CENTER_VERTICAL, 5 );
	sz_h2->Add( numSum = new wxNumericCtrl(this) );
	sz_h2->Add( new wxStaticText( this, wxID_ANY, "Sum/1000:" ), 0, wxLEFT|wxALIGN_CENTER_VERTICAL, 5 );
	sz_h2->Add( numSumOver1000 = new wxNumericCtrl(this) );
	
	grdMonthly = new wxExtGridCtrl(this, wxID_ANY);
	grdMonthly->CreateGrid(12,4);
	grdMonthly->EnableEditing(false);
	grdMonthly->DisableDragCell();
	grdMonthly->DisableDragColSize();
	grdMonthly->DisableDragRowSize();
	grdMonthly->DisableDragColMove();
	grdMonthly->DisableDragGridSize();
	grdMonthly->SetRowLabelSize(23);
	grdMonthly->SetColLabelSize(23);

	wxBoxSizer *sz_main = new wxBoxSizer( wxVERTICAL );
	sz_main->Add( sz_h1 );	
	sz_main->Add( sz_h2 );
	sz_main->Add( grdMonthly, 1, wxALL|wxEXPAND, 5 );
	sz_main->Add( CreateButtonSizer( wxOK ), 0, wxALL|wxEXPAND, 5 );

	SetSizer( sz_main );
	Fit();
}

void StatDialog::Compute( util::matrix_t<ssc_number_t> &val )
{
static int nday[12] = {31,28,31,30,31,30,31,31,30,31,30,31}; 

	size_t len = val.length();
	ssc_number_t *pvals = val.data();

	ssc_number_t min, max, mean, sum;
	ssc_number_t mmin[12],mmax[12],mmean[12],msum[12];

	size_t i,j;
	min = (ssc_number_t)1e19;
	max = (ssc_number_t)-1e19;
	mean=sum=0.0;
	for (i=0;i<12;i++)
	{
		mmin[i]=(ssc_number_t)1e19;
		mmax[i]=(ssc_number_t)-1e19;
		mmean[i]=msum[i] = 0;
	}


	for (i=0;i<len;i++)
	{
		if (pvals[i] < min) min = pvals[i];
		if (pvals[i] > max) max = pvals[i];
		sum += pvals[i];
	}

	mean = sum/((ssc_number_t)len);

	numMin->SetValue( min );
	numMax->SetValue( max );
	numMean->SetValue( mean );
	numSum->SetValue( sum );
	numSumOver1000->SetValue( sum/1000.0 );

	size_t multiple = len / 8760;
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
		grdMonthly->SetCellValue( wxString::Format("%lg", mmin[i] ), i, 0 );
		grdMonthly->SetCellValue( wxString::Format("%lg", mmax[i] ), i, 1 );
		grdMonthly->SetCellValue( wxString::Format("%lg", mmean[i] ), i, 2 );
		grdMonthly->SetCellValue( wxString::Format("%lg", msum[i] ), i, 3 );
		grdMonthly->SetCellValue( wxString::Format("%lg", msum[i]/1000.0f ), i, 4 );
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
