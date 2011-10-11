#ifndef __lib_weatherfile_h
#define __lib_weatherfile_h

#define WFHDR_MAXLEN 64

#define WF_EPW  1
#define WF_TMY2 2
#define WF_TMY3 3
#define WF_SMW 4

struct wf_header_t
{
	int type;
	char loc_id[WFHDR_MAXLEN];
	char city[WFHDR_MAXLEN];
	char state[WFHDR_MAXLEN];
	double tz;
	double lat;
	double lon;
	double elev;
	double start; // start time in seconds, 0 = jan 1st midnight
	double step; // step time in seconds
	int nrecords; // number of data records in file
};

struct wf_record_t
{
	int year;
	int month;
	int day;
	int hour;
	double minute;
	double gh;   /* global (Wh/m2) */
	double dn;   /* direct (Wh/m2) */
	double df;   /* diffuse (Wh/m2) */
	double wspd; /* wind speed (m/s) */
	double wdir; /* wind direction (deg: N = 0 or 360, E = 90, S = 180,W = 270 ) */
	double tdry; /* dry bulb temp (C) */
	double twet; /* wet bulb temp (C) */
	double rhum; /* relative humidity (%) */
	double pres; /* pressure (mbar) */
	double snow; /* snow depth (cm) 0-150 */
	double albedo; /* ground reflectance 0-1.  values outside this range mean it is not included */
};

typedef void* wf_obj_t;

int  wf_get_type(const char *file);
int  wf_read_header(const char *file, wf_header_t *p_hdr);
wf_obj_t  wf_open(const char *file, wf_header_t *p_hdr);
int  wf_read_data( wf_obj_t wf, wf_record_t *dat);
void  wf_close(wf_obj_t wf);
void  wf_rewind(wf_obj_t wf);


#endif

