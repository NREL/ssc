#ifndef __lib_wfhrly_h
#define __lib_wfhrly_h

#define WFHDR_MAXLEN 64

#define WF_EPW  1
#define WF_TMY2 2
#define WF_TMY3 3

struct __wf_header
{
	int type;
	char loc_id[WFHDR_MAXLEN];
	char city[WFHDR_MAXLEN];
	char state[WFHDR_MAXLEN];
	double tz;
	double lat;
	double lon;
	double elev;
};
typedef struct __wf_header wf_header;

struct __wf_data
{
	int year;
	int month;
	int day;
	int hour;
	double gh;   /* global (Wh/m2) */
	double dn;   /* direct (Wh/m2) */
	double df;   /* diffuse (Wh/m2) */
	double wspd; /* wind speed (m/s) */
	double wdir; /* wind direction (deg: N = 0 or 360, E = 90, S = 180,W = 270 ) */
	double tdry; /* dry bulb temp (C) */
	double twet; /* wet bulb temp (C) */
	double rhum; /* relative humidity (%) */
	double pres; /* pressure (mbar) */
};
typedef struct __wf_data wf_data;

typedef void* wf_obj_t;

int  wf_get_type(const char *file);
int  wf_read_header(const char *file, wf_header *p_hdr);
wf_obj_t  wf_open(const char *file, wf_header *p_hdr);
int  wf_read_data( wf_obj_t wf, wf_data *dat);
void  wf_close(wf_obj_t wf);
void  wf_rewind(wf_obj_t wf);


#endif

