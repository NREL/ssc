#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)
#define CASECMP(a,b) _stricmp(a,b)
#define CASENCMP(a,b,n) _strnicmp(a,b,n)
#else
#define CASECMP(a,b) strcasecmp(a,b)
#define CASENCMP(a,b,n) strncasecmp(a,b,n)
#endif

#include "lib_wfhrly.h"

struct __wf_object
{
	int wf_type;
	FILE *fp;
};
typedef struct __wf_object wf_object;
#define WF_OBJECT(x) ((wf_object*)x)

static double conv_deg_min_sec(double degrees, 
								double minutes, 
								double seconds, 
								char direction)
{
	double dd = degrees + minutes/60.0 + seconds/3600.0;
	if ( tolower((int)direction) == 's' || tolower((int)direction) == 'w')
		dd = 0 - dd;
	return dd;
}


static int locate(char *buf, char **colidx, int colmax, char delim)
{
	char *p = buf;
	int i = 1;
	int ncols = 0;
	
	colidx[0] = p;
	while (p && *p && i < colmax)
	{
		p = strchr(p, delim);
		if (p) colidx[i++] = ++p;
	}

	ncols = i;

	while (i<colmax) colidx[i++] = 0;

	return ncols;
}

/* 
   version of strtok_r from (2010/9/24)
   http://www.koders.com/c/fid9E7961E1E818E911DA7C34DD56DD8782726B3FCD.aspx
   */
char *gettoken (char *s, const char *delim, char **save_ptr)
{
	char *token;

	if (s == NULL)
		s = *save_ptr;

	/* Scan leading delimiters.  */
	s += strspn (s, delim);
	if (*s == '\0')
	{
		*save_ptr = s;
		return NULL;
	}

	/* Find the end of the token.  */
	token = s;
	s = strpbrk (token, delim);
	
	if (s == NULL)
		/* This token finishes the string.  */
		*save_ptr = strchr (token, '\0');
	else
	{
		/* Terminate the token and make *SAVE_PTR point past it.  */
		*s = '\0';
		*save_ptr = s + 1;
	}
	return token;
}

static int cmp_ext(const char *file, const char *ext)
{
	size_t len_ext, len_file;
	const char *extp;
	
	if (!file || !ext) return 0;
	len_ext = strlen(ext);
	len_file = strlen(file);
	extp = file+len_file-len_ext;
	
	if (extp < file) return 0;
	return CASENCMP(extp, ext, len_ext)==0 ? 1 : 0;
}

 int  wf_get_type(const char *file)
{
	if (!file) return -1;
	
	if (cmp_ext(file,"tm2") || cmp_ext(file, "tmy2"))
		return WF_TMY2;
	else if ( cmp_ext(file, "tm3") || cmp_ext(file, "tmy3") || cmp_ext(file, "csv") )
		return WF_TMY3;
	else if ( cmp_ext(file, "epw") )
		return WF_EPW;
	else
		return -1;	
}

 int  wf_read_header(const char *file, wf_header *p_hdr)
{
	wf_obj_t w = wf_open(file, p_hdr);
	if (w)
	{
		wf_close(w);
		return 1;
	}
	else
		return 0;
}

 wf_obj_t  wf_open(const char *file, wf_header *p_hdr)
{
	wf_header hdr;
	FILE *fp = NULL;
	wf_object *obj = NULL;
	char buf[2048];
	
	if (!file) return NULL;
	
	hdr.type = wf_get_type( file );
	if (hdr.type < 0) return NULL;
	
	fp = fopen(file, "r");
	if (!fp) return NULL;
	
	obj = (wf_object*) malloc(sizeof(wf_object));
	obj->wf_type = hdr.type;
	obj->fp = fp;
	
	
	/* read header information */
	if (hdr.type==WF_TMY2)
	{
	/*  93037 COLORADO_SPRINGS       CO  -7 N 38 49 W 104 43  1881 */
		char slat[10], slon[10];
		int dlat, mlat, dlon, mlon, elv;
		
		fgets(buf, 2047, fp);
		sscanf(buf, "%s %s %s %lg %s %d %d %s %d %d %d",
				hdr.loc_id, hdr.city, hdr.state,
				&hdr.tz,
				slat,&dlat,&mlat,
				slon,&dlon,&mlon,
				&elv);
		
		hdr.lat = conv_deg_min_sec(dlat, mlat, 0, slat[0]);
		hdr.lon = conv_deg_min_sec(dlon, mlon, 0, slon[0]);
		hdr.elev = elv;
	}
	else if(hdr.type==WF_TMY3)
	{
	/*  724699,"BROOMFIELD/JEFFCO [BOULDER - SURFRAD]",CO,-7.0,40.130,-105.240,1689 */
		char *lasts, *p;
		
		fgets(buf, 2047, fp);
		
		lasts = NULL;
		p = gettoken( buf, ",", &lasts );
		strncpy(hdr.loc_id, p, WFHDR_MAXLEN-1);
				
		p = gettoken( NULL, ",", &lasts);		
		strncpy(hdr.city, p, WFHDR_MAXLEN-1);
		
		p = gettoken(NULL, ",", &lasts);		
		strncpy(hdr.state, p, WFHDR_MAXLEN-1);
		
		p = gettoken(NULL, ",", &lasts);
		hdr.tz = atof( p );
		
		p = gettoken(NULL, ",", &lasts);
		hdr.lat = atof( p );
		
		p = gettoken(NULL, ",", &lasts);
		hdr.lon = atof( p );
		
		p = gettoken(NULL, ",", &lasts);
		hdr.elev = atof( p);
		
		fgets(buf, 2047, fp); /* skip over labels line */
	}
	else if(hdr.type==WF_EPW)
	{
	/*  LOCATION,Cairo Intl Airport,Al Qahirah,EGY,ETMY,623660,30.13,31.40,2.0,74.0 */
	/*  LOCATION,Alice Springs Airport,NT,AUS,RMY,943260,-23.80,133.88,9.5,547.0 */
		char *lasts, *p;
		
		fgets(buf, 2047, fp);
		
		lasts = NULL;
		p = gettoken( buf, ",", &lasts );
		/* skip LOCATION */

		p = gettoken( NULL, ",", &lasts );
		strncpy(hdr.city, p, WFHDR_MAXLEN-1); /* city */
		
		p = gettoken( NULL, ",", &lasts );
		strncpy(hdr.state, p, WFHDR_MAXLEN-1); /* state */
		
		p = gettoken( NULL, ",", &lasts );
		/* skip COUNTRY */
		
		p = gettoken( NULL, ",", &lasts );
		/* skip SOURCE */
		
		p = gettoken( NULL, ",", &lasts );
		strncpy(hdr.loc_id, p, WFHDR_MAXLEN-1); /* location id */
		
		p = gettoken( NULL, ",", &lasts );
		hdr.lat = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		hdr.lon = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		hdr.tz = atof(p);
		
		p = gettoken( NULL, ",", &lasts );
		hdr.elev = atof(p);
		
		/* skip over excess header lines */
		
		fgets(buf,2047,fp); /* DESIGN CONDITIONS */
		fgets(buf,2047,fp); /* TYPICAL/EXTREME PERIODS */
		fgets(buf,2047,fp); /* GROUND TEMPERATURES */
		fgets(buf,2047,fp); /* HOLIDAY/DAYLIGHT SAVINGS */
		fgets(buf,2047,fp); /* COMMENTS 1 */
		fgets(buf,2047,fp); /* COMMENTS 2 */
		fgets(buf,2047,fp); /* DATA PERIODS */
		
	}
	else
	{
		free( obj );
		fclose( fp );
		return NULL;
	}
	
	if (p_hdr)
		memcpy(p_hdr, &hdr, sizeof(wf_header));
	
	return (wf_obj_t*) obj;	
}

 int  wf_read_data( wf_obj_t wf, wf_data *dat)
{
	char buf[1023];
	char *cols[128], *p;
	wf_object *obj = WF_OBJECT(wf);
	
	if (!obj || !dat || obj->wf_type < 0 || obj->fp == NULL) return -1;
	
	if (obj->wf_type == WF_TMY2)
	{
		char *pret = fgets(buf, 1023, obj->fp);
		
		int yr,mn,dy,hr,ethor,etdn;
		int d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21;      /* which of these are used? d3, d10, d15 & d20 */
		int u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21;  /* are any of these ever used?? */
		int w1,w2,w3,w4,w5,w6,w7,w8,w9,w10;     
		char f1[2],f2[2],f3[2],f4[2],f5[2],f6[2],f7[2],f8[2],f9[2],f10[2],f11[2],f12[2],f13[2],f14[2],f15[2],f16[2],f17[2],f18[2],f19[2],f20[2],f21[2];
	
		int nread = sscanf(buf,
		 "%2d%2d%2d%2d"
		 "%4d%4d"
		 "%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d%4d%1s%1d"
		 "%2d%1s%1d%2d%1s%1d%4d%1s%1d%4d%1s%1d%3d%1s%1d%4d%1s%1d%3d%1s%1d"
		 "%3d%1s%1d%4d%1s%1d%5d%1s%1d%1d%1d%1d%1d%1d%1d%1d%1d%1d%1d%3d%1s%1d%3d%1s%1d%3d%1s%1d%2d%1s%1d\n",
			 &yr,&mn,&dy,&hr,
			 &ethor, /* extraterrestrial horizontal radiation */
			 &etdn, /* extraterrestrial direct normal radiation */
			 &d1,f1,&u1, /* GH data value 0-1415 Wh/m2, Source, Uncertainty */
			 &d2,f2,&u2, /* DN data value 0-1200 Wh/m2, Source, Uncertainty */
			 &d3,f3,&u3, /* DF data value 0-700 Wh/m2, Source, Uncertainty */
			 &d4,f4,&u4, /* GH illum data value, Source, Uncertainty */
			 &d5,f5,&u5, /* DN illum data value, Source, Uncertainty */
			 &d6,f6,&u6, /* DF illum data value, Source, Uncertainty */
			 &d7,f7,&u7, /* Zenith illum data value, Source, Uncertainty */
			 &d8,f8,&u8, /* Total sky cover */
			 &d9,f9,&u9, /* opaque sky cover */
			 &d10,f10,&u10, /* dry bulb temp -500 to 500 = -50.0 to 50.0 'C */
			 &d11,f11,&u11, /* dew point temp -600 to 300 = -60.0 to 30.0 'C */
			 &d12,f12,&u12, /* relative humidity 0-100 */
			 &d13,f13,&u13, /* pressure millibars */
			 &d14,f14,&u14, /* wind direction */
			 &d15,&f15,&u15, // wind speed 0 to 400 = 0.0 to 40.0 m/s
			 &d16,&f16,&u16, // visibility
			 &d17,&f17,&u17, // ceiling height
			 &w1,&w2,&w3,&w4,&w5,&w6,&w7,&w8,&w9,&w10, // present weather
			 &d18,&f18,&u18, // precipitable water
			 &d19,&f19,&u19, // aerosol optical depth
			 &d20,&f20,&u20, // snow depth 0-150 cm
			 &d21,&f21,&u21 ); // days since last snowfall 0-88

		dat->year = yr + 1900;
		dat->month = mn;
		dat->day = dy;
		dat->hour = hr;
		dat->gh = (double)d1*1.0;
		dat->dn=(double)d2;           /* Direct radiation */
		dat->df=(double)d3;           /* Diffuse radiation */
		dat->tdry=(double)d10/10.0;       /* Ambient dry bulb temperature(C) */
		dat->twet = (double)d11/10.0;
		dat->wspd=(double)d15/10.0;       /* Wind speed(m/s) */
		dat->wdir=(double)d14; /* wind dir */
		dat->rhum = (double)d12;
		dat->pres = (double)d13;
		dat->snow = (double)d20;

		return nread==79 && pret==buf;
	}
	else if (obj->wf_type == WF_TMY3)
	{
		char *pret = fgets(buf, 1023, obj->fp);
		int ncols = locate(buf, cols, 128, ',');

		if (ncols < 68)
			return -3;
	
		p = cols[0];

		dat->month = atoi( p );
		p = strchr(p, '/');
		if (!p)
			return -4;
		p++;
		dat->day = atoi( p );
		p = strchr(p, '/');
		if (!p) return -5;
		p++;
		dat->year = atoi( p );

		dat->hour = atoi( cols[1] );

		dat->gh = (double)atof( cols[4] );
		dat->dn = (double)atof( cols[7] );
		dat->df = (double)atof( cols[10] );

		dat->tdry = (double)atof( cols[31] );
		dat->twet = (double)atof( cols[34] );

		dat->wspd = (double)atof( cols[46] );
		dat->wdir = (double)atof( cols[43] );

		dat->rhum = (double)atof( cols[37] );
		dat->pres = (double)atof( cols[40] );
		dat->snow = 999.0; // no snowfall in TMY3
		
		return pret==buf;
	}
	else if (obj->wf_type == WF_EPW)
	{
		char *pret = fgets(buf, 1024, obj->fp);
		int ncols = locate(buf, cols, 128, ',');

		if (ncols < 32)
			return -6;

		dat->year = atoi(cols[0]);
		dat->month = atoi(cols[1]);
		dat->day = atoi(cols[2]);
		dat->hour = atoi(cols[3]);

		dat->gh = (double)atof(cols[13]);
		dat->dn = (double)atof(cols[14]);
		dat->df = (double)atof(cols[15]);
		
		dat->wspd = (double)atof(cols[21]);
		dat->wdir = (double)atof(cols[20]);
		
		dat->tdry = (double)atof(cols[6]);
		dat->twet = (double)atof(cols[7]);
		
		dat->rhum = (double)atof( cols[8] );
		dat->pres = (double)atof( cols[9] ) * 0.01; /* convert Pa in to mbar */
		dat->snow = (double)atof( cols[30] ); // snowfall

		return pret==buf;
	}
	else
		return -1;
}

 void  wf_close(wf_obj_t wf)
{
	wf_object *obj = WF_OBJECT(wf);
	if (obj && obj->fp)
		fclose( obj->fp );
	
	if (obj) free( obj );
}

 void  wf_rewind(wf_obj_t wf)
{
	wf_object *obj = WF_OBJECT(wf);
	if (obj && obj->fp)
	{
		rewind( obj->fp );
		
		// skip header info
		char buf[2048];
		if (obj->wf_type == WF_TMY2)
			fgets(buf,2047,obj->fp); 
		else if(obj->wf_type==WF_TMY3)
			for(int i=0;i<2;i++) fgets(buf,2047,obj->fp);
		else if(obj->wf_type==WF_EPW)
			for(int i=0;i<8;i++) fgets(buf,2047,obj->fp);
	}
}
