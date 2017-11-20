#ifndef _CODE_GENERATOR_INPUTS_H_
#define _CODE_GENERATOR_INPUTS_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sscapi.h"

ssc_bool_t my_handler(ssc_module_t p_mod, ssc_handler_t p_handler, int action,
	float f0, float f1, const char *s0, const char *s1, void *user_data)
{
	if (action == SSC_LOG)
	{
		// print log message to console
		switch ((int)f0)
		{
		case SSC_NOTICE: printf("Notice: %s", s0); break;
		case SSC_WARNING: printf("Warning: %s", s0); break;
		case SSC_ERROR: printf("Error: %s", s0); break;
		}
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		printf("(%.2f %%) %s", f0, s0);
		return 1; // return 0 to abort simulation as needed.
	}
	else
		return 0;
}

int set_array(ssc_data_t p_data, const char *name, const char* fn, int len)
{
	char buffer[1024];
	char *record, *line;
	int i = 0;
	ssc_number_t *ary;
	FILE *fp = fopen(fn, "r");
	if (fp == NULL)
	{
		printf("file opening failed ");
		return 0;
	}
	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));
	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)
	{
		record = strtok(line, ",");
		while ((record != NULL) && (i < len))
		{
			ary[i] = (ssc_number_t)atof(record);
			record = strtok(NULL, ",");
			i++;
		}
	}
	fclose(fp);
	ssc_data_set_array(p_data, name, ary, len);
	free(ary);
	return 1;
}

int set_matrix(ssc_data_t p_data, const char *name, const char* fn, int nr, int nc)
{
	char buffer[1024];
	char *record, *line;
	ssc_number_t *ary;
	int i = 0, len = nr*nc;
	FILE *fp = fopen(fn, "r");
	if (fp == NULL)
	{
		printf("file opening failed ");
		return 0;
	}
	ary = (ssc_number_t *)malloc(len * sizeof(ssc_number_t));
	while ((line = fgets(buffer, sizeof(buffer), fp)) != NULL)
	{
		record = strtok(line, ",");
		while ((record != NULL) && (i < len))
		{
			ary[i] = (ssc_number_t)atof(record);
			record = strtok(NULL, ",");
			i++;
		}
	}
	fclose(fp);
	ssc_data_set_matrix(p_data, name, ary, nr, nc);
	free(ary);
	return 1;
}

#endif