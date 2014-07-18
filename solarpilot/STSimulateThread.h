#ifndef _ST_SIMTHREAD_
#define _ST_SIMTHREAD_ 1
#include "definitions.h"

#ifdef _USE_SOLTRACE

//#include <wx/wx.h>
#include <thread>
#include <mutex>
#include "stapi.h"
using namespace std;
typedef void* st_context_t;

class STSimThread 
{
	
	bool
		Finished,
		CancelFlag;
	int NToTrace, 
		NTraced, 
		NTraceTotal, 
		ResultCode, 
		SeedVal, 
		CurStage, 
		NStages, 
		ThreadNum;
	
	st_context_t ContextId;
	mutex
	//wxMutex
		StatusLock,
		CancelLock,
		FinishedLock;

public:

	STSimThread(){};

	void Setup( st_context_t spcxt, int thd_num, int seed );
	
	~STSimThread();

	void CancelTrace();

	bool IsTraceCancelled();

	bool IsFinished();

	void UpdateStatus(int ntracedtotal, int ntraced, int ntotrace, int curstage, int nstages);

	void GetStatus(int *total, int *traced, int *ntotrace, int *stage, int *nstages);
	
	int GetResultCode();

	st_context_t GetContextId();

	void StartThread();
//private:
//	
//	void *Entry();

};

//Multithreaded callback 
static int STCallback_MT(st_uint_t ntracedtotal, st_uint_t ntraced, st_uint_t ntotrace, st_uint_t curstage, st_uint_t nstages, void *data)
{
	//STSimThread *t = (STSimThread*)data;
	STSimThread *t = static_cast<STSimThread*>(data);
	t->UpdateStatus(ntracedtotal, ntraced, ntotrace, curstage, nstages);
	return t->IsTraceCancelled()? 0 : 1;
};


#endif

#endif