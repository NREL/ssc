#include "STSimulateThread.h"
#include "definitions.h"

#ifdef SP_USE_SOLTRACE
using namespace std;

int STSimThread::GetResultCode(){ return ResultCode; }

st_context_t STSimThread::GetContextId() { return ContextId; }

void STSimThread::Setup( st_context_t spcxt, int thd_num, int seed )
{
	ThreadNum = thd_num;
	CancelFlag = false;
	Finished = false;
	SeedVal = seed;
	ContextId = spcxt;
	ResultCode = -1;
	NToTrace = 0;
	NTraced = 0; 
	NTraceTotal = 0;
	CurStage = 0;
	NStages = 0;
}

STSimThread::~STSimThread()
{
	::st_free_context( ContextId );
}	


void STSimThread::CancelTrace()
{
	CancelLock.lock();
	CancelFlag = true;
	CancelLock.unlock();
}

bool STSimThread::IsTraceCancelled()
{
	bool r;
	CancelLock.lock();
	r = CancelFlag;
	CancelLock.unlock();
	return r;
}

bool STSimThread::IsFinished()
{
	bool f;
	FinishedLock.lock();
	f = Finished;
	FinishedLock.unlock();
	return f;
}

void STSimThread::UpdateStatus(int ntracedtotal, int ntraced, int ntotrace, int curstage, int nstages)
{
	StatusLock.lock();
	this->NTraceTotal = ntracedtotal;
	this->NTraced = ntraced;
	this->NToTrace = ntotrace;
	this->CurStage = curstage;
	this->NStages = nstages;
	StatusLock.unlock();
}

void STSimThread::GetStatus(int *total, int *traced, int *ntotrace, int *stage, int *nstages)
{
	StatusLock.lock();
	*total = this->NTraceTotal;
	*traced = this->NTraced;
	*ntotrace = this->NToTrace;
	*stage = this->CurStage;
	*nstages = this->NStages;
	StatusLock.unlock();
}

//void *STSimThread::Entry()
//{
//	ResultCode = st_sim_run( ContextId, (unsigned int)SeedVal, STCallback_MT, (void*) this );
//	FinishedLock.lock();
//	Finished = true;
//	FinishedLock.unlock();
//	return NULL;
//
//};

void STSimThread::StartThread()
{
	ResultCode = st_sim_run( ContextId, (unsigned int)SeedVal, STCallback_MT, (void*) this );
	FinishedLock.lock();
	Finished = true;
	FinishedLock.unlock();
	//	return NULL;

};	

#endif