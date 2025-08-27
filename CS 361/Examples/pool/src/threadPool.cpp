/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
Implementation of a thread pool.
 
 Inspired by:
 https://ncona.com/2019/05/using-thread-pools-in-cpp/
 */

#include "threadPool.h"
#include <iostream>

//Constructor
//Determine number of threads for this
//system
threadPool::threadPool(){
	numberOfThreads =
		std::thread::hardware_concurrency();
	//Zero means we aren't on a threaded
	//System, so we make 1 thread in the pool
    if (numberOfThreads == 0) {
      numberOfThreads = 1;
    }
    //Allocate Space for Threads
    myThreads = new std::thread*[numberOfThreads];
    //Allocate Status Array
    status = new bool[numberOfThreads];
    //Queue to store tasks
    toDO = new std::queue<task*>();
}

//Close Down the Pool
threadPool::~threadPool(){
	delete myThreads;
	delete status;
	delete toDO;
}

//Add A Task to the work list
void threadPool::addWork(task* t){
	std::lock_guard<std::mutex> lk(queueLock);
	toDO->push(t);
}

//Start up all threads
void threadPool::start(){
	//Nothing to do!
	if(toDO->empty()){return;}
	//Start up all the threads
	for(int i=0; i < numberOfThreads; i++){
		//This Thread is alive
		status[i] = true;
		//Start the thread
		myThreads[i] = new std::thread(
			&threadPool::workerThread,
			this,
			i
			);
	}
	//Join all
	for(int i=0; i < numberOfThreads; i++){
		myThreads[i]->join();
	}
}

//Shut Down all threads
void threadPool::stop(){
	done = true;
	//Wake Everyone Up
	needsWork.notify_all();
}

//Get a task, resolve it
//update the task list
//wait for more tasks
void threadPool::workerThread(int statusID){
	//Pointer to hold a task
	task* t;
	//Break this loop when done
	while(true){
		{//Start Lock Scope
		std::unique_lock<std::mutex> lk(queueLock);
		//Status update, we are waiting
		status[statusID]=false;
		//Wait for work
		needsWork.wait(lk, [&]{
          // Elements in queue
          //or job done
          return !toDO->empty() || done;
        });
        //Exit if we got woken on done
        if(done){break;}
        //Get a value off the queue
        t = toDO->front();
        toDO->pop();
        status[statusID]=true;
        }//End Lock Scope
		//Do the task
        std::queue<task*>* more = t->runTask();
        while(!more->empty()){
			addWork(more->front());
			more->pop();
        }
        //Decide if we are done?
        {//Lock Scope
			std::unique_lock<std::mutex> lk(queueLock);
			//No work left
			if(toDO->empty())
			{
				//I am not working
				status[statusID]=false;
				//Is anyone working?
				bool working=false;
				for(int i=0; i < numberOfThreads; i++)
				{
					working = working || status[i];
				}
				if(!working){stop();}
			}
        }//End of Scope Lock
	}
	
}
