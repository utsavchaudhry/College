/**
 @file
 @author Mark Boady <mwb33@drexel.edu>
 @date Orgininal: 2021, Revised: 2024
 @section Description
 
 An abstract Task idea for the thread pool.
 */

#ifndef _TASK_H_
#define _TASK_H_

#include <queue>

/**
 A task is a think the thread pool needs to do.
 */
class task{
public:
    //These are virtual functions
    //They need to be implemented
    //Returns any new tasks to do
    /**
     Run a task. This should be done in a thread.
    */
    virtual std::queue<task*>* runTask() = 0;
    /**
     Destructor to remove a task from memory
    */
    virtual ~task(){}
};
#endif
